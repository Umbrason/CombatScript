using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;

public class CombatScriptInterpreter
{
    private readonly string source;
    private readonly CombatScriptParser parser;
    private readonly ICombatScriptStdLib stdLib;
    const BindingFlags BINDINGS = BindingFlags.Public | BindingFlags.Instance;

    private class Scope
    {
        private readonly Dictionary<string, object> variables = new();
        readonly Scope parentScope;
        readonly string name;
        readonly List<Scope> childScopes = new();
        public bool isReadonly = false;

        public Scope() { }
        public Scope(Scope parentScope, string name = "", bool isReadonly = false)
        {
            this.parentScope = parentScope;
            this.isReadonly = isReadonly;
            this.name = parentScope == null ? name : $"{parentScope.name}.{name}";
        }

        public object this[string var]
        {
            get => variables.ContainsKey(var) ? variables[var] : parentScope != null ? parentScope[var] : null;
            set { var _ = parentScope?.HasVar(var) ?? false ? parentScope[var] = value : variables[var] = isReadonly ? variables[var] : value; }
        }
        public bool HasVar(string var) => variables.ContainsKey(var) || (parentScope?.HasVar(var) ?? false);
        public Scope SubScope(string name = "", bool isReadonly = false)
        {
            var subScope = new Scope(this, name, isReadonly);
            childScopes.Add(subScope);
            return subScope;
        }

        public void Clear()
        {
            variables.Clear();
            for (int i = 0; i < childScopes.Count; i++)
            {
                Scope child = childScopes[i];
                child.Clear();
            }
        }
    }

    public CombatScriptInterpreter(string source, CombatScriptParser parser, ICombatScriptStdLib stdLib)
    {
        this.source = source;
        this.parser = parser;
        this.stdLib = stdLib;
    }

    public static CombatScriptInterpreter Compile(string source, ICombatScriptStdLib stdLib)
    {
        var antlerStream = new AntlrInputStream(source);
        var lexer = new CombatScriptLexer(antlerStream);
        var tokenStream = new CommonTokenStream(lexer);
        var parser = new CombatScriptParser(tokenStream);
        return new CombatScriptInterpreter(source, parser, stdLib);
    }


    //TODO: prevent exploitation by instantiating nasty stuff like URL/threads/tasks shit.
    // also prevent "jailbreaking" by chaining a lot of member accesses/function calls
    public IEnumerator Execute()
    {
        var ctx = parser.program();
        var rootScope = new Scope(null, "root");
        rootScope["game"] = stdLib;
        rootScope.isReadonly = true;
        rootScope = rootScope.SubScope("user", false);
        if (ctx.scope()?.Length > 0)
        {
            var subScopeCtxs = ctx.scope();
            for (int i = 0; i < subScopeCtxs.Length; i++)
            {
                var subscopeIEnumerator = HandleScope(rootScope, subScopeCtxs[i]);
                do
                    if (subscopeIEnumerator != null)
                        if ((subscopeIEnumerator.Current is ITerminalNode node) && node.Symbol.Type == CombatScriptLexer.RETURN)
                            yield break;
                        else yield return subscopeIEnumerator.Current;
                while (subscopeIEnumerator.MoveNext());
            }
        }
    }

    private IEnumerator HandleScope(Scope scope, CombatScriptParser.ScopeContext ctx)
    {
        if (ctx.scope()?.Length > 0)
        {
            var subScopeCtxs = ctx.scope();
            for (int i = 0; i < subScopeCtxs.Length; i++)
            {
                var subScope = scope.SubScope("curly");
                var subscopeIEnumerator = HandleScope(subScope, subScopeCtxs[i]);
                do if (subscopeIEnumerator != null) yield return subscopeIEnumerator.Current;
                while (subscopeIEnumerator.MoveNext());
            }
        }
        else
        {
            var enumerator = (IEnumerator)null;
            if (ctx.for_loop() != null) enumerator = HandleForLoop(scope, ctx.for_loop());
            if (ctx.foreach_loop() != null) enumerator = HandleForeachLoop(scope, ctx.foreach_loop());
            if (ctx.while_loop() != null) enumerator = HandleWhileLoop(scope, ctx.while_loop());
            if (ctx.conditional() != null) enumerator = HandleConditional(scope, ctx.conditional());
            if (ctx.@switch() != null) enumerator = HandleSwitch(scope, ctx.@switch());
            if (ctx.statement() != null) enumerator = HandleStatement(scope, ctx.statement());
            do if (enumerator != null) yield return enumerator.Current;
            while (enumerator.MoveNext());
        }
    }

    private IEnumerator HandleStatement(Scope scope, CombatScriptParser.StatementContext ctx)
    {
        if (ctx.variable_definition() != null) HandleVariableDefinition(scope, ctx.variable_definition());
        if (ctx.variable_assignment() != null) HandleVariableAssignment(scope, ctx.variable_assignment());
        if (ctx.variable_increment() != null) HandleVariableIncrement(scope, ctx.variable_increment());
        if (ctx.RETURN() != null)
        {
            yield return ctx.RETURN();
            if (ctx.expression() == null)
                yield return null;
        }
        if (ctx.BREAK() != null) yield return ctx.BREAK();
        if (ctx.CONTINUE() != null) yield return ctx.CONTINUE();
        if (ctx.expression() != null) yield return EvaluateExpression(scope, ctx.expression());
    }

    private void HandleVariableDefinition(Scope scope, CombatScriptParser.Variable_definitionContext ctx)
    {
        var ID = ctx.ID().Symbol.Text;
        if (scope.HasVar(ID)) throw new Exception($"variable {ID} already exists!");
        if (ctx.expression() != null)
        {
            var value = EvaluateExpression(scope, ctx.expression());
            scope[ID] = value;
        }
    }
    private (Func<object> getter, Action<object> setter) VariableAccessToGetterSetter(Scope scope, CombatScriptParser.Variable_accessContext ctx)
    {
        if (ctx.ID() != null)
        {
            var ID = ctx.ID().Symbol.Text;
            if (!scope.HasVar(ID)) throw new Exception($"the name '{ID}' does not exist in the current context");
            return (() => scope[ID], (value) => scope[ID] = value);
        }

        var target = EvaluateExpression(scope, ctx.expression());
        var targetType = target.GetType();

        if (ctx.member_access() != null)
        {
            var memberID = ctx.member_access().ID().Symbol.Text;
            var fieldInfo = targetType.GetField(memberID, BINDINGS);
            var propertyInfo = targetType.GetProperty(memberID, BINDINGS);
            if (fieldInfo != null) return (() => fieldInfo.GetValue(target), (value) => fieldInfo.SetValue(target, value));
            if (propertyInfo != null)
            {
                var indexParams = propertyInfo.GetIndexParameters();
                if (indexParams.Length > 0) throw new Exception($"Missing index paramters: [{string.Join(" ,", indexParams.Select(param => param.ParameterType.Name))}]");
                return (() => propertyInfo.GetValue(target), (value) => propertyInfo.SetValue(target, value));
            }
            throw new Exception($"No public member '{targetType.Name}.{memberID}' found!");
        }
        else
        {
            var arrayAccess = ctx.array_access();
            var accessParamExpressions = arrayAccess.expression();
            var accessParamValues = accessParamExpressions.Select(expression => EvaluateExpression(scope, expression)).ToArray();
            var accessParamTypes = accessParamValues.Select(p => p?.GetType() ?? null).ToArray();
            foreach (var property in targetType.GetProperties(BINDINGS))
            {
                if (!ParametersMatch(property.GetIndexParameters(), accessParamTypes)) continue;
                return (() => property.GetValue(target, accessParamValues), (value) => property.SetValue(target, value, accessParamValues));
            }
            throw new Exception($"{targetType.Name} has no indexer [{string.Join(", ", accessParamTypes.Select(type => type?.Name ?? "null"))}]");
        }
    }

    private void HandleVariableAssignment(Scope scope, CombatScriptParser.Variable_assignmentContext ctx)
    {
        var (getter, setter) = VariableAccessToGetterSetter(scope, ctx.variable_access());
        var value = EvaluateExpression(scope, ctx.expression());
        if (ctx.arithmetic_op() != null) value = HandleArithmeticOp(getter(), value, ctx.arithmetic_op().children[0] as ITerminalNode);
        if (ctx.bitwise_op() != null) value = HandleBitwiseOp(getter(), value, ctx.bitwise_op().children[0] as ITerminalNode);
        setter(value);
    }

    private object HandleArithmeticOp(object a, object b, ITerminalNode opCode)
    {
        if (!(a is int or float && b is int or float)) throw new Exception($"cannot apply operator {opCode.Symbol.Text} to {a.GetType().Name} and {b.GetType().Name}");
        var bothInt = a is int && b is int;
        return opCode.Symbol.Type switch
        {
            CombatScriptLexer.ADD => bothInt ? (int)a + (int)b : Convert.ToSingle(a) + Convert.ToSingle(b),
            CombatScriptLexer.SUB => bothInt ? (int)a - (int)b : Convert.ToSingle(a) - Convert.ToSingle(b),
            CombatScriptLexer.MUL => bothInt ? (int)a * (int)b : Convert.ToSingle(a) * Convert.ToSingle(b),
            CombatScriptLexer.DIV => bothInt ? (int)a / (int)b : Convert.ToSingle(a) / Convert.ToSingle(b),
            CombatScriptLexer.MOD => bothInt ? (int)a % (int)b : Convert.ToSingle(a) % Convert.ToSingle(b),
            CombatScriptLexer.POW => bothInt ? (int)Math.Pow((int)a, (int)b) : (float)Math.Pow(Convert.ToSingle(a), Convert.ToSingle(b)),
            _ => null,
        };
    }

    private object HandleCompareOp(object a, object b, ITerminalNode opCode)
    {
        if (a == null && b == null) return true;
        if (a == null || b == null) return false;

        var compareValue = 0f;
        if (opCode.Symbol.Type != CombatScriptLexer.EQ && opCode.Symbol.Type != CombatScriptLexer.NEQ)
        {
            if (a is int ia && b is int ib) compareValue = ia - ib;
            else if (a is int or float && b is int or float) compareValue = Convert.ToSingle(a) - Convert.ToSingle(b);
            else compareValue = Comparer.Default.Compare(a, b);
        }
        return opCode.Symbol.Type switch
        {
            CombatScriptLexer.EQ => a.Equals(b),
            CombatScriptLexer.NEQ => !a.Equals(b),
            CombatScriptLexer.LT => compareValue < 0,
            CombatScriptLexer.LE => compareValue <= 0,
            CombatScriptLexer.GT => compareValue > 0,
            CombatScriptLexer.GE => compareValue >= 0,
            _ => null,
        };
    }

    private object HandleLogicalOp(object a, object b, ITerminalNode opCode)
    {
        if (!(a is bool ab && b is bool bb)) throw new Exception($"cannot apply operator {opCode.Symbol.Text} to {a.GetType().Name} and {b.GetType().Name}");
        return opCode.Symbol.Type switch
        {
            CombatScriptLexer.NEG => !ab,
            CombatScriptLexer.AND => ab && bb,
            CombatScriptLexer.OR => ab || bb,
            _ => null,
        };
    }

    private object HandleBitwiseOp(object a, object b, ITerminalNode opCode)
    {
        if (!(a is int ai && b is int bi)) throw new Exception($"cannot apply operator {opCode.Symbol.Text} to {a.GetType().Name} and {b.GetType().Name}");
        return opCode.Symbol.Type switch
        {
            CombatScriptLexer.BWAND => ai & bi,
            CombatScriptLexer.BWOR => ai | bi,
            CombatScriptLexer.BWXOR => ai ^ bi,
            CombatScriptLexer.BWLS => ai << bi,
            CombatScriptLexer.BWRS => ai >> bi,
            _ => null,
        };
    }

    private void HandleVariableIncrement(Scope scope, CombatScriptParser.Variable_incrementContext ctx)
    {
        var variable = VariableAccessToGetterSetter(scope, ctx.variable_access());
        var value = variable.getter();
        if (!(value is int or float)) throw new InvalidOperationException($"Cannot apply {(ctx.INCR() != null ? "++" : "--")} operator on value of type {value.GetType()}");
        if (ctx.INCR() != null)
        {
            value = value is int i ? ++i : value;
            value = value is float f ? ++f : value;
        }
        else
        {
            value = value is int i ? --i : value;
            value = value is float f ? --f : value;
        }
        variable.setter(value);
    }

    private IEnumerator HandleSwitch(Scope scope, CombatScriptParser.SwitchContext ctx)
    {
        var expression = ctx.expression();
        var expressionValue = EvaluateExpression(scope, expression);
        var branches = ctx.switchBranch();
        for (int i = 0; i < branches.Length; i++)
        {
            var branch = branches[i];
            var cases = branch.@case();
            var matchesCase = cases.Any(@case => @case.DEFAULT() != null || expressionValue.Equals(EvaluateExpression(scope, @case.expression())));
            if (!matchesCase) continue;
            var subscope = scope.SubScope("switch");
            var scopeIEnumerator = HandleScope(subscope, branch.scope());
            do if (scopeIEnumerator.Current != null) yield return scopeIEnumerator.Current;
            while (scopeIEnumerator.MoveNext());
            break;
        }
    }

    private IEnumerator HandleConditional(Scope scope, CombatScriptParser.ConditionalContext ctx)
    {
        var scopes = ctx.scope();
        var conditions = ctx.expression();

        for (int i = 0; i < scopes.Length; i++)
        {
            if (i <= conditions.Length && !(bool)EvaluateExpression(scope, conditions[i])) continue;
            var subscope = scope.SubScope("conditional");
            var scopeIEnumerator = HandleScope(subscope, scopes[i]);
            do if (scopeIEnumerator.Current != null) yield return scopeIEnumerator.Current;
            while (scopeIEnumerator.MoveNext());
        }
    }

    private IEnumerator HandleForeachLoop(Scope scope, CombatScriptParser.Foreach_loopContext ctx)
    {
        var subscope = scope.SubScope("foreach");
        var collection = ctx.expression();
        var body = ctx.scope();
        var ID = ctx.ID();
        var enumerator = (IEnumerable)EvaluateExpression(scope, collection);
        foreach (var value in enumerator)
        {
            var @break = false;
            subscope[ID.Symbol.Text] = value;
            var bodyIEnumerable = HandleScope(subscope, body);
            do
            {
                var current = bodyIEnumerable.Current;
                if (current is int keyword)
                {
                    if (keyword == CombatScriptLexer.BREAK) @break = true;
                    if (keyword == CombatScriptLexer.CONTINUE || keyword == CombatScriptLexer.BREAK)
                        break;
                }
                if (bodyIEnumerable.Current != null) yield return bodyIEnumerable.Current;
            }
            while (bodyIEnumerable.MoveNext());
            if (@break) break;
        }
    }

    private IEnumerator HandleWhileLoop(Scope scope, CombatScriptParser.While_loopContext ctx)
    {
        var subscope = scope.SubScope("while");
        var condition = ctx.expression();
        var body = ctx.scope();
        var loopIEnumerator = HandleGenericLoop(() => (bool)EvaluateExpression(subscope, condition), () => HandleScope(subscope, body));
        do if (loopIEnumerator.Current != null) yield return loopIEnumerator.Current;
        while (loopIEnumerator.MoveNext());
    }

    private IEnumerator HandleForLoop(Scope scope, CombatScriptParser.For_loopContext ctx)
    {
        var subscope = scope.SubScope("for");

        var variableDeclaration = ctx.statement()[0];
        var variableDeclarationIEnumerator = HandleStatement(subscope, variableDeclaration);
        do if (variableDeclarationIEnumerator.Current != null) yield return variableDeclarationIEnumerator.Current;
        while (variableDeclarationIEnumerator.MoveNext());

        var condition = ctx.expression();
        var incrementer = ctx.statement()[1];
        var body = ctx.scope();

        var loopIEnumerator = HandleGenericLoop(() => (bool)EvaluateExpression(subscope, condition), () => HandleScope(subscope, body), () => HandleStatement(subscope, incrementer));
        do if (loopIEnumerator.Current != null) yield return loopIEnumerator.Current;
        while (loopIEnumerator.MoveNext());
    }

    private IEnumerator HandleGenericLoop(Func<bool> condition, params Func<IEnumerator>[] body)
    {
        while (condition.Invoke())
        {
            var @break = false;
            for (int i = 0; i < body.Length; i++)
            {
                var IEnumerator = body[i].Invoke();
                do
                {
                    var current = IEnumerator.Current;
                    if (current is ITerminalNode node)
                    {
                        var tokenType = node.Symbol.Type;
                        if (tokenType == CombatScriptLexer.BREAK) @break = true;
                        if (tokenType == CombatScriptLexer.CONTINUE || tokenType == CombatScriptLexer.BREAK)
                            break;
                    }
                    if (IEnumerator.Current != null) yield return IEnumerator.Current;
                }
                while (IEnumerator.MoveNext());
                if (@break) break;
            }
            if (@break) break;
        }
    }

    private Func<object[], object> GetStdLibFunc(string funcID)
    {
        var stdLibType = stdLib.GetType();
        var fieldInfo = stdLibType.GetField((string)funcID, BINDINGS);
        var propertyInfo = stdLibType.GetProperty((string)funcID, BINDINGS);
        var functionInfos = stdLibType.GetMethods(BINDINGS).Where(info => info.Name == funcID).ToArray();
        if (functionInfos.Length > 0)
        {
            return (object[] parameters) =>
            {
                var parameterTypes = parameters.Select(p => p?.GetType()).ToArray();
                foreach (var functionInfo in functionInfos)
                {
                    var parameterInfo = functionInfo.GetParameters();
                    if (!ParametersMatch(parameterInfo, parameterTypes)) continue;
                    return functionInfo.Invoke(stdLib, parameters);
                }
                throw new Exception($"Invalid parameters: {functionInfos[0].Name}({string.Join(" ,", parameterTypes.Select(type => type?.Name ?? "null"))})");
            };
        }
        throw new Exception($"{stdLibType.Name} has no member {funcID}");
    }

    private object GetPrimitive(Scope scope, CombatScriptParser.PrimitiveContext ctx)
    {
        if (ctx.ID() != null)
        {
            var text = ctx.ID().Symbol.Text;
            if (scope.HasVar(text))
                return scope[text];
        }
        if (ctx.FLOAT() != null) return float.Parse(ctx.FLOAT().Symbol.Text);
        if (ctx.INT() != null) return int.Parse(ctx.INT().Symbol.Text);
        if (ctx.BOOL() != null) return bool.Parse(ctx.BOOL().Symbol.Text);
        if (ctx.STRING() != null) return ctx.STRING().Symbol.Text[1..^1];
        if (ctx.NULL() != null) return null;
        if (ctx.range() != null)
        {
            var rangeParams = ctx.range().INT();
            var lower = int.Parse(rangeParams[0].Symbol.Text);
            var upper = int.Parse(rangeParams[1].Symbol.Text);
            return Enumerable.Range(lower, upper - lower).ToArray();
        }
        if (ctx.array() != null) return ctx.array().expression().Select(expression => EvaluateExpression(scope, expression)).ToArray();
        if (ctx.lambda() != null)
        {
            var lambdaCtx = ctx.lambda();
            var lambdaParameters = lambdaCtx.ID().Select(id => id.Symbol.Text).ToArray();
            var lambdaScopeCtxs = lambdaCtx.scope();
            var paramCount = lambdaParameters.Length;
            return
            (Func<object[], object>)((object[] parameters) =>
            {
                if (parameters.Length != paramCount)
                    throw new Exception($"Encountered invalid parameter count for Lambda. Expected {paramCount} parameters ({string.Join(", ", lambdaParameters)}) but found {parameters.Length}");
                var lambdaScope = scope.SubScope("lambda");
                for (int i = 0; i < lambdaParameters.Length; i++)
                    lambdaScope[lambdaParameters[i]] = parameters[i];
                if (lambdaCtx.expression() != null) return EvaluateExpression(lambdaScope, lambdaCtx.expression());
                foreach (var scopeCtx in lambdaScopeCtxs)
                {
                    var subscopeIEnumerator = HandleScope(lambdaScope, scopeCtx);
                    //TODO: move this up the callchain through lambda somehow...
                    // consequences:
                    // lambdas are enumerators -> every expression could be an enumerator.
                    //                            every EvaluateExpression() call needs to be an enumerator too
                    //compromise for now: just ignore wait() etc. in lambdas and skip to the result                    

                    //do if (subscopeIEnumerator != null) yield return subscopeIEnumerator.Current;
                    while (subscopeIEnumerator.MoveNext())
                        if ((subscopeIEnumerator.Current is ITerminalNode node) && node.Symbol.Type == CombatScriptLexer.RETURN)
                        {
                            subscopeIEnumerator.MoveNext();
                            return subscopeIEnumerator.Current;
                        }
                }
                return null;
            });
        }
        if (ctx.ctor() != null)
        {
            return HandleConstructor(scope, ctx.ctor());
        }
        return null;
    }

    private object HandleConstructor(Scope scope, CombatScriptParser.CtorContext ctx)
    {
        var typeID = ctx.ID().Symbol.Text;
        var type = Type.GetType(typeID);
        var args = ctx.expression().Select(exp => EvaluateExpression(scope, exp)).ToArray();
        var instance = Activator.CreateInstance(type, args);
        return instance;
    }

    private object GetValue(Scope scope, CombatScriptParser.ValueContext ctx)
    {
        var value = GetPrimitive(scope, ctx.primitive());
        foreach (var child in ctx.children.Skip(1))
        {
            var currentType = value.GetType();
            switch (child.GetType().Name)
            {
                case nameof(CombatScriptParser.Member_accessContext):
                    //field or function or property
                    var memberAccessContext = child as CombatScriptParser.Member_accessContext;
                    var memberID = memberAccessContext.ID().Symbol.Text;
                    var fieldInfo = currentType.GetField(memberID, BINDINGS);
                    var propertyInfo = currentType.GetProperty(memberID, BINDINGS);
                    var functionInfos = currentType.GetMethods(BINDINGS).Where(info => info.Name == memberID).ToArray();
                    if (fieldInfo != null)
                    {
                        value = fieldInfo.GetValue(value);
                        continue;
                    }
                    if (propertyInfo != null)
                    {
                        var indexParams = propertyInfo.GetIndexParameters();
                        if (indexParams.Length > 0) throw new Exception($"Missing index paramters: [{string.Join(" ,", indexParams.Select(param => param.ParameterType.Name))}]");
                        value = propertyInfo.GetValue(value);
                        continue;
                    }
                    if (functionInfos.Length > 0)
                    {
                        var currentValue = value;
                        value = (Func<object[], object>)((object[] parameters) =>
                        {
                            var parameterTypes = parameters.Select(p => p?.GetType()).ToArray();
                            foreach (var functionInfo in functionInfos)
                            {
                                var parameterInfo = functionInfo.GetParameters();
                                if (!ParametersMatch(parameterInfo, parameterTypes)) continue;
                                return functionInfo.Invoke(currentValue, parameters);
                            }
                            throw new Exception($"Invalid parameters: {functionInfos[0].Name}({string.Join(" ,", parameterTypes.Select(type => type?.Name ?? "null"))})");
                        });
                        continue;
                    }
                    throw new Exception($"{currentType.Name} has no member {memberID}");
                case nameof(CombatScriptParser.Function_callContext):
                    var functionCallContext = child as CombatScriptParser.Function_callContext;
                    var functionParameters = functionCallContext.expression();
                    var functionParameterValues = functionParameters.Select(p => EvaluateExpression(scope, p)).ToArray();
                    value = (value as Func<object[], object>).Invoke(functionParameterValues);
                    break;
                case nameof(CombatScriptParser.Array_accessContext):
                    var arrayAccessContext = child as CombatScriptParser.Array_accessContext;
                    var indexParameters = arrayAccessContext.expression();
                    var indexParameterValues = indexParameters.Select(param => EvaluateExpression(scope, param)).ToArray();
                    var indexParameterTypes = indexParameterValues.Select(p => p?.GetType()).ToArray();
                    bool anyMatched = false;
                    if (currentType.IsArray)
                    {
                        value = ((Array)value).GetValue(indexParameterValues.Select(o => (int)o).ToArray());
                        break;
                    }
                    foreach (var property in currentType.GetProperties(BINDINGS))
                    {
                        if (!ParametersMatch(property.GetIndexParameters(), indexParameterTypes)) continue;
                        value = property.GetValue(value, indexParameterValues);
                        anyMatched = true;
                        break;
                    }
                    if (!anyMatched) throw new Exception($"{currentType.Name} has no indexer [{string.Join(", ", indexParameterTypes.Select(type => type?.Name ?? "null"))}]");
                    break;
            }
        }
        return value;
    }

    private bool ParametersMatch(ParameterInfo[] required, Type[] given)
    {
        if (given.Length > required.Length) return false;
        for (int i = 0; i < required.Length; i++)
        {
            var paramInfo = required[i];
            var isNullable = !paramInfo.ParameterType.IsValueType || Nullable.GetUnderlyingType(paramInfo.ParameterType) != null;
            if (paramInfo.IsOptional && given.Length <= i) continue;
            if (given.Length > i && given[i] == null && isNullable) continue;
            if (given.Length > i && paramInfo.ParameterType.IsAssignableFrom(given[i])) continue;
            return false;
        }
        return true;
    }

    private object EvaluateExpression(Scope scope, CombatScriptParser.ExpressionContext expression)
    {
        var expressionValue = (object)null;
        while (expression.LP() != null) expression = expression.expression()[0];
        if (expression.value() != null) return GetValue(scope, expression.value());

        var opperands = expression.expression();
        if (opperands.Length == 1)
        {
            var opperandValue = EvaluateExpression(scope, opperands[0]);
            var opCode = expression.children[0] as ITerminalNode;
            if (expression.SUB() != null) return HandleArithmeticOp(0, opperandValue, opCode);
            if (expression.BWNEG() != null) return HandleArithmeticOp(0, opperandValue, opCode);
            if (expression.NEG() != null) return HandleLogicalOp(opperandValue, opperandValue, opCode);
        }
        else if (opperands.Length == 2)
        {
            var opCode = expression.children[1] as ITerminalNode;
            var a = EvaluateExpression(scope, opperands[0]);
            var b = EvaluateExpression(scope, opperands[1]);
            switch (opCode.Symbol.Type)
            {
                case CombatScriptLexer.ADD:
                case CombatScriptLexer.SUB:
                case CombatScriptLexer.MUL:
                case CombatScriptLexer.DIV:
                case CombatScriptLexer.MOD:
                case CombatScriptLexer.POW:
                    return HandleArithmeticOp(a, b, opCode);
                case CombatScriptLexer.EQ:
                case CombatScriptLexer.NEQ:
                case CombatScriptLexer.LT:
                case CombatScriptLexer.LE:
                case CombatScriptLexer.GT:
                case CombatScriptLexer.GE:
                    return HandleCompareOp(a, b, opCode);
                case CombatScriptLexer.AND:
                case CombatScriptLexer.OR:
                    return HandleLogicalOp(a, b, opCode);
                case CombatScriptLexer.BWAND:
                case CombatScriptLexer.BWOR:
                case CombatScriptLexer.BWXOR:
                case CombatScriptLexer.BWLS:
                case CombatScriptLexer.BWRS:
                    return HandleBitwiseOp(a, b, opCode);
            }
        }
        return expressionValue;
    }
}