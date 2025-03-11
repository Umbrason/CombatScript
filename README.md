# CombatScript (WIP)
CombatScript is a custom language + C# interpreter.\

CombatScript is still WIP and I would not recommend using this in a project yet.\
However feel free to fork and finish this package or make a PR to implement the missing functionality.\

CombatScript is supposed to be used by players to create custom behaviour for a game, like a custom item or card effect.\

You can define which functions should be part of the standard library of CombatScript in C# by providing the "compiler" with a custom standardLib object.\
CombatScript works via reflections so it is probably possible to do some nasty stuff with malicious intentions.\

In the future I'd like to add an easily configureable whitelist for the functions CombatScript is safely allowed to execute.
