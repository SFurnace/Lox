namespace LoxFsharp

type LoxEnvironment =
    abstract define: Token * obj -> unit
    abstract assign: Token * obj -> unit
    abstract get: Token -> obj

type LoxInterpreter =
    abstract execute: Stmt * LoxEnvironment -> unit

type LoxCallable =
    abstract arity: int
    abstract call: LoxInterpreter * ResizeArray<obj> -> obj
