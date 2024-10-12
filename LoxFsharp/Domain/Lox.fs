namespace rec LoxFsharp

type LoxInterpreter =
    abstract execute: Stmt * LoxEnvironment -> unit
    abstract resolve: Expr * int -> unit

type LoxEnvironment =
    abstract define: Token * obj -> unit
    abstract assign: Token * obj -> unit
    abstract assignAt: int * Token * obj -> unit
    abstract get: Token -> obj
    abstract getAt: int * Token -> obj

type LoxStaticAnalyser =
    abstract analyse: Program -> unit

type LoxCallable =
    abstract arity: int
    abstract call: LoxInterpreter * ResizeArray<obj> -> obj
