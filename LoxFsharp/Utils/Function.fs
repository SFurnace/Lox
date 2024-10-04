namespace LoxFsharp

open System.Collections.Generic

type LoxInterpreter =
    abstract globalEnv: Environment
    abstract execute: Stmt * Environment -> unit

type LoxCallable =
    abstract arity: int
    abstract call: LoxInterpreter -> List<obj> -> obj

type LoxFunction(decl: FuncDeclStmt) =
    interface LoxCallable with
        member this.arity = decl.parameters.Count

        member this.call interpreter args =
            let env = Environment(Some(interpreter.globalEnv))

            for i = 0 to (decl.parameters.Count - 1) do
                env.define (decl.parameters[i].lexme) (args[i])

            try
                interpreter.execute (decl.body, env)
                Expr.Literal LiteralExpr.Nil
            with :? Return as e ->
                e.Data1

    override this.ToString() = $"<fn {decl.name.lexme}>"
