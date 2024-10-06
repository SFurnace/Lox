namespace LoxFsharp

type LoxFunction(decl: {| name: Token; parameters: ResizeArray<Token>; body: Stmt |}, closure: LoxEnvironment) =
    interface LoxCallable with
        member this.arity = decl.parameters.Count

        member this.call(interpreter, args) =
            let env = Environment(Some(closure)) :> LoxEnvironment

            for i = 0 to (decl.parameters.Count - 1) do
                env.define (decl.parameters[i], args[i])

            try
                interpreter.execute (decl.body, env)
                Expr.Literal LiteralExpr.Nil
            with :? Return as e ->
                e.value

    override this.ToString() = $"<fn {decl.name.lexme}>"
