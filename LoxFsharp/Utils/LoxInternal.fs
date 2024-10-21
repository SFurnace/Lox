namespace rec LoxFsharp

type LoxFunction(decl: FuncDeclStmt, closure: LoxEnvironment) =
    interface LoxCallable with
        member this.arity = decl.parameters.Count

        member this.call(interpreter, args) =
            let env = Environment(Some(closure :?> Environment)) :> LoxEnvironment

            for i = 0 to (decl.parameters.Count - 1) do
                env.define (decl.parameters[i], args[i])

            try
                interpreter.execute (decl.body, env)
                Expr.Literal LiteralExpr.Nil
            with :? Return as e ->
                e.value

    override this.ToString() = $"<fn {decl.name.lexme}>"

type LoxClass(name: Token) =
    interface LoxCallable with
        member val arity = 0
        member this.call(_, _) = LoxInstance(this)

    override this.ToString() = $"<class {name.lexme}>"

type LoxInstance(klass: LoxClass) =
    override this.ToString() = $"<instance {klass}>"
