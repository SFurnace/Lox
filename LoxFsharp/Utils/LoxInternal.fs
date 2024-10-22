namespace rec LoxFsharp

open System.Collections.Generic

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

    override this.ToString() = $"<fn {decl.name.lexeme}>"

type LoxClass(name: Token, methods: IDictionary<string, LoxCallable>) =
    interface LoxCallable with
        member val arity = 0
        member this.call(_, _) = LoxInstance(this)

    member this.hasMethod(name) = methods.ContainsKey(name)

    member this.getMethod(name) = methods[name]

    override this.ToString() = $"<class {name.lexeme}>"

type LoxInstance(klass: LoxClass) =
    let fields = Dictionary()

    member this.get(name: Token) : obj =
        if fields.ContainsKey name.lexeme then fields[name.lexeme]
        elif klass.hasMethod (name.lexeme) then klass.getMethod (name.lexeme)
        else raise (RuntimeError(name, $"Undefined property {name.lexeme}"))

    member this.set(name: Token, value: obj) = fields[name.lexeme] <- value

    override this.ToString() = $"<instance {klass}>"
