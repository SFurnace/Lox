namespace LoxFsharp


type Interpreter(reporter: ErrReporter) =
    let globalEnv = Environment()

    member this.interpret(program: Program) =
        try
            for d in program do
                this.execute (d, globalEnv)
        with :? RuntimeError as e ->
            reporter.runtimeError e

    member this.execute(stmt: Stmt, env: Environment) =
        match stmt with
        | Stmt.Expr expr -> this.eval expr env |> ignore
        | Stmt.Print expr -> printfn $"{Utils.stringify (this.eval expr env)}"
        | Stmt.VarDecl stmt ->
            if stmt.value.IsSome then
                env.define stmt.identifier.lexme (this.eval stmt.value.Value env)
            else
                env.define stmt.identifier.lexme null
        | Stmt.Block stmts ->
            let newEnv = Environment(Some(env))

            for s in stmts do
                this.execute (s, newEnv)
        | Stmt.If s ->
            let v = this.eval s.condition env

            if Utils.isTruthy v then this.execute (s.thenStmt, env)
            elif s.elseStmt.IsSome then this.execute (s.elseStmt.Value, env)
            else ()
        | Stmt.While s ->
            while Utils.isTruthy (this.eval s.condition env) do
                this.execute (s.body, env)


    member this.eval expr env : obj =
        match expr with
        | Expr.Grouping v -> this.eval v env
        | Expr.Literal v ->
            match v with
            | LiteralExpr.Nil -> null
            | LiteralExpr.True -> true
            | LiteralExpr.False -> false
            | LiteralExpr.Number f -> f
            | LiteralExpr.String s -> s
        | Expr.Unary { operator = op; operand = v } ->
            match op.typ with
            | TokenType.BANG -> not (Utils.isTruthy v)
            | TokenType.MINUS ->
                let v = this.eval v env
                Utils.checkNumberOperands op [| v |]
                -(v :?> float)
            | _ -> reporter.error (op, "Invalid unary expr.")
        | Expr.Binary { left = l; operator = op; right = r } ->
            match op.typ with
            | TokenType.MINUS ->
                let l, r = (this.eval l env), (this.eval r env)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) - (r :?> float) :> obj
            | TokenType.SLASH ->
                let l, r = (this.eval l env), (this.eval r env)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) / (r :?> float) :> obj
            | TokenType.STAR ->
                let l, r = (this.eval l env), (this.eval r env)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) * (r :?> float) :> obj
            | TokenType.PLUS ->
                match (this.eval l env), (this.eval r env) with
                | :? float as l, (:? float as r) -> l + r :> obj
                | :? string as l, (:? string as r) -> l + r :> obj
                | _ -> raise (RuntimeError(op, "Operands must be two numbers or two strings."))
            | TokenType.GREATER ->
                let l, r = (this.eval l env), (this.eval r env)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) > (r :?> float) :> obj
            | TokenType.GREATER_EQUAL ->
                let l, r = (this.eval l env), (this.eval r env)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) >= (r :?> float) :> obj
            | TokenType.LESS ->
                let l, r = (this.eval l env), (this.eval r env)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) < (r :?> float) :> obj
            | TokenType.LESS_EQUAL ->
                let l, r = (this.eval l env), (this.eval r env)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) <= (r :?> float) :> obj
            | TokenType.BANG_EQUAL -> not (Utils.isEqual (this.eval l env) (this.eval r env))
            | TokenType.EQUAL_EQUAL -> Utils.isEqual (this.eval l env) (this.eval r env)
            | _ -> raise (RuntimeError(op, "Invalid binary expr."))

        | Expr.Variable t -> env.get t

        | Expr.Assign a ->
            let value = this.eval a.value env
            env.assign a.name value
            value

        | Expr.Logical v ->
            let mutable result = this.eval v.left env

            if
                (v.operator.typ = TokenType.OR && not (Utils.isTruthy result))
                || (v.operator.typ = TokenType.AND && Utils.isTruthy result)
            then
                result <- this.eval v.right env

            result
