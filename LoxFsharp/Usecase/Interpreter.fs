namespace rec LoxFsharp

open System.Collections.Generic

type Interpreter(reporter: ErrReporter) =
    let globalEnv: LoxEnvironment = Environment()
    let locals: IDictionary<Expr, int> = Dictionary()

    do
        globalEnv.define (
            { typ = TokenType.IDENTIFIER; lexeme = "clock"; line = -1; literal = null },
            { new LoxCallable with
                member this.arity = 0
                member this.call(_, _) = float (System.DateTimeOffset.Now.ToUnixTimeMilliseconds()) / 1000.0 :> obj }
        )

    interface LoxInterpreter with
        member this.resolve(var: Expr, distance: int) = locals.Add(var, distance)

        member this.execute(stmt: Stmt, env) =
            match stmt with
            | Stmt.Expr expr -> this.eval expr env |> ignore
            | Stmt.Print expr -> printf $"{Utils.Stringify(this.eval expr env)}\n"
            | Stmt.VarDecl stmt ->
                if stmt.value.IsSome then
                    env.define (stmt.identifier, (this.eval stmt.value.Value env))
                else
                    env.define (stmt.identifier, null)
            | Stmt.Block stmts ->
                let newEnv = Environment(Some(env :?> Environment))

                for s in stmts do
                    (this :> LoxInterpreter).execute (s, newEnv)
            | Stmt.If s ->
                let v = this.eval s.condition env

                if Utils.IsTruthy v then
                    (this :> LoxInterpreter).execute (s.thenStmt, env)
                elif s.elseStmt.IsSome then
                    (this :> LoxInterpreter).execute (s.elseStmt.Value, env)
                else
                    ()
            | Stmt.While s ->
                while Utils.IsTruthy(this.eval s.condition env) do
                    (this :> LoxInterpreter).execute (s.body, env)
            | Stmt.FunDecl s -> env.define (s.name, LoxFunction(s, env))
            | Stmt.Return(token, expr) ->
                let value = this.eval expr env
                raise (Return(token, value))
            | Stmt.ClassDecl s ->
                env.define (s.name, null)

                let methods = Dictionary()

                for method in s.methods do
                    methods.Add(method.name.lexeme, LoxFunction(method, env) :> LoxCallable)

                let klass = LoxClass(s.name, methods)
                env.assign (s.name, klass)

    member this.interpret(program: Program) =
        try
            for d in program do
                (this :> LoxInterpreter).execute (d, globalEnv)
        with :? RuntimeError as e ->
            reporter.runtimeError e

    member this.eval expr (env: LoxEnvironment) : obj =
        match expr with
        | Expr.Grouping v -> this.eval v env
        | Expr.Literal v ->
            match v with
            | LiteralExpr.Nil -> null
            | LiteralExpr.True -> true
            | LiteralExpr.False -> false
            | LiteralExpr.Number f -> f
            | LiteralExpr.String s -> s
        | Expr.Unary v ->
            match v.operator.typ with
            | TokenType.BANG -> not (Utils.IsTruthy v)
            | TokenType.MINUS ->
                let tmp = this.eval v.operand env
                Utils.CheckNumberOperands v.operator [| tmp |]
                -(tmp :?> float)
            | _ -> reporter.error (v.operator, "Invalid unary expr.")
        | Expr.Binary v ->
            match v.operator.typ with
            | TokenType.MINUS ->
                let l, r = (this.eval v.left env), (this.eval v.right env)
                Utils.CheckNumberOperands v.operator [| l; r |]
                (l :?> float) - (r :?> float) :> obj
            | TokenType.SLASH ->
                let l, r = (this.eval v.left env), (this.eval v.right env)
                Utils.CheckNumberOperands v.operator [| l; r |]
                (l :?> float) / (r :?> float) :> obj
            | TokenType.STAR ->
                let l, r = (this.eval v.left env), (this.eval v.right env)
                Utils.CheckNumberOperands v.operator [| l; r |]
                (l :?> float) * (r :?> float) :> obj
            | TokenType.PLUS ->
                match (this.eval v.left env), (this.eval v.right env) with
                | :? float as l, (:? float as r) -> l + r :> obj
                | :? string as l, (:? string as r) -> l + r :> obj
                | _ -> raise (RuntimeError(v.operator, "Operands must be two numbers or two strings."))
            | TokenType.GREATER ->
                let l, r = (this.eval v.left env), (this.eval v.right env)
                Utils.CheckNumberOperands v.operator [| l; r |]
                (l :?> float) > (r :?> float) :> obj
            | TokenType.GREATER_EQUAL ->
                let l, r = (this.eval v.left env), (this.eval v.right env)
                Utils.CheckNumberOperands v.operator [| l; r |]
                (l :?> float) >= (r :?> float) :> obj
            | TokenType.LESS ->
                let l, r = (this.eval v.left env), (this.eval v.right env)
                Utils.CheckNumberOperands v.operator [| l; r |]
                (l :?> float) < (r :?> float) :> obj
            | TokenType.LESS_EQUAL ->
                let l, r = (this.eval v.left env), (this.eval v.right env)
                Utils.CheckNumberOperands v.operator [| l; r |]
                (l :?> float) <= (r :?> float) :> obj
            | TokenType.BANG_EQUAL -> not (Utils.IsEqual (this.eval v.left env) (this.eval v.right env))
            | TokenType.EQUAL_EQUAL -> Utils.IsEqual (this.eval v.left env) (this.eval v.right env)
            | _ -> raise (RuntimeError(v.operator, "Invalid binary expr."))

        | Expr.Variable v -> if locals.ContainsKey expr then env.getAt (locals[expr], v) else globalEnv.get v

        | Expr.Assign v ->
            let value = this.eval v.value env

            if locals.ContainsKey expr then
                env.assignAt (locals[expr], v.name, value)
            else
                globalEnv.assign (v.name, value)

            value

        | Expr.Logical v ->
            let mutable result = this.eval v.left env

            if
                (v.operator.typ = TokenType.OR && not (Utils.IsTruthy result))
                || (v.operator.typ = TokenType.AND && Utils.IsTruthy result)
            then
                result <- this.eval v.right env

            result

        | Expr.Call v ->
            let callee = this.eval v.callee env
            let args = v.args.ConvertAll(fun a -> this.eval a env)

            match callee with
            | :? LoxCallable as callee ->
                if args.Count <> callee.arity then
                    raise (RuntimeError(v.paren, $"Expected {callee.arity} arguments but got {args.Count}."))

                callee.call (this, args)
            | _ -> raise (RuntimeError(v.paren, "Can only call functions and classes."))

        | Expr.Get v ->
            match this.eval v.obj env with
            | :? LoxInstance as obj -> obj.get v.name
            | _ -> raise (RuntimeError(v.name, "Only instances have properties."))

        | Expr.Set v ->
            match this.eval v.obj env with
            | :? LoxInstance as obj ->
                let value = this.eval v.value env
                obj.set (v.name, value)
            | _ -> raise (RuntimeError(v.name, "Only instances have fields."))
