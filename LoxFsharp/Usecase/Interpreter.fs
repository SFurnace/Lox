namespace LoxFsharp

type Interpreter(reporter: ErrReporter) =
    member this.interpret expr : obj =
        match expr with
        | Expr.Grouping v -> this.interpret v
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
                let v = this.interpret v
                Utils.checkNumberOperands op [| v |]
                -(v :?> float)
            | _ -> reporter.error (op, "Invalid unary expr.")
        | Expr.Binary { left = l; operator = op; right = r } ->
            match op.typ with
            | TokenType.MINUS ->
                let l, r = (this.interpret l), (this.interpret r)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) - (r :?> float) :> obj
            | TokenType.SLASH ->
                let l, r = (this.interpret l), (this.interpret r)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) / (r :?> float) :> obj
            | TokenType.STAR ->
                let l, r = (this.interpret l), (this.interpret r)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) * (r :?> float) :> obj
            | TokenType.PLUS ->
                match (this.interpret l), (this.interpret r) with
                | (:? float as l), (:? float as r) -> l + r :> obj
                | (:? string as l), (:? string as r) -> l + r :> obj
                | _ -> raise (RuntimeError(op, "Operands must be two numbers or two strings."))
            | TokenType.GREATER ->
                let l, r = (this.interpret l), (this.interpret r)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) > (r :?> float) :> obj
            | TokenType.GREATER_EQUAL ->
                let l, r = (this.interpret l), (this.interpret r)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) >= (r :?> float) :> obj
            | TokenType.LESS ->
                let l, r = (this.interpret l), (this.interpret r)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) < (r :?> float) :> obj
            | TokenType.LESS_EQUAL ->
                let l, r = (this.interpret l), (this.interpret r)
                Utils.checkNumberOperands op [| l; r |]
                (l :?> float) <= (r :?> float) :> obj
            | TokenType.BANG_EQUAL -> not (Utils.isEqual (this.interpret l) (this.interpret r))
            | TokenType.EQUAL_EQUAL -> Utils.isEqual (this.interpret l) (this.interpret r)
            | _ -> raise (RuntimeError(op, "Invalid binary expr."))
