namespace LoxFsharp

open System
open System.Collections.Generic

exception ParserError of Token * string

type Parser(tokens: IList<Token>, reporter: ErrReporter) =
    let mutable current = 0

    let peek () = tokens[current]

    let previous () = tokens[current - 1]

    let isAtEnd () = peek().typ = TokenType.EOF

    let error (token: Token) message =
        reporter.error (token, message)
        raise (ParserError(token, message))

    let check tokenType = not (isAtEnd ()) && peek().typ.Equals(tokenType)

    let advance () =
        if not (isAtEnd ()) then
            current <- current + 1

        previous ()

    let consume tokenType message = if check tokenType then advance () else error (peek ()) message

    member private this.match1([<ParamArray>] tokenTypes: TokenType[]) =
        if tokenTypes |> Array.exists check then
            advance () |> ignore
            true
        else
            false

    member private this.expression() = this.equality ()

    member private this.equality() =
        let mutable expr = this.comparison ()

        while this.match1 (TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL) do
            let operation = previous ()
            let right = this.comparison ()

            expr <- Expr.Binary { left = expr; operator = operation; right = right }

        expr

    member private this.comparison() =
        let mutable expr = this.term ()

        while this.match1 (TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL) do
            let operation = previous ()
            let right = this.term ()

            expr <- Expr.Binary { left = expr; operator = operation; right = right }

        expr

    member private this.term() =
        let mutable expr = this.unary ()

        while this.match1 (TokenType.SLASH, TokenType.STAR) do
            let operation = previous ()
            let right = this.unary ()

            expr <- Expr.Binary { left = expr; operator = operation; right = right }

        expr

    member private this.unary() =
        if this.match1 (TokenType.BANG, TokenType.MINUS) then
            let operation = previous ()
            let operand = this.unary ()
            Expr.Unary { operator = operation; operand = operand }
        else
            this.primary ()

    member private this.primary() =
        if this.match1 TokenType.FALSE then
            Expr.Literal LiteralExpr.False
        elif this.match1 TokenType.TRUE then
            Expr.Literal LiteralExpr.True
        elif this.match1 TokenType.NIL then
            Expr.Literal LiteralExpr.Nil
        elif this.match1 TokenType.NUMBER then
            Expr.Literal(LiteralExpr.Number(previous().literal :?> float))
        elif this.match1 TokenType.STRING then
            Expr.Literal(LiteralExpr.String(previous().literal :?> string))
        elif this.match1 TokenType.LEFT_PAREN then
            let expr = this.expression ()
            consume TokenType.RIGHT_PAREN "Expect ')' after expression." |> ignore
            Expr.Grouping expr
        else
            error (peek ()) "bad expression"

    member this.parse() =
        try
            Some(this.expression ())
        with :? ParserError ->
            None
