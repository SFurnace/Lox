namespace LoxFsharp

open System
open System.Collections.Generic
open Microsoft.FSharp.Core

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

    let synchronize () =
        advance () |> ignore
        let mutable mark = true

        while not (isAtEnd ()) && mark do
            if previous().typ = TokenType.SEMICOLON then
                mark <- false
            else
                match peek().typ with
                | TokenType.CLASS
                | TokenType.FUN
                | TokenType.VAR
                | TokenType.FOR
                | TokenType.IF
                | TokenType.WHILE
                | TokenType.PRINT
                | TokenType.RETURN -> mark <- false
                | _ -> advance () |> ignore

    member private this.match1([<ParamArray>] tokenTypes: TokenType[]) =
        if tokenTypes |> Array.exists check then
            advance () |> ignore
            true
        else
            false

    member private this.declaration() : Option<Stmt> =
        try
            if this.match1 TokenType.VAR then
                Some(this.varDeclaration ())
            else
                Some(this.statement ())
        with :? ParserError ->
            synchronize ()
            None

    member private this.varDeclaration() =
        let name = consume TokenType.IDENTIFIER "Expect variable name."
        let value = if this.match1 TokenType.EQUAL then Some(this.expression ()) else None
        consume TokenType.SEMICOLON "Expect ';' after variable declaration." |> ignore
        Stmt.VarDecl { identifier = name; value = value }

    member private this.statement() : Stmt =
        if this.match1 TokenType.PRINT then this.printStatement ()
        elif this.match1 TokenType.WHILE then this.whileStatement ()
        elif this.match1 TokenType.LEFT_BRACE then this.block ()
        elif this.match1 TokenType.FOR then this.forStatement ()
        elif this.match1 TokenType.IF then this.ifStatement ()
        else this.expressionStatement ()

    member private this.printStatement() =
        let expr = this.expression ()
        consume TokenType.SEMICOLON "Expect ';' after value." |> ignore
        Stmt.Print expr

    member private this.whileStatement() =
        consume TokenType.LEFT_PAREN "Expect '(' after 'while'." |> ignore
        let condition = this.expression ()
        consume TokenType.RIGHT_PAREN "Expect ')' after while condition." |> ignore
        let body = this.statement ()
        Stmt.While { condition = condition; body = body }

    member private this.block() =
        let declarations = ResizeArray()

        while not (check TokenType.RIGHT_BRACE) && not (isAtEnd ()) do
            let s = this.declaration ()

            if s.IsSome then
                declarations.Add(s.Value)

        consume TokenType.RIGHT_BRACE "Expect '}' after block." |> ignore
        Stmt.Block declarations

    member private this.forStatement() =
        consume TokenType.LEFT_PAREN "Expect '(' after 'for'." |> ignore
        let mutable initializer: Option<Stmt> = None
        let mutable condition: Option<Expr> = None
        let mutable increment: Option<Expr> = None

        if this.match1 TokenType.SEMICOLON then initializer <- None
        elif this.match1 TokenType.VAR then initializer <- Some(this.varDeclaration ())
        else initializer <- Some(this.expressionStatement ())

        if not (check TokenType.SEMICOLON) then
            condition <- Some(this.expression ())

        consume TokenType.SEMICOLON "Expect ';' after loop condition." |> ignore

        if not (check TokenType.RIGHT_PAREN) then
            increment <- Some(this.expression ())

        consume TokenType.RIGHT_PAREN "Expect ')' after loop condition." |> ignore

        let mutable body = this.statement ()

        if increment.IsSome then
            body <- Stmt.Block(ResizeArray([ body; Stmt.Expr increment.Value ]))

        if condition.IsNone then
            condition <- Some(Expr.Literal LiteralExpr.True)

        if initializer.IsSome then
            Stmt.Block(ResizeArray([ initializer.Value; Stmt.While { condition = condition.Value; body = body } ]))
        else
            Stmt.While { condition = condition.Value; body = body }

    member private this.ifStatement() =
        consume TokenType.LEFT_PAREN "Expect '(' after 'if'." |> ignore
        let condition = this.expression ()
        consume TokenType.RIGHT_PAREN "Expect ')' after if condition." |> ignore
        let thenStmt = this.statement ()

        if this.match1 TokenType.ELSE then
            let elseStmt = this.statement ()
            Stmt.If { condition = condition; thenStmt = thenStmt; elseStmt = Some(elseStmt) }
        else
            Stmt.If { condition = condition; thenStmt = thenStmt; elseStmt = None }

    member private this.expressionStatement() =
        let expr = this.expression ()
        consume TokenType.SEMICOLON "Expect ';' after value." |> ignore
        Stmt.Expr expr

    member private this.expression() = this.assignment ()

    member private this.assignment() =
        let expr = this.``or`` ()

        if (this.match1 TokenType.EQUAL) then
            let equals = previous ()
            let value = this.assignment ()

            match expr with
            | Expr.Variable t -> Expr.Assign { name = t; value = value }
            | _ -> error equals "Invalid assignment target."
        else
            expr

    member private this.``or``() =
        let mutable expr = this.``and`` ()

        while this.match1 TokenType.OR do
            let operator = previous ()
            let right = this.``and`` ()
            expr <- Expr.Logical { left = expr; operator = operator; right = right }

        expr

    member private this.``and``() =
        let mutable expr = this.equality ()

        while this.match1 TokenType.AND do
            let operator = previous ()
            let right = this.equality ()
            expr <- Expr.Logical { left = expr; operator = operator; right = right }

        expr

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
        let mutable expr = this.factor ()

        while this.match1 (TokenType.PLUS, TokenType.MINUS) do
            let operation = previous ()
            let right = this.factor ()

            expr <- Expr.Binary { left = expr; operator = operation; right = right }

        expr

    member private this.factor() =
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
        elif this.match1 TokenType.IDENTIFIER then
            Expr.Variable(previous ())
        else
            error (peek ()) "bad expression"

    member this.parse() : Program =
        let statements = ResizeArray()

        while not (isAtEnd ()) do
            let stmt = this.declaration ()

            if stmt.IsSome then
                statements.Add(stmt.Value)

        statements
