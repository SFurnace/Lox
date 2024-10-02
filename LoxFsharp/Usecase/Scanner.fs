namespace LoxFsharp

open System
open System.Collections.Generic

type Scanner(source: string, reporter: ErrReporter) =
    static let keywords =
        Map
            [ "and", TokenType.AND
              "class", TokenType.CLASS
              "else", TokenType.ELSE
              "false", TokenType.FALSE
              "for", TokenType.FOR
              "fun", TokenType.FUN
              "if", TokenType.IF
              "nil", TokenType.NIL
              "or", TokenType.OR
              "print", TokenType.PRINT
              "return", TokenType.RETURN
              "super", TokenType.SUPER
              "this", TokenType.THIS
              "true", TokenType.TRUE
              "var", TokenType.VAR
              "while", TokenType.WHILE ]

    let tokens: IList<Token> = ResizeArray()
    let mutable start = 0
    let mutable current = 0
    let mutable line = 1

    let isDigit c = '0' <= c && c <= '9'

    let isAlpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'

    let isAlphaDigit c = (isAlpha c) || (isDigit c)

    let isAtEnd () = current >= source.Length

    let advance () =
        let c = source[current]
        current <- current + 1
        c

    let match1 expect =
        if isAtEnd () || (source[current] <> expect) then
            false
        else
            current <- current + 1
            true

    let peek () = if isAtEnd () then char 0 else source[current]

    let peekNext () = if current + 1 >= source.Length then char 0 else source[current + 1]

    let addToken (typ: TokenType) (literal: Object) =
        tokens.Add
            { typ = typ
              lexme = source.Substring(start, current - start)
              literal = literal
              line = line }

    let addTokenSimple (typ: TokenType) = addToken typ null

    let dropLineComment () =
        while peek () <> '\n' && not (isAtEnd ()) do
            advance () |> ignore

    let dropBlockComment () =
        let mutable depth = 1

        while not (depth = 0) && not (isAtEnd ()) do
            if peek () = '/' && peekNext () = '*' then
                advance () |> ignore
                advance () |> ignore
                depth <- depth + 1
            elif peek () = '*' && peekNext () = '/' then
                advance () |> ignore
                advance () |> ignore
                depth <- depth - 1
            elif match1 '\n' then
                line <- line + 1
            else
                advance () |> ignore

        if depth <> 0 then
            reporter.error (line, "Unterminated block comment.")

    let scanString () =
        while peek () <> '"' && not (isAtEnd ()) do
            if peek () = '\n' then
                line <- line + 1

            advance () |> ignore

        if isAtEnd () then
            reporter.error (line, "Unterminated string.")
        else
            advance () |> ignore
            let value = source.Substring((start + 1), (current - 1) - (start + 1))
            addToken TokenType.STRING value

    let scanNumber () =
        while isDigit (peek ()) do
            advance () |> ignore

        if peek () = '.' && isDigit (peekNext ()) then
            advance () |> ignore

        while isDigit (peek ()) do
            advance () |> ignore

        addToken TokenType.NUMBER (Double.Parse(source.Substring(start, current - start)))

    let scanIdentifier () =
        while isAlphaDigit (peek ()) do
            advance () |> ignore

        let text = source.Substring(start, current - start)

        if keywords.ContainsKey(text) then
            addTokenSimple (keywords[text])
        else
            addTokenSimple TokenType.IDENTIFIER

    let scanToken () =
        let c = advance ()

        match c with
        | '(' -> addTokenSimple TokenType.LEFT_PAREN
        | ')' -> addTokenSimple TokenType.RIGHT_PAREN
        | '{' -> addTokenSimple TokenType.LEFT_BRACE
        | '}' -> addTokenSimple TokenType.RIGHT_BRACE
        | ',' -> addTokenSimple TokenType.COMMA
        | '.' -> addTokenSimple TokenType.DOT
        | '-' -> addTokenSimple TokenType.MINUS
        | '+' -> addTokenSimple TokenType.PLUS
        | ';' -> addTokenSimple TokenType.SEMICOLON
        | '*' -> addTokenSimple TokenType.STAR
        | '!' -> addTokenSimple (if match1 '=' then TokenType.BANG_EQUAL else TokenType.BANG)
        | '=' -> addTokenSimple (if match1 '=' then TokenType.EQUAL_EQUAL else TokenType.EQUAL)
        | '<' -> addTokenSimple (if match1 '=' then TokenType.LESS_EQUAL else TokenType.LESS)
        | '>' -> addTokenSimple (if match1 '=' then TokenType.GREATER_EQUAL else TokenType.GREATER)
        | '/' ->
            if match1 '/' then dropLineComment ()
            elif match1 '*' then dropBlockComment ()
            else addTokenSimple TokenType.SLASH
        | ' '
        | '\r'
        | '\t' -> ()
        | '\n' -> line <- line + 1
        | '"' -> scanString ()
        | c when isDigit c -> scanNumber ()
        | c when isAlpha c -> scanIdentifier ()
        | _ -> reporter.error (line, $"Unexpect character {c}.")

    member this.scanTokens() =
        while not (isAtEnd ()) do
            start <- current
            scanToken ()

        tokens.Add { typ = TokenType.EOF; lexme = ""; literal = null; line = line }

        tokens
