module LoxFsharp.Scanner

open System
open LoxFsharp.Utils

type TokenType =
    // Single-character tokens.
    | LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR
    // One or two character tokens.
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    // Literals.
    | IDENTIFIER
    | STRING
    | NUMBER
    // Keywords.
    | AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | TRUE
    | VAR
    | WHILE
    | EOF

[<Struct>]
type Token =
    { typ: TokenType
      lexme: string
      literal: Object
      line: int }

    override this.ToString() =
        $"{this.typ} %s{this.lexme} %A{this.literal}"

type Scanner(source: string, reporter: ErrReporter) =
    static let keywords =
        Map
            [ "and", AND
              "class", CLASS
              "else", ELSE
              "false", FALSE
              "for", FOR
              "fun", FUN
              "if", IF
              "nil", NIL
              "or", OR
              "print", PRINT
              "return", RETURN
              "super", SUPER
              "this", THIS
              "true", TRUE
              "var", VAR
              "while", WHILE ]


    let tokens = ResizeArray<Token>()
    let mutable start = 0
    let mutable current = 0
    let mutable line = 1

    let isDigit c = '0' <= c && c <= '9'

    let isAlpha c =
        ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'

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

    let peek () =
        if isAtEnd () then char 0 else source[current]

    let peekNext () =
        if current + 1 >= source.Length then
            char 0
        else
            source[current + 1]

    let addToken (typ: TokenType) (literal: Object) =
        tokens.Add
            { typ = typ
              lexme = source.Substring(start, current - start)
              literal = literal
              line = line }

    let addTokenSimple (typ: TokenType) = addToken typ null

    let scanString () =
        while peek () <> '"' && not (isAtEnd ()) do
            if peek () = '\n' then
                line <- line + 1

            advance () |> ignore

        if isAtEnd () then
            reporter.error line "Unterminated string."
        else
            advance () |> ignore
            let value = source.Substring((start + 1), (current - 1) - (start + 1))
            addToken STRING value

    let scanNumber () =
        while isDigit (peek ()) do
            advance () |> ignore

        if peek () = '.' && isDigit (peekNext ()) then
            advance () |> ignore

        while isDigit (peek ()) do
            advance () |> ignore

        addToken NUMBER (Double.Parse(source.Substring(start, current - start)))

    let scanIdentifier () =
        while isAlphaDigit (peek ()) do
            advance () |> ignore

        let text = source.Substring(start, current - start)

        if keywords.ContainsKey(text) then
            addTokenSimple (keywords[text])
        else
            addTokenSimple IDENTIFIER

    let scanToken () =
        let c = advance ()

        match c with
        | '(' -> addTokenSimple LEFT_PAREN
        | ')' -> addTokenSimple RIGHT_PAREN
        | '{' -> addTokenSimple LEFT_BRACE
        | '}' -> addTokenSimple RIGHT_BRACE
        | ',' -> addTokenSimple COMMA
        | '.' -> addTokenSimple DOT
        | '-' -> addTokenSimple MINUS
        | '+' -> addTokenSimple PLUS
        | ';' -> addTokenSimple SEMICOLON
        | '*' -> addTokenSimple STAR
        | '!' -> addTokenSimple (if match1 '=' then BANG_EQUAL else BANG)
        | '=' -> addTokenSimple (if match1 '=' then EQUAL_EQUAL else EQUAL)
        | '<' -> addTokenSimple (if match1 '=' then LESS_EQUAL else LESS)
        | '>' -> addTokenSimple (if match1 '=' then GREATER_EQUAL else GREATER)
        | '/' ->
            if match1 '/' then
                while (peek () <> '\n') && not (isAtEnd ()) do
                    advance () |> ignore
            else
                addTokenSimple SLASH
        | ' '
        | '\r'
        | '\t' -> ()
        | '\n' -> line <- line + 1
        | '"' -> scanString ()
        | c when isDigit c -> scanNumber ()
        | c when isAlpha c -> scanIdentifier ()
        | _ -> reporter.error line $"Unexpect character {c}."

    member this.scanTokens() =
        while not (isAtEnd ()) do
            start <- current
            scanToken ()

        tokens.Add
            { typ = EOF
              lexme = ""
              literal = null
              line = line }

        tokens
