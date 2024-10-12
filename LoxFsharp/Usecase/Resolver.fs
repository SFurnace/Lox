namespace LoxFsharp

open System.Collections.Generic

[<RequireQualifiedAccess>]
type private FunctionType =
    | None
    | Function

type Resolver(interpreter: LoxInterpreter, reporter: ErrReporter) as this =
    let scopes: ResizeArray<IDictionary<string, bool>> = ResizeArray()
    let mutable currentFunction = FunctionType.None

    let beginScope () = scopes.Add(Dictionary())
    let endScope () = scopes.RemoveAt(scopes.Count - 1)

    let declare (name: Token) =
        if scopes.Count > 0 then // 全局空间不在此处理
            let s = scopes[scopes.Count - 1]

            if s.ContainsKey name.lexme then
                reporter.error (name, "Already a variable with this name in this scope.")
            else
                s.Add(name.lexme, false)

    let define (name: Token) =
        if scopes.Count > 0 then // 全局空间不在此处理
            scopes[scopes.Count - 1][name.lexme] <- true

    let resolveLocal (expr: Expr, name: Token) =
        let mutable i = scopes.Count - 1
        let mutable resolved = false

        while i >= 0 && not resolved do
            if scopes[i].ContainsKey name.lexme then
                interpreter.resolve (expr, scopes.Count - 1 - i)
                resolved <- true

            i <- i - 1

    let resolveFunction (f: {| name: Token; parameters: ResizeArray<Token>; body: Stmt |}, typ: FunctionType) =
        let enclosingFunction = currentFunction
        currentFunction <- typ
        beginScope ()

        for p in f.parameters do
            declare p
            define p

        this.analyse f.body
        endScope ()
        currentFunction <- enclosingFunction

    interface LoxStaticAnalyser with
        member this.analyse(program) =
            for stmt in program do
                this.analyse stmt

    member this.analyse(stmt: Stmt) =
        match stmt with
        | Stmt.Block stmts ->
            beginScope ()

            for stmt in stmts do
                this.analyse stmt

            endScope ()
        | Stmt.VarDecl v ->
            declare v.identifier

            if v.value.IsSome then
                this.analyse v.value.Value

            define v.identifier
        | Stmt.FunDecl v ->
            declare v.name
            define v.name
            resolveFunction (v, FunctionType.Function)

        | Stmt.Expr expr -> this.analyse expr
        | Stmt.Print expr -> this.analyse expr
        | Stmt.If v ->
            this.analyse v.condition
            this.analyse v.thenStmt

            if v.elseStmt.IsSome then
                this.analyse v.elseStmt.Value
        | Stmt.While v ->
            this.analyse v.condition
            this.analyse v.body
        | Stmt.Return(tok, expr) ->
            if currentFunction = FunctionType.None then
                reporter.error (tok, "Can't return from top-level code.")
            else
                this.analyse expr

    member this.analyse(expr: Expr) =
        match expr with
        | Expr.Variable v ->
            if
                scopes.Count > 0
                && scopes[scopes.Count - 1].ContainsKey(v.lexme)
                && scopes[scopes.Count - 1][v.lexme] = false
            then
                reporter.error (v, "Can't read local variable in its own initializer.")

            resolveLocal (expr, v)
        | Expr.Grouping v -> this.analyse v
        | Expr.Literal _ -> ()
        | Expr.Logical v ->
            this.analyse v.left
            this.analyse v.right
        | Expr.Unary v -> this.analyse v.operand
        | Expr.Binary v ->
            this.analyse v.left
            this.analyse v.right
        | Expr.Call v ->
            this.analyse v.callee

            for arg in v.args do
                this.analyse arg
        | Expr.Assign v ->
            this.analyse v.value
            resolveLocal (expr, v.name)
