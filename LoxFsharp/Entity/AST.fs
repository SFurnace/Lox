namespace rec LoxFsharp

[<RequireQualifiedAccess>]
type Program = ResizeArray<Stmt>

[<RequireQualifiedAccess>]
type Stmt =
    | Expr of Expr
    | Print of Expr
    | VarDecl of {| identifier: Token; value: Option<Expr> |}
    | FunDecl of {| name: Token; parameters: ResizeArray<Token>; body: Stmt |}
    | If of {| condition: Expr; thenStmt: Stmt; elseStmt: Option<Stmt> |}
    | While of {| condition: Expr; body: Stmt |}
    | Block of ResizeArray<Stmt>
    | Return of Token * Expr

[<RequireQualifiedAccess>]
type Expr =
    | Literal of LiteralExpr
    | Logical of {| left: Expr; operator: Token; right: Expr |}
    | Unary of {| operator: Token; operand: Expr |}
    | Binary of {| left: Expr; operator: Token; right: Expr |}
    | Call of {| callee: Expr; paren: Token; args: ResizeArray<Expr> |}
    | Grouping of Expr
    | Variable of Token
    | Assign of {| name: Token; value: Expr |}

[<RequireQualifiedAccess>]
type LiteralExpr =
    | Nil
    | True
    | False
    | Number of float
    | String of string
