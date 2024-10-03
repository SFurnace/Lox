namespace rec LoxFsharp

[<RequireQualifiedAccess>]
type Stmt =
    | Expr of Expr
    | Print of Expr
    | Var of VarStmt
    | Block of ResizeArray<Stmt>

type VarStmt = { identifier: Token; value: Option<Expr> }

[<RequireQualifiedAccess>]
type Expr =
    | Literal of LiteralExpr
    | Unary of UnaryExpr
    | Binary of BinaryExpr
    | Grouping of Expr
    | Variable of Token
    | Assign of AssignExpr

[<RequireQualifiedAccess>]
type LiteralExpr =
    | Nil
    | True
    | False
    | Number of float
    | String of string

type UnaryExpr = { operator: Token; operand: Expr }

type BinaryExpr = { left: Expr; operator: Token; right: Expr }

type AssignExpr = { name: Token; value: Expr }
