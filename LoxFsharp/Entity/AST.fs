namespace rec LoxFsharp

[<RequireQualifiedAccess>]
type Expr =
    | Literal of LiteralExpr
    | Unary of UnaryExpr
    | Binary of BinaryExpr
    | Grouping of Expr

[<RequireQualifiedAccess>]
type LiteralExpr =
    | Nil
    | True
    | False
    | Number of float
    | String of string

type UnaryExpr = { operator: Token; operand: Expr }

type BinaryExpr = { left: Expr; operator: Token; right: Expr }
