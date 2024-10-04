namespace rec LoxFsharp

open Microsoft.FSharp.Core
open System.Collections.Generic

[<RequireQualifiedAccess>]
type Program = List<Stmt>

[<RequireQualifiedAccess>]
type Stmt =
    | Expr of Expr
    | Print of Expr
    | VarDecl of VarDeclStmt
    | If of IfStmt
    | While of WhileStmt
    | Block of List<Stmt>

type VarDeclStmt = { identifier: Token; value: Option<Expr> }

type IfStmt = { condition: Expr; thenStmt: Stmt; elseStmt: Option<Stmt> }

type WhileStmt = { condition: Expr; body: Stmt }

[<RequireQualifiedAccess>]
type Expr =
    | Literal of LiteralExpr
    | Logical of LogicalExpr
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

type LogicalExpr = { left: Expr; operator: Token; right: Expr }
