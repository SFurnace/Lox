namespace rec LoxFsharp

open Microsoft.FSharp.Core
open System.Collections.Generic
open Microsoft.FSharp.Quotations

[<RequireQualifiedAccess>]
type Program = List<Stmt>

[<RequireQualifiedAccess>]
type Stmt =
    | Expr of Expr
    | Print of Expr
    | VarDecl of VarDeclStmt
    | FunDecl of FuncDeclStmt
    | If of IfStmt
    | While of WhileStmt
    | Block of List<Stmt>
    | Return of Token * Expr

type VarDeclStmt = { identifier: Token; value: Option<Expr> }

type FuncDeclStmt = { name: Token; parameters: List<Token>; body: Stmt }

type IfStmt = { condition: Expr; thenStmt: Stmt; elseStmt: Option<Stmt> }

type WhileStmt = { condition: Expr; body: Stmt }

[<RequireQualifiedAccess>]
type Expr =
    | Literal of LiteralExpr
    | Logical of LogicalExpr
    | Unary of UnaryExpr
    | Binary of BinaryExpr
    | Call of CallExpr
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

type CallExpr = { callee: Expr; paren: Token; args: List<Expr> }
