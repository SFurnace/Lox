namespace rec LoxFsharp

type Program = ResizeArray<Stmt>

[<ReferenceEquality>]
[<RequireQualifiedAccess>]
type Stmt =
    | Expr of Expr
    | Print of Expr
    | VarDecl of VarDeclStmt
    | FunDecl of FuncDeclStmt
    | ClassDecl of ClassDeclStmt
    | If of IfStmt
    | While of WhileStmt
    | Block of ResizeArray<Stmt>
    | Return of Token * Expr

[<ReferenceEquality>]
type VarDeclStmt = { identifier: Token; value: Option<Expr> }

[<ReferenceEquality>]
type FuncDeclStmt = { name: Token; parameters: ResizeArray<Token>; body: Stmt }

[<ReferenceEquality>]
type ClassDeclStmt = { name: Token; methods: ResizeArray<FuncDeclStmt> }

[<ReferenceEquality>]
type IfStmt = { condition: Expr; thenStmt: Stmt; elseStmt: Option<Stmt> }

[<ReferenceEquality>]
type WhileStmt = { condition: Expr; body: Stmt }

[<ReferenceEquality>]
[<RequireQualifiedAccess>]
type Expr =
    | Literal of LiteralExpr
    | Logical of LogicalExpr
    | Set of SetExpr
    | Unary of UnaryExpr
    | Binary of BinaryExpr
    | Call of CallExpr
    | Get of GetExpr
    | Grouping of Expr
    | Variable of Token
    | Assign of AssignExpr

[<ReferenceEquality>]
type LogicalExpr = { left: Expr; operator: Token; right: Expr }

[<ReferenceEquality>]
type SetExpr = { obj: Expr; name: Token; value: Expr }

[<ReferenceEquality>]
type UnaryExpr = { operator: Token; operand: Expr }

[<ReferenceEquality>]
type BinaryExpr = { left: Expr; operator: Token; right: Expr }

[<ReferenceEquality>]
type CallExpr = { callee: Expr; paren: Token; args: ResizeArray<Expr> }

[<ReferenceEquality>]
type GetExpr = { obj: Expr; name: Token }

[<ReferenceEquality>]
type AssignExpr = { name: Token; value: Expr }

[<ReferenceEquality>]
[<RequireQualifiedAccess>]
type LiteralExpr =
    | Nil
    | True
    | False
    | Number of float
    | String of string
