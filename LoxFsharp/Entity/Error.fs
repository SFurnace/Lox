namespace LoxFsharp

type RuntimeError(token: Token, message: string) =
    inherit System.Exception(message)
    member val token = token

type Return(token: Token, value: obj) =
    inherit RuntimeError(token, "")
    member val value = value
