type Id = String
data RetType = Type | Void
data Type = Num Int | Bool | Tuple Type Type | List Type | Id

data Op1 = Negate | UnitaryMinus
data Op2 = Add | Sub | Mult | Div | Mod | Equals | Less | More | LessEq | MoreEq | Not | And | Or -- what's ':'?

main = do putStrLn "test"

