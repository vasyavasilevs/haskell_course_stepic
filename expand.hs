module Demo where

infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)


expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = if (expand e1 :*: expand e :+: expand e2 :*: expand e) == ((e1 :+: e2) :*: e) then (expand e1 :*: expand e :+: expand e2 :*: expand e) else expand (expand e1 :*: expand e :+: expand e2 :*: expand e)
expand (e :*: (e1 :+: e2)) = if (expand e :*: expand e1 :+: expand e :*: expand e2) == (e :*: (e1 :+: e2)) then (expand e :*: expand e1 :+: expand e :*: expand e2) else expand (expand e :*: expand e1 :+: expand e :*: expand e2)
expand (e1 :+: e2) = if (expand e1 :+: expand e2) == (e1 :+: e2) then (expand e1 :+: expand e2) else expand (expand e1 :+: expand e2)
expand (e1 :*: e2) = if (expand e1 :*: expand e2) == (e1 :*: e2) then (expand e1 :*: expand e2) else expand (expand e1 :*: expand e2)
expand e = e