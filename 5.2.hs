module Demo where

import Control.Monad (ap, liftM)

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = func where
                func x = Log [msg] (f x)
{-
GHCi> let add1Log = toLogger (+1) "added one"
GHCi> add1Log 3
Log ["added one"] 4

GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
GHCi> mult2Log 3
Log ["multiplied by 2"] 6
-}

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (fstF ++ fstG) resG where
                    Log fstF resF = f x
                    Log fstG resG = g resF

{-
GHCi> execLoggers 3 add1Log mult2Log
Log ["added one","multiplied by 2"] 8
-}



{-
GHCi> Log ["nothing done yet"] 0 `bindLog` add1Log
Log ["nothing done yet","added one"] 1

GHCi> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
Log ["nothing done yet","added one","multiplied by 2"] 8

(>>=) :: m a -> (a -> m b) -> m b -- произносят bind (монадическое связывание)
-}

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log strA valueA) foo = res where
                Log strB valueB = foo valueA 
                res = Log (strA ++ strB) valueB 

returnLog :: a -> Log a
returnLog a = Log [] a


instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

{-
execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x fs = foldr (\f acc -> acc >>= f) (return x) (reverse fs)
-}

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x fs = foldl (>>=) (return x) fs