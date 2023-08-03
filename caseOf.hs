module Demo where

processData :: SomeData -> String
processData tmp = 
    case snd (doSomeWork tmp) of 
        0 -> "Success"
        errno -> "Fail: " ++ show errno