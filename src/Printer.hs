module Printer
    (validateId)
where

import Data.Char

validateId :: String -> String
validateId "" = error "empty id"
validateId (x:xs) = (if isAlpha x then x else '_') :
        map (\c -> if isAlphaNum c then c else '_') xs
