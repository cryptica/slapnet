module Printer
    (validateId)
where

validateId :: String -> String
validateId = map (\c -> if c `elem` ",;:(){}\t \n\r" then '_' else c)
