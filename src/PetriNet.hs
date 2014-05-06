{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PetriNet
    (PetriNet,showName,places,transitions,initial,
     pre,lpre,post,lpost,
     makePetriNet)
where

import qualified Data.Map as M

data PetriNet = PetriNet {
        name :: String,
        places :: [String],
        transitions :: [String],
        adjacency :: M.Map String ([(String,Integer)], [(String,Integer)]),
        initial :: [(String,Integer)]
}

context :: PetriNet -> String -> ([(String, Integer)], [(String, Integer)])
context net x = M.findWithDefault ([],[]) x (adjacency net)

pre :: PetriNet -> String -> [String]
pre net = map fst . fst . context net

lpre :: PetriNet -> String -> [(String, Integer)]
lpre net = fst . context net

post :: PetriNet -> String -> [String]
post net = map fst . snd . context net

lpost :: PetriNet -> String -> [(String, Integer)]
lpost net = snd . context net

showName :: PetriNet -> String
showName net = "Petri net" ++
               (if null (name net) then "" else " " ++ show (name net))

instance Show PetriNet where
        show net = showName net ++
                   "\nPlaces: " ++ unwords (places net) ++
                   "\nTransitions: " ++ unwords (transitions net) ++
                   "\nArcs:\n" ++ unlines
                        (map (\(s,(l,r)) -> show l ++ " -> " ++
                            s ++ " -> " ++ show r)
                        (M.toList (adjacency net))) ++
                   "Initial: " ++ unwords
                        (map (\(n,i) -> n ++
                            (if i /= 1 then "[" ++ show i ++ "]" else []))
                        (initial net))

makePetriNet :: String -> [String] -> [String] ->
        [(String, String, Integer)] -> [(String, Integer)] -> PetriNet
makePetriNet name places transitions arcs initial =
        let adjacency = foldl buildMap M.empty arcs
        in  PetriNet { name=name, places=places, transitions=transitions,
                   adjacency=adjacency, initial=initial }
        where
            buildMap m (l,r,w) =
              let m'  = M.insertWith addArc l ([],[(r,w)]) m
                  m'' = M.insertWith addArc r ([(l,w)],[]) m'
              in  m''
            addArc (lNew,rNew) (lOld,rOld) = (lNew ++ lOld,rNew ++ rOld)

