{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PetriNet
    (PetriNet,name,places,transitions,
     makePetriNet)
where

data PetriNet = PetriNet {
        name :: String,
        places :: [String],
        transitions :: [String],
        arcs :: [(String,String,Integer)],
        initial :: [(String,Integer)]
}

instance Show PetriNet where
        show net = "Petri net " ++ name net ++ "\n" ++
                   "Places: " ++ unwords (places net) ++ "\n" ++
                   "Transitions: " ++ unwords (transitions net) ++ "\n" ++
                   "Arcs:\n" ++ unlines
                        (map (\(l,r,w) -> l ++ " ->" ++
                            (if w /= 1 then "[" ++ show w ++ "]" else []) ++
                            " " ++ r)
                        (arcs net)) ++
                   "Initial: " ++ unwords
                        (map (\(n,i) -> n ++
                            (if i /= 1 then "[" ++ show i ++ "]" else []))
                        (initial net))

makePetriNet :: String -> [String] -> [String] ->
        [(String, String,Integer)] -> [(String, Integer)] -> PetriNet
makePetriNet name places transitions arcs initial =
        PetriNet { name=name, places=places, transitions=transitions,
                   arcs=arcs, initial=initial }
