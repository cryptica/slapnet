{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PetriNet
    (PetriNet,name,showNetName,places,transitions,initial,
     pre,lpre,post,lpost,initials,context,arcs,
     makePetriNet,makePetriNetWithTrans)
where

import qualified Data.Map as M

data PetriNet = PetriNet {
        name :: String,
        places :: [String],
        transitions :: [String],
        adjacency :: M.Map String ([(String,Integer)], [(String,Integer)]),
        initMap :: M.Map String Integer
}

initial :: PetriNet -> String -> Integer
initial net p = M.findWithDefault 0 p (initMap net)

arcs :: PetriNet -> [(String, String, Integer)]
arcs net = concatMap (\(a,(_,bs)) -> map (\(b,w) -> (a,b,w)) bs)
                           (M.toList (adjacency net))

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

initials :: PetriNet -> [(String,Integer)]
initials net = M.toList (initMap net)

showNetName :: PetriNet -> String
showNetName net = "Petri net" ++
               (if null (name net) then "" else " " ++ show (name net))

instance Show PetriNet where
        show net = showNetName net ++
                   "\nPlaces: " ++ unwords (places net) ++
                   "\nTransitions: " ++ unwords (transitions net) ++
                   "\nArcs:\n" ++ unlines
                        (map (\(s,(l,r)) -> show l ++ " -> " ++
                            s ++ " -> " ++ show r)
                        (M.toList (adjacency net))) ++
                   "Initial: " ++ unwords
                        (map (\(n,i) -> n ++
                            (if i /= 1 then "[" ++ show i ++ "]" else []))
                        (M.toList (initMap net)))

makePetriNet :: String -> [String] -> [String] ->
        [(String, String, Integer)] -> [(String, Integer)] -> PetriNet
makePetriNet name places transitions arcs initial =
        let adjacency = foldl buildMap M.empty arcs
            initMap = M.fromList initial
        in  PetriNet { name=name, places=places, transitions=transitions,
                   adjacency=adjacency, initMap=initMap }
        where
            buildMap m (l,r,w) =
              let m'  = M.insertWith addArc l ([],[(r,w)]) m
                  m'' = M.insertWith addArc r ([(l,w)],[]) m'
              in  m''
            addArc (lNew,rNew) (lOld,rOld) = (lNew ++ lOld,rNew ++ rOld)

makePetriNetWithTrans :: String -> [String] ->
        [(String, [(String, Integer)], [(String, Integer)])] ->
        [(String, Integer)] -> PetriNet
makePetriNetWithTrans name places ts initial =
        let transitions = [ t | (t,_,_) <- ts ]
            arcs = [ (i,t,w) | (t,is,_) <- ts, (i,w) <- is ] ++
                   [ (t,o,w) | (t,_,os) <- ts, (o,w) <- os ]
        in  makePetriNet name places transitions arcs initial
