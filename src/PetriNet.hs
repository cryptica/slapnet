{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PetriNet
    (Marking,tokens,buildMarking,
     PetriNet,name,showNetName,places,transitions,initial,initialMarking,
     pre,lpre,post,lpost,initials,context,arcs,ghostTransitions,
     makePetriNet,makePetriNetWithTrans)
where

import qualified Data.Map as M

type Place = String
type Transition = String
type Node = String
newtype Marking = Marking { getMarkingMap :: M.Map Place Integer }

instance Show Marking where
        show (Marking m) = unwords $ map showPlaceMarking $ M.toList m
            where showPlaceMarking (n,i) =
                    n ++ (if i /= 1 then "[" ++ show i ++ "]" else "")

tokens :: Marking -> Place -> Integer
tokens m p = M.findWithDefault 0 p (getMarkingMap m)

buildMarking :: [(Place, Integer)] -> Marking
buildMarking xs = Marking $ M.fromList $ filter ((/=0) . snd) xs

data PetriNet = PetriNet {
        name :: String,
        places :: [Place],
        transitions :: [Transition],
        adjacency :: M.Map Node ([(Node,Integer)], [(Node,Integer)]),
        initialMarking :: Marking,
        ghostTransitions :: [Transition]
}

initial :: PetriNet -> Place -> Integer
initial net = tokens (initialMarking net)

arcs :: PetriNet -> [(Node, Node, Integer)]
arcs net = concatMap (\(a,(_,bs)) -> map (\(b,w) -> (a,b,w)) bs)
                           (M.toList (adjacency net))

context :: PetriNet -> Node -> ([(Node, Integer)], [(Node, Integer)])
context net x = M.findWithDefault ([],[]) x (adjacency net)

pre :: PetriNet -> Node -> [Node]
pre net = map fst . fst . context net

lpre :: PetriNet -> Node -> [(Node, Integer)]
lpre net = fst . context net

post :: PetriNet -> Node -> [Node]
post net = map fst . snd . context net

lpost :: PetriNet -> Node -> [(Node, Integer)]
lpost net = snd . context net

initials :: PetriNet -> [(Place,Integer)]
initials net = M.toList (getMarkingMap (initialMarking net))

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
                   "Initial: " ++ show (initialMarking net) ++
                   "\nGhost transitions: " ++ unwords (ghostTransitions net)

makePetriNet :: String -> [Place] -> [Transition] ->
        [(Node, Node, Integer)] -> [(Place, Integer)] -> [Transition] -> PetriNet
makePetriNet name places transitions arcs initial gs =
        let adjacency = foldl buildMap M.empty $ filter (\(_,_,w) -> w /= 0) arcs
        in  PetriNet { name=name, places=places, transitions=transitions,
                       adjacency=adjacency, initialMarking=buildMarking initial,
                       ghostTransitions=gs }
        where
            buildMap m (l,r,w) =
              let m'  = M.insertWith addArc l ([],[(r,w)]) m
                  m'' = M.insertWith addArc r ([(l,w)],[]) m'
              in  m''
            addArc (lNew,rNew) (lOld,rOld) = (lNew ++ lOld,rNew ++ rOld)

makePetriNetWithTrans :: String -> [Place] ->
        [(Transition, [(Place, Integer)], [(Place, Integer)])] ->
        [(Place, Integer)] -> [Transition] -> PetriNet
makePetriNetWithTrans name places ts initial gs =
        let transitions = [ t | (t,_,_) <- ts ]
            arcs = [ (i,t,w) | (t,is,_) <- ts, (i,w) <- is ] ++
                   [ (t,o,w) | (t,_,os) <- ts, (o,w) <- os ]
        in  makePetriNet name places transitions arcs initial gs
