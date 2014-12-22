{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PetriNet
    (PetriNet,Place(..),Transition(..),Marking,FiringVector,
     renamePlace,renameTransition,renamePetriNetPlacesAndTransitions,
     name,showNetName,places,transitions,
     initialMarking,initial,initials,linitials,
     pre,lpre,post,lpost,mpre,mpost,context,ghostTransitions,
     makePetriNet,makePetriNetWithTrans,makePetriNetWith,Trap,Cut,
     constructCut)
where

import qualified Data.Map as M
import Control.Arrow (first,(***))
import Data.List (sort,(\\))

import Util

newtype Place = Place String deriving (Ord,Eq)
newtype Transition = Transition String deriving (Ord,Eq)

instance Show Place where
        show (Place p) = p
instance Show Transition where
        show (Transition t) = t

type ContextMap a b = M.Map a ([(b, Integer)],[(b, Integer)])

-- TODO: Use Map/Set for pre/post
class (Ord a, Ord b) => Nodes a b | a -> b where
        lpre :: PetriNet -> a -> [(b, Integer)]
        lpre net = fst . context net
        lpost :: PetriNet -> a -> [(b, Integer)]
        lpost net = snd . context net
        pre :: PetriNet -> a -> [b]
        pre net = map fst . lpre net
        post :: PetriNet -> a -> [b]
        post net = map fst . lpost net
        lmpre :: PetriNet -> [a] -> [(b, Integer)]
        lmpre net = nubOrdBy fst . concatMap (lpre net)
        lmpost :: PetriNet -> [a] -> [(b, Integer)]
        lmpost net = nubOrdBy fst . concatMap (lpost net)
        mpre :: PetriNet -> [a] -> [b]
        mpre net = map fst . lmpre net
        mpost :: PetriNet -> [a] -> [b]
        mpost net = map fst . lmpost net
        context :: PetriNet -> a -> ([(b, Integer)], [(b, Integer)])
        context net x = M.findWithDefault ([],[]) x (contextMap net)
        contextMap :: PetriNet -> ContextMap a b

instance Nodes Place Transition where
        contextMap = adjacencyP
instance Nodes Transition Place where
        contextMap = adjacencyT

type Marking = Vector Place
type FiringVector = Vector Transition

type Trap = [Place]
-- TODO: generalize cut type
type Cut = ([([Place], [Transition])], [Transition])

data PetriNet = PetriNet {
        name :: String,
        places :: [Place],
        transitions :: [Transition],
        adjacencyP :: M.Map Place ([(Transition,Integer)], [(Transition,Integer)]),
        adjacencyT :: M.Map Transition ([(Place,Integer)], [(Place,Integer)]),
        initialMarking :: Marking,
        ghostTransitions :: [Transition]
}

initial :: PetriNet -> Place -> Integer
initial net = val (initialMarking net)

initials :: PetriNet -> [Place]
initials = elems . initialMarking

linitials :: PetriNet -> [(Place,Integer)]
linitials = items . initialMarking

showNetName :: PetriNet -> String
showNetName net = "Petri net" ++
               (if null (name net) then "" else " " ++ show (name net))

instance Show PetriNet where
        show net = showNetName net ++
                   "\nPlaces: " ++ show (places net) ++
                   "\nTransitions: " ++ show (transitions net) ++
                   "\nPlace arcs:\n" ++ unlines
                        (map showContext (M.toList (adjacencyP net))) ++
                   "\nTransition arcs:\n" ++ unlines
                        (map showContext (M.toList (adjacencyT net))) ++
                   "\nInitial: " ++ show (initialMarking net) ++
                   "\nGhost transitions: " ++ show (ghostTransitions net)
                where showContext (s,(l,r)) =
                          show l ++ " -> " ++ show s ++ " -> " ++ show r

renamePlace :: (String -> String) -> Place -> Place
renamePlace f (Place p) = Place (f p)

renameTransition :: (String -> String) -> Transition -> Transition
renameTransition f (Transition t) = Transition (f t)

renamePetriNetPlacesAndTransitions :: (String -> String) -> PetriNet -> PetriNet
renamePetriNetPlacesAndTransitions f net =
            PetriNet {
                name = name net,
                places      = map (renamePlace f) $ places net,
                transitions = map (renameTransition f) $ transitions net,
                adjacencyP  = mapAdjacency (renamePlace f) (renameTransition f) $
                    adjacencyP net,
                adjacencyT  = mapAdjacency (renameTransition f) (renamePlace f) $
                    adjacencyT net,
                initialMarking = emap (renamePlace f) $ initialMarking net,
                ghostTransitions = map (renameTransition f) $ ghostTransitions net
            }
        where mapAdjacency f g m = M.mapKeys f (M.map (mapContext g) m)
              mapContext f (pre, post) = (map (first f) pre, map (first f) post)

-- TODO: better cuts, scc, min cut?
constructCut:: PetriNet -> FiringVector -> [Trap] -> Cut
constructCut net _ traps =
            uniqueCut (map trapComponent traps, concatMap trapOutput traps)
        where trapComponent trap =
                  (trap, mpre net trap)
              trapOutput trap = mpost net trap \\ mpre net trap
              uniqueCut (ts, u) = (nubOrd (map (sort *** sort) ts), nubOrd u)

-- TODO: better constructors, only one main constructor
-- TODO: enforce sorted lists
makePetriNet :: String -> [String] -> [String] ->
        [(String, String, Integer)] ->
        [(String, Integer)] -> [String] -> PetriNet
makePetriNet name places transitions arcs initial gs =
        let (adP, adT) = foldl buildMaps (M.empty, M.empty)
                            (filter (\(_,_,w) -> w /= 0) arcs)
        in  PetriNet {
                name = name,
                places = map Place places,
                transitions = map Transition transitions,
                adjacencyP = adP,
                adjacencyT = adT,
                initialMarking = buildVector (map (first Place) initial),
                ghostTransitions = map Transition gs
            }
        where
            buildMaps (mp,mt) (_,_,0) = (mp,mt)
            buildMaps (mp,mt) (l,r,w) | l `elem` places && r `elem` transitions =
                       let mp' = M.insertWith addArc
                                    (Place l) ([],[(Transition r, w)]) mp
                           mt' = M.insertWith addArc
                                    (Transition r) ([(Place l, w)],[]) mt
                       in  (mp',mt')
            buildMaps (mp,mt) (l,r,w) | l `elem` transitions && r `elem` places =
                       let mt' = M.insertWith addArc
                                    (Transition l) ([],[(Place r, w)]) mt
                           mp' = M.insertWith addArc
                                    (Place r) ([(Transition l, w)],[]) mp
                       in  (mp',mt')
            buildMaps _ (l,r,_) = error $ "nodes " ++ l ++ " and " ++ r ++
                                    " both places or transitions"
            addArc (lNew,rNew) (lOld,rOld) = (lNew ++ lOld,rNew ++ rOld)

makePetriNetWith :: String -> [Place] ->
        [(Transition, ([(Place, Integer)], [(Place, Integer)]))] ->
        [(Place, Integer)] -> [Transition] -> PetriNet
makePetriNetWith name places ts initial gs =
        let transitions = map fst ts
            buildMap m (p,c) = M.insertWith addArc p c m
            addArc (lNew,rNew) (lOld,rOld) = (lNew ++ lOld,rNew ++ rOld)
            placeArcs = [ (i,([],[(t,w)])) | (t,(is,_)) <- ts, (i,w) <- is ] ++
                        [ (o,([(t,w)],[])) | (t,(_,os)) <- ts, (o,w) <- os ]
            placeMap = foldl buildMap M.empty placeArcs
        in  PetriNet {
                name = name,
                places = places,
                transitions = transitions,
                adjacencyP = placeMap,
                adjacencyT = M.fromList ts,
                initialMarking = buildVector initial,
                ghostTransitions = gs
            }

makePetriNetWithTrans :: String -> [String] ->
        [(String, [(String, Integer)], [(String, Integer)])] ->
        [(String, Integer)] -> [String] -> PetriNet
makePetriNetWithTrans name places ts initial gs =
        let transitions = [ t | (t,_,_) <- ts ]
            arcs = [ (i,t,w) | (t,is,_) <- ts, (i,w) <- is ] ++
                   [ (t,o,w) | (t,_,os) <- ts, (o,w) <- os ]
        in  makePetriNet name places transitions arcs initial gs
