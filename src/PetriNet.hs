{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PetriNet
    (PetriNet,Place(..),Transition(..),Marking,tokens,buildMarking,
     marked,lmarked,makeMarking,
     renamePlace,renameTransition,renamePetriNetPlacesAndTransitions,
     name,showNetName,places,transitions,initial,initialMarking,
     pre,lpre,post,lpost,initials,context,ghostTransitions,
     makePetriNet,makePetriNetWithTrans,makePetriNetWith,Trap)
where

import qualified Data.Map as M
import Data.List (intercalate)
import Control.Arrow (first)

newtype Place = Place String deriving (Ord,Eq)
newtype Transition = Transition String deriving (Ord,Eq)

instance Show Place where
        show (Place p) = p
instance Show Transition where
        show (Transition t) = t

type ContextMap a b = M.Map a ([(b, Integer)],[(b, Integer)])

class Nodes a b | a -> b where
        pre :: (Ord a) => PetriNet -> a -> [b]
        pre net = map fst . fst . context net
        post :: (Ord a) => PetriNet -> a -> [b]
        post net = map fst . snd . context net
        lpre :: (Ord a) => PetriNet -> a -> [(b, Integer)]
        lpre net = fst . context net
        lpost :: (Ord a) => PetriNet -> a -> [(b, Integer)]
        lpost net = snd . context net
        context :: (Ord a) => PetriNet -> a -> ([(b, Integer)], [(b, Integer)])
        context net x = M.findWithDefault ([],[]) x (contextMap net)
        contextMap :: PetriNet -> ContextMap a b

instance Nodes Place Transition where
        contextMap = adjacencyP
instance Nodes Transition Place where
        contextMap = adjacencyT

newtype Marking = Marking { getMarking :: M.Map Place Integer }

instance Show Marking where
        show (Marking m) =
                "[" ++ intercalate "," (map showPlaceMarking (M.toList m)) ++ "]"
            where showPlaceMarking (n,i) =
                    show n ++ (if i /= 1 then "(" ++ show i ++ ")" else "")

tokens :: Marking -> Place -> Integer
tokens m p = M.findWithDefault 0 p (getMarking m)

buildMarking :: [(Place, Integer)] -> Marking
buildMarking = makeMarking . M.fromList

makeMarking :: M.Map Place Integer -> Marking
makeMarking = Marking . M.filter (/=0)

marked :: Marking -> [Place]
marked = M.keys . getMarking

lmarked :: Marking -> [(Place,Integer)]
lmarked = M.toList . getMarking

type Trap = [Place]

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
initial net = tokens (initialMarking net)

initials :: PetriNet -> [(Place,Integer)]
initials net = M.toList (getMarking (initialMarking net))

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
                initialMarking = Marking $
                    M.mapKeys (renamePlace f) $ getMarking $ initialMarking net,
                ghostTransitions = map (renameTransition f) $ ghostTransitions net
            }
        where mapAdjacency f g m = M.mapKeys f (M.map (mapContext g) m)
              mapContext f (pre, post) = (map (first f) pre, map (first f) post)

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
                initialMarking = buildMarking (map (first Place) initial),
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

-- TODO: better constructors
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
                initialMarking = buildMarking initial,
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
