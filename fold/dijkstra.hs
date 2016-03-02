{-# LANGUAGE BangPatterns #-}
import System.Environment (getArgs)     -- Kommandozeilenparameter lesen
import Prelude hiding (traverse)        -- Standardfunktionen, außer traverse
import Data.Maybe (fromJust)            -- Maybe-Werte "auspacken"
import Data.List (foldl')
import System.IO (readFile)             -- Dateiinhalte lesen
import CustomTypes                      -- Eigenen Datentypen
import qualified Data.Heap as H         -- MinHeap fuer das Kandidaten-Set
import qualified Data.Map.Strict as M   -- Map (BST) fuer das Visited-Set


{--
 - Wrapper fuer alle noetigen Teilfunktionen
 - (Graph erzeugen, Graph durchlaufen, Ergebnis zurückgeben)
 -}
dijkstra :: (Ord a) => [Adj a] -> a -> a -> (Int, [Arc a])
dijkstra l s d = (dist, M.elems res) where
    dist  = weight $ fromJust $ M.lookup d res
    res   = fst $ foldl' trav (M.singleton s init, H.fromList $ adjcnt start) $ tail graph
    graph = mkGraph l
    init  = Arc start 0 s
    start = fromJust $ findNode s graph

trav :: (Ord a) => (M.Map a (Arc a), H.MinHeap (Arc a)) -> b -> (M.Map a (Arc a), H.MinHeap (Arc a))
trav (v,p) _ = (vs,ps)
    where arc = getNextArc v p
          v'  = M.insert (label $ node arc) arc v
          loc = map (updateWeight $ weight arc) (adjcnt $ node arc)
          (vs,ps) = updateSets loc v' p


getNextArc :: (Ord a) => M.Map a (Arc a) -> H.MinHeap (Arc a) -> Arc a
getNextArc v p
    | M.notMember (label $ node h) v = h
    | otherwise                      = getNextArc v (tailH p)
    where h = fromJust $ H.viewHead p


--getNext :: (M.Map a (Arc a), H.MinHeap (Arc a)) -> (M.Map a (Arc a), H.MinHeap (Arc a))
--getNext vs ps
--    | M.notMember (label $ node h) vs = (vs,ps)
--    | otherwise                       = getNext (vs, tailH ps)
--    where h = fromJust $ H.viewHead ps

tailH :: (Ord a) => H.MinHeap a -> H.MinHeap a
tailH x = case H.viewTail x of
    Nothing -> H.empty
    Just x  -> x

headH :: (Ord a) => H.MinHeap a -> H.MinHeap a
headH x = case H.viewTail x of
    Nothing -> H.empty
    Just x  -> x

{--
 - Aktualisiert das Gewicht eines Arcs
 -}
updateWeight :: (Ord a) => Int -> Arc a -> Arc a
updateWeight w x = Arc (node x) (w + weight x) (via x)

{--
 - Akualisiert Kandidaten- und Visited Set fuer eine Liste von Arcs.
 - Ist eine Kante nicht im Visited-Set vorhanden, wird sie dem
 - Kandidaten-Set hinzugefuegt. Befindet sie sich bereits im Visited-Set,
 - wird gepueft ob sie eine bessere Alternative zum jeweiligen Knoten
 - darstellt und anschließend hinzugefuegt oder ignoriert.
 -}
updateSets :: (Ord a) => [Arc a] -> M.Map (a) (Arc a) -> H.MinHeap (Arc a) -> (M.Map (a) (Arc a), H.MinHeap (Arc a))
updateSets [] v p = (v,p)
updateSets (c:cs) v p
    | M.notMember (label $ node c) v = updateSets cs v (H.insert c p)
    | better c v                     = updateSets cs (M.insert (label $ node c) c v) p
    | otherwise                      = updateSets cs v p

{--
 - Prueft ob ein gegebener Arc eine bessere Alternative zu einem
 - Knoten darstellt, d.h. ob sein Gewicht niedriger ist.
 -}
better :: (Ord a) => Arc a -> M.Map (a) (Arc a) -> Bool
better c v = case n of
    Nothing -> False
    Just x  -> weight c < weight x
    where n = M.lookup (label $ node c) v

{--
 - Erstellt einen (moeglicherweise zyklischen) Graphen anhand
 - einer gegebenen Adjazenzliste.
 -}
mkGraph :: (Eq a) => [Adj a] -> Graph a
mkGraph links = map snd nodeList where
    mkNode (lab, adj) = (lab, Node lab (map (lookupNode lab) adj))
    nodeList = map mkNode links
    lookupNode via (label, weight) = Arc node weight via
        where node = fromJust $ lookup label nodeList

{--
 - Durchsucht eine Liste aus Knoten mithilfe eines gegebenen
 - Knoten-Labels.
 -}
findNode :: (Eq a) => a -> Graph a -> Maybe (Node a)
findNode y []     = Nothing
findNode y (x:xs) = if y == label x then Just x else findNode y xs

nlist = [('a',[('b',2),('c',1)]),('b',[('a',2),('c',2)]),('c',[('a',1),('b',2)])]

{- Hauptfunktion -}
main = do
    f <- getArgs
    c <- readFile (f !! 0)
    let nlist = read c :: [(String,[(String, Int)])]
    print $ dijkstra nlist (f!!1) (f!!2)
