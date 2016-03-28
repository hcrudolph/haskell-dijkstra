import System.Environment (getArgs)     -- Kommandozeilenparameter lesen
import Prelude hiding (traverse)        -- Standardfunktionen, außer traverse
import Data.Maybe (fromJust)            -- Maybe-Werte "auspacken"
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
    res   = traverse (M.elems graph) (Arc start 0 s) H.empty M.empty
    graph = mkGraph l
    start = fromJust $ M.lookup s graph

{--
 - Rekursive Funktion zum Durchlaufen des Graphen
 - Endet, wenn alle Knoten besucht wurden, d.h. die Liste
 - der Knoten (= Graph) leer ist.
 -}
traverse :: (Ord a) => [Node a] -> Arc a -> H.MinHeap (Arc a) -> M.Map (a) (Arc a) -> M.Map (a) (Arc a)
traverse [] arc ps vs = vs
traverse (x:xs) arc ps vs =
    let loc = map (updateWeight $ weight arc) (adjcnt $ node arc)
        (vs', ps') = updateSets loc (M.insert (label $ node arc) arc vs) ps
        (arc', ps'') = getNext vs' ps'
    in traverse xs arc' ps'' vs'

tailH :: (Ord a) => H.MinHeap a -> H.MinHeap a
tailH h = case H.viewTail h of
    Nothing -> H.empty
    Just x  -> x

getNext :: (Ord a) => M.Map (a) (Arc a) -> H.MinHeap (Arc a) -> (Arc a, H.MinHeap (Arc a))
getNext vs ps
    | M.notMember (label $ node h) vs = (h, tailH ps)
    | otherwise                       = getNext vs (fromJust $ H.viewTail ps)
    where h = fromJust $ H.viewHead ps

{--
 - Aktualisiert das Gewicht eines Arcs
 -}
updateWeight :: Int -> Arc a -> Arc a
updateWeight w x = Arc (node x) (w + weight x) (via x)

{--
 - Akualisiert Kandidaten- und Visited Set fuer eine Liste von Arcs.
 - Ist eine Kante nicht im Visited-Set vorhanden, wird sie dem
 - Kandidaten-Set hinzugefuegt. Befindet sie sich bereits im Visited-Set,
 - wird gepueft ob sie eine bessere Alternative zum jeweiligen Knoten
 - darstellt und anschließend hinzugefuegt oder ignoriert.
 -}
updateSets :: (Ord a) => [Arc a] -> M.Map (a) (Arc a) -> H.MinHeap (Arc a)
                         -> (M.Map (a) (Arc a), H.MinHeap (Arc a))
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
mkGraph :: (Ord a) => [Adj a] -> Graph a
mkGraph links = nodeMap where
    nodeMap = M.map mkNode $ M.fromList $ map (\(x,y) -> (x,(x,y))) links
    mkNode (lab, adj) = Node lab (map (mkArc lab) adj)
    mkArc via (label, weight) = Arc node weight via
        where node = fromJust $ M.lookup label nodeMap

{- Hauptfunktion -}
main :: IO ()
main = do
    [f,s,d] <- getArgs
    c <- readFile f
    let nlist = read c :: [(String,[(String, Int)])]
    print $ fst $ dijkstra nlist s d
