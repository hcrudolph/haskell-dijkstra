import System.Environment (getArgs)     -- Kommandozeilenparameter lesen
import Prelude hiding (traverse)        -- Standardfunktionen, außer traverse
import Data.Maybe (fromJust)            -- Maybe-Werte "auspacken"
import System.IO (readFile)             -- Dateiinhalte lesen
import Control.Monad.State              -- State-Monade
import CustomTypes                      -- Eigenen Datentypen
import qualified Data.Heap as H         -- MinHeap fuer das Kandidaten-Set
import qualified Data.Map.Strict as M   -- Map (BST) fuer das Visited-Set
import Data.IORef

{--
 - Wrapper fuer alle noetigen Teilfunktionen
 - (Graph erzeugen, Graph durchlaufen, Ergebnis zurückgeben)
 -}
dijkstra :: (Ord a) => [Adj a] -> a -> a -> IO (Int, [Arc a])
dijkstra l s d = do
    let graph = mkGraph l
        start = fromJust $ findNode s graph
    arc <- newIORef $ Arc start 0 s
    ps  <- newIORef $ H.empty
    vs  <- newIORef $ M.empty
    replicateM_ (length graph) $ traverse arc ps vs
    vs' <- readIORef vs
    return (weight $ fromJust $ M.lookup d vs', M.elems vs')

{--
 - Funktion zum Aktualisierung von Kandidaten- und Visited-Set
 - Beide Sets sind durch IORefs repräsentiert.
 -}
traverse :: (Ord a) => IORef (Arc a) -> IORef (H.MinHeap (Arc a)) -> IORef (M.Map (a) (Arc a)) -> IO ()
traverse ioarc iops iovs = do
    arc <- readIORef ioarc
    ps  <- readIORef iops
    vs  <- readIORef iovs
    let loc = map (updateWeight $ weight arc) (adjcnt $ node arc)
        (vs',ps') = updateSets loc (M.insert (label $ node arc) arc vs) ps
        (a',ps'') = getNext vs' ps'
    writeIORef ioarc a'
    writeIORef iops ps''
    writeIORef iovs vs'

{--
- Gibt das erste Element der MinHeap zurück,
- das nicht im Visited-Set enthalten ist. 
-}
getNext :: (Ord a) => M.Map (a) (Arc a) -> H.MinHeap (Arc a) -> (Arc a, H.MinHeap (Arc a))
getNext vs ps
    | M.notMember (label $ node h) vs = (h, tailH ps)
    | otherwise                       = getNext vs (tailH ps)
    where h = fromJust $ H.viewHead ps

{--
- Nicht-monadischer Wrapper für Heap.viewHead
- Gibt Heap.empty bei leerem Tail zurück.
-}
tailH :: (Ord a) => H.MinHeap a -> H.MinHeap a
tailH h = case H.viewTail h of
    Nothing -> H.empty
    Just x  -> x

{--
 - Aktualisiert das Gewicht eines Arcs
 -}
updateWeight :: Int -> Arc a -> Arc a
updateWeight w x = Arc (node x) (w + weight x) (via x)

{--
 - Akualisiert Kandidaten- und Visited-Set fuer eine Liste von Arcs.
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

{- Hauptfunktion -}
main :: IO ()
main = do
    [f,s,d] <- getArgs
    c <- readFile f
    let nlist = read c :: [(String,[(String, Int)])]
    res <- dijkstra nlist s d
    print res
