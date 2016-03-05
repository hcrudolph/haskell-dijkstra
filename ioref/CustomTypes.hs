module CustomTypes where
import Data.Function (on)
import qualified Data.Heap as H
import qualified Data.Map.Strict as M

----------------------------------------------

data Node a = Node
  { label :: a
  , adjcnt :: [Arc a]
  }

data Arc a = Arc 
  { node    :: Node a
  , weight  :: Int
  , via     :: a
  } deriving (Show)

type Graph a  = [Node a]
type Adj a    = (a, [(a, Int)])
type GrValue a = M.Map (a) (Arc a)
type GrState a = (Arc a, Maybe (H.MinHeap (Arc a)), M.Map (a) (Arc a))
type DState a = (a, Int)

----------------------------------------------

instance (Show a) => Show (Node a) where
  show g = show $ label g

instance (Eq a) => Eq (Node a) where
  x == y = (label x) == (label y)

instance (Eq a) => Eq (Arc a) where
  x == y = node x == node y

instance (Ord a) => Ord (Arc a) where
  compare = compare `on` weight
