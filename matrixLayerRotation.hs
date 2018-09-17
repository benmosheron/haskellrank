import Data.List(sortBy,groupBy)
import Data.Vector((//),(!),fromList)
import qualified Data.Vector as Vec

type V = Vec.Vector Int
type M = Vec.Vector V

-- Unwrap the outermost ring
unwrap :: M -> (V,M)
unwrap m = top $ right $ bottom $ left m

stop :: M -> Bool
stop m = m == Vec.empty || Vec.head m == Vec.empty

left :: M -> (V,M)
left m = (Vec.map (! 0) m, Vec.map Vec.tail m)

bottom :: (V,M) -> (V,M)
bottom (v,m) 
  | stop m    = (v,m)
  | otherwise = (Vec.concat [v,Vec.last m], Vec.init m)

right :: (V,M) -> (V,M)
right (v,m) 
  | stop m    = (v,m)
  | otherwise = (Vec.concat [v,Vec.reverse $ Vec.map Vec.last m], Vec.map Vec.init m)

top :: (V,M) -> (V,M)
top (v,m) 
  | stop m    = (v,m)
  | otherwise = (Vec.concat [v, Vec.reverse $ m ! 0], Vec.tail m)

-- Convert matrix to anticlockwise ring vector
rings :: M -> M
rings m = ringsRec Vec.empty m

ringsRec :: M -> M -> M
ringsRec rs m 
  | stop m    = rs
  | otherwise = ringsRec (Vec.snoc rs r) m'
    where (r,m') = unwrap m

-- Convert vector of rings back into matrix
unrings :: Int -> Int -> M -> M
unrings nRows nCols mr = snd $ unringsRec mr 0 (Vec.replicate nRows (Vec.replicate nCols 0))

--We will build up our matrix m' ring by ring, working inwards. m' will initially be all zeros.
unringsRec :: M -> Int -> M -> (M,M)
unringsRec rings layer m'
  | rings == Vec.empty = (Vec.empty,m')
  | otherwise = unringsRec (Vec.tail rings) (layer + 1) $ wrap (Vec.head rings) layer m'

-- Wrap vector V around layer 'layer' of m'
wrap :: V -> Int -> M -> M
wrap ring layer m' = replace m' (gatherRowReplacements $ Vec.toList $ Vec.concat $ [
  Vec.imap (\i e -> (layer + i,                     (layer,e))) leftCol,
  Vec.imap (\i e -> (y - 1 - layer,         (i + 1 + layer,e))) bottomRow,
  Vec.imap (\i e -> (y - i - 2 - layer,     (x - 1 - layer,e))) rightCol,
  Vec.imap (\i e -> (layer,             (x - i - 2 - layer,e))) topRow
  ] )
  where x = length (Vec.head m')
        y = length m'
        x' = x - (layer * 2)
        y' = y - (layer * 2)
        leftCol = Vec.slice 0 y' ring
        bottomRow = Vec.slice y' (x' - 1) ring
        rightCol = Vec.slice (y' + x' - 1) (y' - 1) ring
        topRow = Vec.slice (y' + x' - 1 + y' - 1) (x' - 2) ring


replace :: M -> [(Int,[(Int,Int)])] -> M
replace m' changes = m' // (map (\rowChanges -> change rowChanges m') changes)

gatherRowReplacements :: [(Int,(Int,Int))] -> [(Int,[(Int,Int)])]
gatherRowReplacements changes = map mf $ groupBy gb $ sortBy sb changes
  where gb = \(c1,_) (c2,_) -> c1 == c2
        sb = \(c1,_) (c2,_) -> compare c1 c2
        mf = \a -> (fst $ head a, map (\(c,u) -> u) a)

change :: (Int,[(Int,Int)]) -> M -> (Int,V)
change (rowIndex,rowChanges) m'  = (rowIndex, (m' ! rowIndex) // rowChanges)

rotateRing :: Int -> V -> V
rotateRing d r = r

rotate :: (Int,Int,Int, M) -> M
rotate (nRows,nCols,d,m) = unrings nRows nCols $ Vec.map (rotateRing d) $ rings m

-- Parse our input data, extracting the rotation and matrix
args :: [[Int]] -> (Int,Int,Int,M)
args ((nRows:nCols:d:[]):ls) = (nRows, nCols, d, fromList $ map fromList ls)

main :: IO()
main = interact $ show . rotate . args . map (map (read :: String -> Int) . words) . lines