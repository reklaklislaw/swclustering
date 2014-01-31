import System.IO
import System.Environment
import Control.Monad                
import Control.Parallel.Strategies
import Data.Text(splitOn, pack, unpack)
import Data.List

data Window = Window {l :: Float, t :: Float,
                      r :: Float, b :: Float}

--data Cluster = Cluster {Window,
--                        size :: Int}

deg2rad :: Float -> Float
deg2rad x = 2 * pi * x / 360 



main = do
  args <- getArgs
  let len = length args
  when (len /=  7) $ error "invalid number of arguments"
  let in_file = args !! 0
      out_file = args !! 1
      window_width = read (args !! 2) :: Int
      window_height = read (args !! 3) :: Int
      skew_angle = read (args !! 4) :: Float
      mx = read (args !! 5) :: Int
      my = read (args !! 6) :: Int
  
  corners <- getCornersFromFile skew_angle mx my in_file
  let clusters = clusterCorners window_width window_height corners
      dimensions = getClusterDimensions clusters
  dimensionsToFile out_file dimensions
  --print clusters
  --print (show (l clusters) ++" "++ show (t clusters) ++" "++ 
  --       show (r clusters) ++" "++ show (b clusters))
  print "finished"
  
  

dimensionsToFile :: String -> [[Float]] -> IO()
dimensionsToFile out_file dimensions = do
  let dstrings = map (\d -> ((show $ d!!0) ++" "++ (show $ d!!1) ++" "++ 
                            (show $ d!!2) ++" "++ (show $ d!!3) ++"\n")) dimensions
  writeFile out_file $ unwords dstrings



getCornersFromFile :: Float -> Int -> Int -> String -> IO [[Float]]
getCornersFromFile skew_angle mx my in_file = do
  contents <- readFile in_file  
  return (parseCorners skew_angle mx my (lines contents))



parseCorners :: Float -> Int -> Int -> [String] -> [[Float]]
parseCorners skew_angle mx my lns = 
  map ds $ map (\string -> map ((\x -> read x::Int) . unpack) $ 
                           splitOn (pack " ") (pack string) 
               ) lns
  where ds = \corner -> deskew skew_angle mx my corner
  


deskew :: Float -> Int -> Int -> [Int] -> [Float]
deskew skew_angle mx my corner = 
  if skew_angle == 0.0 
  then [fromIntegral (corner !! 0) :: Float, 
        fromIntegral (corner !! 1) :: Float]
  else
    [(( (x - mx') * cos r ) - 
      ( (y - my') * sin r )) + mx' ,
     (( (x - mx') * sin r ) + 
      ( (y - my') * cos r )) + my']
    where 
      x = fromIntegral (corner !! 0) :: Float
      y = fromIntegral (corner !! 1) :: Float
      mx' = fromIntegral (mx) :: Float
      my' = fromIntegral (my) :: Float
      r = deg2rad skew_angle 
      

        
getClusterDimensions :: [[[Float]]] -> [[Float]]
getClusterDimensions clusters = dimensions
  where
    dimensions = map (\c -> boundary c) 
                 clusters `using` parList rdeepseq
      

                 
boundary :: [[Float]] -> [Float]
boundary cluster = [l,t,r,b]
  where
    xs = map (\c -> c!!0) cluster 
    ys = map (\c -> c!!1) cluster
    l = minimum xs
    r = maximum xs
    t = minimum ys
    b = maximum ys



clusterCorners :: Int -> Int -> [[Float]] -> [[[Float]]]
clusterCorners window_width window_height corners = 
  clusters
    where
      assigned = []
      unassigned = deleteFirstsBy (==) corners assigned
      clusters = map (\c -> 
                       slide window_width window_height c unassigned) 
                 unassigned `using` parListChunk 4 rdeepseq
                 
      
        

slide :: Int -> Int -> [Float] -> [[Float]] -> [[Float]]
slide window_width window_height corner unassigned = 
  clust
    where
      window = getWindow window_width window_height corner
      clust = filter (\c -> containedBy window c) unassigned



containedBy :: Window -> [Float] -> Bool
containedBy window corner = 
  if (x >= wl && x <= wr) && (y >= wt && y <= wb)
  then True
  else False
    where
      x = corner !! 0
      y = corner !! 1
      wl = l window
      wr = r window
      wt = t window
      wb = b window



getWindow :: Int -> Int -> [Float] -> Window
getWindow width height corner = 
  Window {l = x, t = y, r = r', b = b'}
    where
      w = fromIntegral(width) :: Float
      h = fromIntegral(height) :: Float
      x = (corner !! 0) - w/2
      y = (corner !! 1) - h/2
      r' = x + w/2
      b' = y + h/2
