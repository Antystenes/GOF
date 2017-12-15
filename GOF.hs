{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Grid where

import Prelude hiding (Either(..))
import Data.List (foldl')
import Control.Lens
import Control.Comonad
import Graphics.Gloss hiding (Line)
import Data.Monoid

-- | Line is defined by its central element, list of elements to the left and list of elements to the right (left and right contexts)

data Line a = Line { _left   :: [a]
                   , _center ::  a
                   , _right  :: [a]} deriving Eq

makeLenses ''Line

instance Functor Line where
  fmap f (Line left x right) = Line (f <$> left) (f x) (f <$> right)

instance Foldable Line where
  foldr f acc (Line left x right) = foldl' (flip f) (f x $ foldr f acc right) left

instance Traversable Line where
  traverse f (Line up x down) = Line <$> traverse f up <*> f x <*> traverse f down

instance Show a => Show (Line a) where
  show = concatMap ((++" ").show)

-- | Grid is defined by central Line, upper and lower contexts

data Grid a = Grid { _up    :: [Line a]
                   , _mid   ::  Line a
                   , _down  :: [Line a]} deriving Eq

makeLenses ''Grid

instance Functor Grid where
  fmap f (Grid a b c) = Grid ((fmap . fmap) f a) (fmap f b) ((fmap . fmap) f c)

instance Foldable Grid where
  foldr f acc (Grid up x down) = foldl' (foldr f) (foldr f (foldr (flip . foldr $ f) acc down) x) up

instance Traversable Grid where
  traverse f (Grid up x down) = Grid <$> (traverse . traverse) f up <*> traverse f x <*> (traverse . traverse) f down

instance Show a => Show (Grid a) where
  show (Grid up x down) = upSh ++ show x ++ "\n" ++ doSh
    where
      upSh = foldl' (flip(++)) [] . fmap ((++"\n").show) $ up
      doSh = concatMap ((++"\n").show) down

-- | Functions returning the same line but focused on the first element to the left from previous center

moveLeftLine (Line (l:left) x right) = Line left l (x:right)
moveLeftLine x                       = x

-- | As above but moving focus to the right

moveRightLine (Line left x (r:right)) = Line (x:left) r right
moveRightLine x                       = x

-- | Maps a function that modifies lines on all lines in a grid

limap :: (Line a -> Line a) -> Grid a -> Grid a
limap f (Grid up x down) = Grid (map f up) (f x) (map f down)

moveLeftGrid :: Grid a -> Grid a
moveLeftGrid = limap moveLeftLine

moveRightGrid :: Grid a -> Grid a
moveRightGrid = limap moveRightLine

moveUpGrid :: Grid a -> Grid a
moveUpGrid (Grid (y:up) x down) = Grid up y (x:down)
moveUpGrid g                    = g

moveDownGrid :: Grid a -> Grid a
moveDownGrid (Grid up x (y:down)) = Grid (x:up) y down
moveDownGrid g                    = g

appendToLine :: Line a -> a -> Line a
appendToLine l w = over left (++[w]) l

-- | Returns height of grid

height :: Grid a -> Int
height (Grid up _ down) = length up + length down + 1

-- | Returns width of grid

width :: Grid a -> Int
width (Grid _ x _) = length x

horizontalConfigurations :: Grid a -> Line (Grid a)
horizontalConfigurations g = Line leftC g rightC
  where
    confs f = map ($ g) . tail . scanl (.) id . map (const f)
    leftC   = confs moveLeftGrid . view (mid.left) $ g
    rightC  = confs moveRightGrid . view (mid.right) $ g

-- | Function that returns all possible configurations of a grid - all elements of grid with their contexts. This function is definition of comonadic 'duplicate' method.

gridConfigurations :: Grid a -> Grid (Grid a)
gridConfigurations a@(Grid u _ d) = Grid up center down
  where
    center  = horizontalConfigurations a
    up      = map horizontalConfigurations uConfs
    down    = map horizontalConfigurations dConfs
    confs f = map ($ a) . tail . scanl (.) id . map (const f)
    uConfs  = confs moveUpGrid u
    dConfs  = confs moveDownGrid d

instance Comonad Grid where
  extract    = view (mid.center)
  duplicate  = gridConfigurations

class Drawable a where
  draw :: a -> Picture

-- | Datatype for state of Cell

data Cell = Alive | Dead deriving Eq

instance Show Cell where
  show Alive = "O"
  show Dead = " "

sampleLine = Line (concat . replicate 34 $ [Alive,Dead]) Dead (concat . replicate 10 $ [Alive,Dead])

sampleGrid = Grid (replicate 6 sampleLine) sampleLine (replicate 6 sampleLine)

lowerNeighb,upperNeighb,horizNeighb :: Grid a -> [a]
upperNeighb = toListOf $ up . ix 0 . (left . ix 0 <> center <> right . ix 0)
lowerNeighb = toListOf $ down . ix 0 . (left . ix 0 <> center <> right . ix 0)
horizNeighb = toListOf $ mid  . (left . ix 0 <> right . ix 0)

-- | Helper function returning all occurences of value in list
counts :: Eq a => a -> [a] -> Int
counts = (length.).filter.(==)

-- | Returns list of pairs of all unique values and numbers of their occurences in a list
countAll :: Eq a => [a] -> [(a,Int)]
countAll = (>>=flip(,)).flip counts >>= map

-- | Returns list of all neighbours according to Moore's neighbourhood
mooresNeighb :: Grid a -> [a]
mooresNeighb = upperNeighb <> lowerNeighb <> horizNeighb

-- | Function that given a 'Cell' with contexts returns type of cell that should be at given location.
conwayUpdate :: Grid Cell -> Cell
conwayUpdate g =
  let aliveCounts = counts Alive . mooresNeighb $ g
  in case extract g of
    Dead  -> if aliveCounts == 3 then Alive else Dead
    Alive -> if aliveCounts > 3 || 2 > aliveCounts
      then Dead
      else Alive

instance Drawable Cell where
  draw Alive = Color blue $ circleSolid 5
  draw Dead  = Pictures []

instance Drawable a => Drawable (Line a) where
  draw (Line left x right) = Pictures $ draw x : leftD++rightD
    where
      leftD = zipWith update [-10,-20..] . map draw $ left
      rightD= zipWith update [10,20..] . map draw $ right
      update _ (Pictures []) = Pictures []
      update p y = Translate p 0 y

instance Drawable a => Drawable (Grid a) where
  draw (Grid up cent down) = Pictures $ draw cent : upD ++ downD
    where
      upD   = zipWith (Translate 0) [10,20..] . map draw $ up
      downD = zipWith (Translate 0) [-10,-20..] . map draw $ down

-- | Function rendering sample Conway's game of life using gloss library

simulationGOF :: IO ()
simulationGOF = simulate (InWindow "Conway" (600, 400) (0,0)) white 3 sampleGrid draw (\_ _ m -> m =>> conwayUpdate)
