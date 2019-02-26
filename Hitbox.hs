module Hitbox where

import qualified Data.Foldable as Fold
import qualified Data.Traversable as Trav
import Graphics.Gloss
import Linear

data Hitbox = Hitbox {
    minBox :: V3 Float,
    maxBox :: V3 Float
}

isInside :: Hitbox -> V3 Float -> Bool
isInside hb point = (gtEq point (minBox hb)) && (gtEq (maxBox hb) point)
    where gtEq a b = Fold.and (liftI2 (>=) a b)

getVertices :: Hitbox -> [V3 Float]
getVertices hb = Trav.sequence $ liftI2 pair (minBox hb) (maxBox hb)
    where pair a b = [a, b]

isHit :: Hitbox -> Hitbox -> Bool
isHit hbA hbB = any (isInside hbA) vListB || any (isInside hbB) vListA where
    vListA = getVertices hbA
    vListB = getVertices hbB

--playerCollision ::
--playerCollision pA pB =

render :: Hitbox -> Picture
render hitbox = Translate xPos yPos (rectangleSolid xSize ySize) where
    (V3 xMin yMin zMin) = minBox hitbox
    (V3 xMax yMax zMax) = maxBox hitbox
    xSize               = xMax - xMin
    ySize               = yMax - yMin
    zSize               = zMax - zMin
    xPos                = xMin + (xSize / 2)
    yPos                = yMin + (ySize / 2) + zMin + (zSize / 2)
