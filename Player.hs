module Player where

import Data.Set as Set hiding (size)
import Linear

import Hitbox as H

data Player = Player {
    pos     :: V3 Float,
    pad     :: V3 Int,
    size    :: V3 Float,
    keyDown :: Set.Set Char,
    vel     :: V3 Float,
    hitbox  :: H.Hitbox
}

c_MAX_JUMP_VEL  = 15.0
c_DECELERATION  = 1.0
c_WALK_VEL      = 1.5

isDown :: Player -> Char -> Int
isDown player key =
    fromEnum $ Set.member key $ keyDown player

jump :: Player -> V3 Int -> Hitbox -> Float
jump player newPad floor
    | onFloor       && not tryJump  = 0
    | onFloor       && tryJump      = c_MAX_JUMP_VEL
    | otherwise                     = zVel - c_DECELERATION
    where
        onFloor         = H.isHit (hitbox player) floor
        tryJump         = (toEnum zPad) :: Bool
            where (V3 _ _ zPad) = newPad
        (V3 _ _ zVel)   = vel player

updatePad :: Player -> [Char] -> t -> V3 Int
updatePad player keys newVel =
    (V3 newXPad newYPad newZPad) where
        newXPad = pressed right - pressed left
        newYPad = pressed up    - pressed down
        newZPad = pressed jump
        pressed = isDown player
        [up, left, down, right, jump] = keys

updateVel :: Player -> V3 Int -> t -> Hitbox -> V3 Float
updateVel player newPad jumpKey floor =
    (V3 newXVel newYVel newZVel) where
        (V3 newXVel newYVel _)  = fmap fromIntegral newPad * c_WALK_VEL
        newZVel                 = jump player newPad floor

updatePos :: Player -> V3 Float -> V3 Float
updatePos player newVel =
    pos player + newVel

updateHb :: Player -> V3 Float -> Hitbox
updateHb player newPos =
    H.Hitbox minPos maxPos where
        minPos      = newPos - halfSize
        maxPos      = newPos + halfSize
        halfSize    = fmap (/ 2) (size player)
