import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Linear hiding (trace)

import qualified Hitbox as H
import qualified Player as P

-- CONSTANTS
c_SCREEN_TITLE  = "Title"
c_SCREEN_SIZE   = (640, 480)
c_SCREEN_POS    = (10, 10)

c_UP            = 'w'
c_LEFT          = 'a'
c_DOWN          = 's'
c_RIGHT         = 'd'
c_JUMP          = 'j'

c_P_SIZE        = (V3 30 30 30)
c_P_INIT_POS    = (V3 200 180 halfSize)
    where (V3 _ _ halfSize) = fmap (/ 2) c_P_SIZE

main = do
    play display bgColor fps player draw handler step where
        floor = H.Hitbox (V3 xMin yMin 0) (V3 xMax yMax 0) where
            xMin    = (-1) * xMax
            yMin    = (-1) * yMax
            xMax   = ((fromIntegral . fst) (c_SCREEN_SIZE) :: Float) / 2
            yMax   = ((fromIntegral . snd) (c_SCREEN_SIZE) :: Float) / 2
-- TODO doesnt work -- where (maxWidth, maxLength) = fmap fromIntegral c_SCREEN_SIZE
        display = (InWindow title size pos) where
            title   = c_SCREEN_TITLE
            size    = c_SCREEN_SIZE
            pos     = c_SCREEN_POS
        bgColor = white
        fps     = 60
        player  = P.Player pos pad size keyDown vel hitbox where
            pos     = c_P_INIT_POS
            pad     = (V3 0 0 0)
            size    = c_P_SIZE
            keyDown = Set.empty
            vel     = (V3 0 0 0)
            hitbox  = P.updateHb player (P.pos player)
        draw player = pictures [
            H.render (P.hitbox player),
            Translate xPos y (color yellow (Circle 15))
            ] where
                y = yPos + zPos
                (V3 xPos yPos zPos) = P.pos player
        handler event@(EventKey (Char key) Down _ _) player =
            player { P.keyDown = Set.insert key $ P.keyDown player }
        handler event@(EventKey (Char key) Up   _ _) player =
            player { P.keyDown = Set.delete key $ P.keyDown player }
        handler event player = player
        step time player = trace (blabla) update where
            update = player {
                P.pad       = newPad,
                P.pos       = newPos,
                P.vel       = newVel,
                P.hitbox    = newHitbox
                } where
                    newPad      = P.updatePad player keys newVel
                        where keys = [c_UP, c_LEFT, c_DOWN, c_RIGHT, c_JUMP]
                    newVel      = P.updateVel player newPad c_JUMP floor
                    newPos      = P.updatePos player newVel
                    newHitbox   = P.updateHb player newPos
-- trace for testing
            blabla = show (H.isHit (P.hitbox player) floor)
