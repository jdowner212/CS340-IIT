module Main where

import Graphics.Gloss
import MP2b

main :: IO ()
main = play (InWindow "MP2" (500,500) (10,10))
            white
            1
            (makeWorld (25,25))
            drawWorld
            handleEvents
            (\_ -> nextWorld)

