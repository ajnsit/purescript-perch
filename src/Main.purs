module Main where

import API (getBody)
import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Functor (void)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Perch (Perch, addEvent, atr, build, div, forElems_, h3, id, p, style, this, (!))

main :: Effect Unit
main = do
  documentBody <- getBody
  void $ build dom documentBody

dom :: Perch
dom = do
  h3 $ "Static DOM"
  div ! id "wrap" $ do
    div ! id "content" $ do
      p "Hello World!"
      div ! id "footer" $ "Happy Purescript!"
  h3 $ "Interactivity!"
  div ! style "font-weight: bold" $ "Click the text below"
  div ! style "margin-left: 20px" $ do
     div ! atr "class" "changeable" $ "Changeable"
     div "Static"
     div ! atr "class" "changeable" $ "Changeable"
     addEvent this "click" $ mkEffectFn1 \_ -> do
       forElems_ ".changeable" $
         this ! style "color:red"
