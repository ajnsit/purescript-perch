module FFI where

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)

data Elem
type Callback1 = EffectFn1 Elem Unit

foreign import js_document :: Effect Elem

foreign import js_documentBody :: Effect Elem

foreign import js_getElementById :: String -> Effect Elem

foreign import js_querySelectorAll :: String -> Effect (Array Elem)

foreign import js_documentCreateNode :: String -> Effect Elem

foreign import js_documentCreateNodeNS ::  String -> String -> Effect Elem

foreign import js_createTextNode :: String -> Effect Elem

-- | Check if object is an HTML Element of current DOM.  Works with HTML
-- elements and text nodes.
-- http://stackoverflow.com/a/20476546/1749901
foreign import js_isInCurrentDOM :: Elem -> Effect Boolean


foreign import js_parentNode :: Elem -> Effect Elem

foreign import js_appendChild :: Elem -> Elem -> Effect Unit

foreign import js_replaceChild :: Elem -> Elem -> Elem -> Effect Unit

foreign import js_removeChild :: Elem -> Elem -> Effect Unit

foreign import js_clearChildren :: Elem -> Effect Unit

foreign import js_setAttribute :: Elem -> String -> String -> Effect Unit

foreign import js_setInnerHtml :: Elem -> String -> Effect Unit


foreign import js_addEventListener :: Elem -> String -> Callback1 -> Effect Unit

-- | Remove event listener from element.
foreign import js_removeEventListener :: Elem -> String -> Callback1 -> Effect Unit
