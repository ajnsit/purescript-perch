module API where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Unit (Unit)
import Effect (Effect)
import FFI (Callback1, Elem, js_addEventListener, js_appendChild, js_clearChildren, js_createTextNode, js_document, js_documentBody, js_documentCreateNode, js_documentCreateNodeNS, js_getElementById, js_parentNode, js_querySelectorAll, js_removeChild, js_removeEventListener, js_replaceChild, js_setAttribute, js_setInnerHtml)

getDocument :: Effect Elem
getDocument = js_document

getBody :: Effect Elem
getBody = js_documentBody

newElem :: String -> Effect Elem
newElem = js_documentCreateNode

newElemNS :: String -> String -> Effect Elem
newElemNS = js_documentCreateNodeNS

newTextElem :: String -> Effect Elem
newTextElem = js_createTextNode

parent :: Elem -> Effect Elem
parent = js_parentNode

-- | Appends one element to another.
addChild :: Elem -- ^ child element to append
         -> Elem -- ^ parent element
         -> Effect Unit
addChild c p = js_appendChild p c

-- | Remove child from parent.
removeChild :: Elem -- ^ child to remove
            -> Elem -- ^ parent node
            -> Effect Unit
removeChild c p = js_removeChild p c

clearChildren :: Elem -> Effect Unit
clearChildren = js_clearChildren

replace :: Elem -> Elem -> Effect Elem
replace oe n = do
  par <- parent oe
  js_replaceChild par oe n
  pure n

-- TODO
type PropId = String

setAttr :: Elem -> PropId -> String -> Effect Unit
setAttr = js_setAttribute

setInnerHTML :: Elem -> String -> Effect Unit
setInnerHTML = js_setInnerHtml

getElemById :: String -> Effect Elem
getElemById = js_getElementById

queryAll :: String -> Effect (Array Elem)
queryAll = js_querySelectorAll

-- | Attach an event listener to element.
--
-- Returns an action removing listener, though you still have to release
-- callback manually.
--
-- If you are sure that you do not want to remove handler consider using
-- 'onEvent''.
onEvent :: Elem -> String -> Callback1 -> Effect (Effect Unit)
onEvent el et cb =
  do js_addEventListener el et cb
     pure $ removeEvent el et cb

-- | Attach endless event listener to element.
--
-- Use this function to attach event handlers which supposed not to be removed
-- during application run.
onEvent' :: Elem -> String -> Callback1 -> Effect Unit
onEvent' = js_addEventListener

-- | Remove attached event listener.
--
-- Normally you can use action returned by 'onEvent' to detach event listener,
-- however you can also use this function directly.
removeEvent :: Elem -> String -> Callback1 -> Effect Unit
removeEvent = js_removeEventListener
