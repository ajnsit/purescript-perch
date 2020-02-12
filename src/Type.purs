module Type where

import Data.Show (class Show)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- Foreign
data Elem

type Attribute = { name :: String, val :: String }

data JsEvent = Blur
             | Change
             | Click
             | DblClick
             | Focus
             | KeyPress
             | KeyUp
             | KeyDown
             | Load
             | MouseDown
             | MouseMove
             | MouseOut
             | MouseOver
             | MouseUp
             | Submit
             | Unload
             | Wheel


-- instance FromJSVal Elem where
--   fromJSVal v =
--     do isElem <- js_isInCurrentDOM v
--        return $
--          if isElem
--          then Just (Elem v)
--          else Nothing
--
-- instance ToJSVal Elem where
--   toJSVal (Elem val) = return val

instance showJsEvent :: Show JsEvent where
  show Blur      = "blur"
  show Change    = "change"
  show Click     = "click"
  show DblClick  = "dblclick"
  show Focus     = "focus"
  show KeyDown   = "keydown"
  show KeyPress  = "keypress"
  show KeyUp     = "keyup"
  show Load      = "load"
  show MouseDown = "mousedown"
  show MouseMove = "mousemove"
  show MouseOut  = "mouseout"
  show MouseOver = "mouseover"
  show MouseUp   = "mouseup"
  show Submit    = "submit"
  show Unload    = "unload"
  show Wheel     = "wheel"
--------------------------------------------------------------------------------
