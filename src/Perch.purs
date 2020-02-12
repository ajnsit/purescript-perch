module Perch where

import API (addChild, clearChildren, getElemById, newElem, newElemNS, newTextElem, onEvent, onEvent', parent, queryAll, removeChild, removeEvent, replace, setAttr, setInnerHTML)
import Control.Alternative (pure)
import Control.Applicative (class Applicative, (*>), (<$))
import Control.Apply (class Apply)
import Control.Bind (class Bind, bind, discard)
import Control.Category ((<<<))
import Control.Monad (class Monad)
import Data.Function (const, flip, ($))
import Data.Functor (class Functor)
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup (class Semigroup, append)
import Data.Show (class Show, show)
import Data.Traversable (traverse_)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import FFI (Callback1, Elem)
import Unsafe.Coerce (unsafeCoerce)

-- temp
type Attribute = { name :: String, val :: String }

data PerchM a = PerchM (Elem -> Effect Elem)
type Perch = PerchM Unit

build :: forall a. PerchM a -> Elem -> Effect Elem
build (PerchM x) = x

perch :: forall a. (Elem -> Effect Elem) -> PerchM a
perch = PerchM

class ToPerch a where
  toPerch :: a -> Perch

class Attributable h where
  attr :: h -> Attribute -> h

infixl 4 attr as !

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
instance semigroupPerch :: Semigroup (PerchM a) where
  append mx my = perch <<< withPerch $ \e -> do
    _ <- build mx e
    build my e

instance monoidPerch :: Monoid (PerchM a) where
  mempty = perch pure

instance functorPerch :: Functor PerchM  where
  map _ x = unsafeCoerce x

instance applyPerch :: Apply PerchM where
  apply f x = unsafeCoerce (append (unsafeCoerce f) x)
  -- apply f x = unsafeCoerce $ mappend (unsafeCoerce f) x

instance applicativePerch :: Applicative PerchM where
  pure _ = mempty

instance bindPerch :: Bind PerchM where
  bind m f = append (unsafeCoerce m) (f dontUseThisArg)
    where
      dontUseThisArg = unsafeCoerce "bind (>>=) invocation in the Perch monad creating DOM elements"

instance monadPerch :: Monad PerchM

instance monadEffectPerch :: MonadEffect PerchM where
  liftEffect io = perch <<< withPerch $ const io

instance atributablePerch :: Attributable (PerchM Unit) where
  attr tag {name, val} = perch $ withPerchBuild tag (\t -> setAttr t name val)

instance attributableFnPerch :: ToPerch a => Attributable (a -> PerchM Unit) where
 attr pe {name, val} = \e -> pe e `attr` {name,  val}

instance toPerchString :: ToPerch String where
  toPerch s = perch $ \x ->
    do e <- newTextElem s
       addChild e x
       pure e
else instance toPerchPerch :: ToPerch (PerchM a) where
  toPerch = unsafeCoerce
else instance toPerchA :: Show a => ToPerch a where
  toPerch = toPerch <<< show

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- * DOM Tree Building

-- | Create a new elemet
nelem :: String -> Perch
nelem s = perch $ \x ->
  do e <- newElem s
     addChild e x
     pure e

-- | Create a new element with a namespace`
nelemNS :: String -> String -> Perch
nelemNS ns s=  perch $ \x ->
  do e <- newElemNS ns s
     addChild e x
     pure e

-- | Build an element as child of another one.  Child element becomes new
-- continuation for monadic expression.
child :: forall a. ToPerch a
      => Perch -- ^ parent
      -> a     -- ^ child
      -> Perch
child me ch = perch <<< withPerchBuild me $ build (toPerch ch)

setHtml :: Perch -> String -> Perch
setHtml me text = perch <<< withPerchBuild me $ flip setInnerHTML text

-- | Build perch and attach an event handler to its element.
--
-- Event handler should be an Effect action wrapped by GHCJS' 'Callback' taking one
-- argument, that is an actual JavaScript event object baked in @JSVal@.
addEvent :: Perch -> String -> Callback1 -> Perch
addEvent pe event action = perch <<< withPerchBuild pe $ \e ->
  onEvent e event action

-- | Build perch and attach an event handler to its element.  Use this function
-- only when you are sure that you won't detach handler during application run.
addEvent' :: Perch -> String -> Callback1 -> Perch
addEvent' pe event action = perch <<< withPerchBuild pe $ \e ->
  onEvent' e event action

-- | Build perch and remove an event handler from it.
--
-- Note, you still have to release callback manually.
remEvent :: Perch -> String -> Callback1 -> Perch
remEvent pe event action = perch <<< withPerchBuild pe $ \e ->
  removeEvent e event action

-- ** Leaf DOM Nodes
area :: Perch
area = nelem "area"
base :: Perch
base = nelem "base"
br :: Perch
br = nelem "br"
col :: Perch
col = nelem "col"
embed :: Perch
embed = nelem "embed"
hr :: Perch
hr = nelem "hr"
img :: Perch
img = nelem "img"
input :: Perch
input = nelem "input"
keygen :: Perch
keygen = nelem "keygen"
link :: Perch
link = nelem "link"
menuitem :: Perch
menuitem = nelem "menuitem"
meta :: Perch
meta = nelem "meta"
param :: Perch
param = nelem "param"
source :: Perch
source = nelem "source"
track :: Perch
track = nelem "track"
wbr :: Perch
wbr = nelem "wbr"

-- ** Parent DOM Nodes

a :: forall a. ToPerch a => a -> Perch
a cont = nelem "a" `child` cont
abbr :: forall a. ToPerch a => a -> Perch
abbr cont = nelem "abbr" `child` cont
address :: forall a. ToPerch a => a -> Perch
address cont = nelem "address" `child` cont
article :: forall a. ToPerch a => a -> Perch
article cont = nelem "article" `child` cont
aside :: forall a. ToPerch a => a -> Perch
aside cont = nelem "aside" `child` cont
audio :: forall a. ToPerch a => a -> Perch
audio cont = nelem "audio" `child` cont
b :: forall a. ToPerch a => a -> Perch
b cont = nelem "b" `child` cont
bdo :: forall a. ToPerch a => a -> Perch
bdo cont = nelem "bdo" `child` cont
blockquote :: forall a. ToPerch a => a -> Perch
blockquote cont = nelem "blockquote" `child` cont
body :: forall a. ToPerch a => a -> Perch
body cont = nelem "body" `child` cont
button :: forall a. ToPerch a => a -> Perch
button cont = nelem "button" `child` cont
canvas :: forall a. ToPerch a => a -> Perch
canvas cont = nelem "canvas" `child` cont
caption :: forall a. ToPerch a => a -> Perch
caption cont = nelem "caption" `child` cont
cite :: forall a. ToPerch a => a -> Perch
cite cont = nelem "cite" `child` cont
code :: forall a. ToPerch a => a -> Perch
code cont = nelem "code" `child` cont
colgroup :: forall a. ToPerch a => a -> Perch
colgroup cont = nelem "colgroup" `child` cont
command :: forall a. ToPerch a => a -> Perch
command cont = nelem "command" `child` cont
datalist :: forall a. ToPerch a => a -> Perch
datalist cont = nelem "datalist" `child` cont
dd :: forall a. ToPerch a => a -> Perch
dd cont = nelem "dd" `child` cont
del :: forall a. ToPerch a => a -> Perch
del cont = nelem "del" `child` cont
details :: forall a. ToPerch a => a -> Perch
details cont = nelem "details" `child` cont
dfn :: forall a. ToPerch a => a -> Perch
dfn cont = nelem "dfn" `child` cont
div :: forall a. ToPerch a => a -> Perch
div cont = nelem "div" `child` cont
dl :: forall a. ToPerch a => a -> Perch
dl cont = nelem "dl" `child` cont
dt :: forall a. ToPerch a => a -> Perch
dt cont = nelem "dt" `child` cont
em :: forall a. ToPerch a => a -> Perch
em cont = nelem "em" `child` cont
fieldset :: forall a. ToPerch a => a -> Perch
fieldset cont = nelem "fieldset" `child` cont
figcaption :: forall a. ToPerch a => a -> Perch
figcaption cont = nelem "figcaption" `child` cont
figure :: forall a. ToPerch a => a -> Perch
figure cont = nelem "figure" `child` cont
footer :: forall a. ToPerch a => a -> Perch
footer cont = nelem "footer" `child` cont
form :: forall a. ToPerch a => a -> Perch
form cont = nelem "form" `child` cont
h1 :: forall a. ToPerch a => a -> Perch
h1 cont = nelem "h1" `child` cont
h2 :: forall a. ToPerch a => a -> Perch
h2 cont = nelem "h2" `child` cont
h3 :: forall a. ToPerch a => a -> Perch
h3 cont = nelem "h3" `child` cont
h4 :: forall a. ToPerch a => a -> Perch
h4 cont = nelem "h4" `child` cont
h5 :: forall a. ToPerch a => a -> Perch
h5 cont = nelem "h5" `child` cont
h6 :: forall a. ToPerch a => a -> Perch
h6 cont = nelem "h6" `child` cont
head :: forall a. ToPerch a => a -> Perch
head cont = nelem "head" `child` cont
header :: forall a. ToPerch a => a -> Perch
header cont = nelem "header" `child` cont
hgroup :: forall a. ToPerch a => a -> Perch
hgroup cont = nelem "hgroup" `child` cont
html :: forall a. ToPerch a => a -> Perch
html cont = nelem "html" `child` cont
i :: forall a. ToPerch a => a -> Perch
i cont = nelem "i" `child` cont
iframe :: forall a. ToPerch a => a -> Perch
iframe cont = nelem "iframe" `child` cont
ins :: forall a. ToPerch a => a -> Perch
ins cont = nelem "ins" `child` cont
kbd :: forall a. ToPerch a => a -> Perch
kbd cont = nelem "kbd" `child` cont
label :: forall a. ToPerch a => a -> Perch
label cont = nelem "label" `child` cont
legend :: forall a. ToPerch a => a -> Perch
legend cont = nelem "legend" `child` cont
li :: forall a. ToPerch a => a -> Perch
li cont = nelem "li" `child` cont
map :: forall a. ToPerch a => a -> Perch
map cont = nelem "map" `child` cont
mark :: forall a. ToPerch a => a -> Perch
mark cont = nelem "mark" `child` cont
menu :: forall a. ToPerch a => a -> Perch
menu cont = nelem "menu" `child` cont
meter :: forall a. ToPerch a => a -> Perch
meter cont = nelem "meter" `child` cont
nav :: forall a. ToPerch a => a -> Perch
nav cont = nelem "nav" `child` cont
noscript :: forall a. ToPerch a => a -> Perch
noscript cont = nelem "noscript" `child` cont
object :: forall a. ToPerch a => a -> Perch
object cont = nelem "object" `child` cont
ol :: forall a. ToPerch a => a -> Perch
ol cont = nelem "ol" `child` cont
optgroup :: forall a. ToPerch a => a -> Perch
optgroup cont = nelem "optgroup" `child` cont
option :: forall a. ToPerch a => a -> Perch
option cont = nelem "option" `child` cont
output :: forall a. ToPerch a => a -> Perch
output cont = nelem "output" `child` cont
p :: forall a. ToPerch a => a -> Perch
p cont = nelem "p" `child` cont
pre :: forall a. ToPerch a => a -> Perch
pre cont = nelem "pre" `child` cont
progress :: forall a. ToPerch a => a -> Perch
progress cont = nelem "progress" `child` cont
q :: forall a. ToPerch a => a -> Perch
q cont = nelem "q" `child` cont
rp :: forall a. ToPerch a => a -> Perch
rp cont = nelem "rp" `child` cont
rt :: forall a. ToPerch a => a -> Perch
rt cont = nelem "rt" `child` cont
ruby :: forall a. ToPerch a => a -> Perch
ruby cont = nelem "ruby" `child` cont
samp :: forall a. ToPerch a => a -> Perch
samp cont = nelem "samp" `child` cont
script :: forall a. ToPerch a => a -> Perch
script cont = nelem "script" `child` cont
section :: forall a. ToPerch a => a -> Perch
section cont = nelem "section" `child` cont
select :: forall a. ToPerch a => a -> Perch
select cont = nelem "select" `child` cont
small :: forall a. ToPerch a => a -> Perch
small cont = nelem "small" `child` cont
span :: forall a. ToPerch a => a -> Perch
span cont = nelem "span" `child` cont
strong :: forall a. ToPerch a => a -> Perch
strong cont = nelem "strong" `child` cont
{-style cont = nelem  "style" `child` cont-}
sub :: forall a. ToPerch a => a -> Perch
sub cont = nelem "sub" `child` cont
summary :: forall a. ToPerch a => a -> Perch
summary cont = nelem "summary" `child` cont
sup :: forall a. ToPerch a => a -> Perch
sup cont = nelem "sup" `child` cont
table :: forall a. ToPerch a => a -> Perch
table cont = nelem "table" `child` cont
tbody :: forall a. ToPerch a => a -> Perch
tbody cont = nelem "tbody" `child` cont
td :: forall a. ToPerch a => a -> Perch
td cont = nelem "td" `child` cont
textarea :: forall a. ToPerch a => a -> Perch
textarea cont = nelem "textarea" `child` cont
tfoot :: forall a. ToPerch a => a -> Perch
tfoot cont = nelem "tfoot" `child` cont
th :: forall a. ToPerch a => a -> Perch
th cont = nelem "th" `child` cont
thead :: forall a. ToPerch a => a -> Perch
thead cont = nelem "thead" `child` cont
time :: forall a. ToPerch a => a -> Perch
time cont = nelem "time" `child` cont
title :: forall a. ToPerch a => a -> Perch
title cont = nelem "title" `child` cont
tr :: forall a. ToPerch a => a -> Perch
tr cont = nelem "tr" `child` cont
ul :: forall a. ToPerch a => a -> Perch
ul cont = nelem "ul" `child` cont
var :: forall a. ToPerch a => a -> Perch
var cont = nelem "var" `child` cont
video :: forall a. ToPerch a => a -> Perch
video cont = nelem "video" `child` cont
ctag :: forall a. ToPerch a => String -> a -> Perch
ctag tag cont = nelem tag `child` cont

-- ** HTML4 Support
center :: forall a. ToPerch a => a -> Perch
center cont = nelem "center" `child` cont

noHtml :: Perch
noHtml = mempty


-- * DOM Tree Navigation & Manipulation

-- ** Attributes

atr :: String -> String -> Attribute
atr name val = {name, val}

id :: String -> Attribute
id = atr "id"
height :: String -> Attribute
height = atr "height"
href :: String -> Attribute
href = atr "href"
src :: String -> Attribute
src = atr "src"
style :: String -> Attribute
style = atr "style"
width :: String -> Attribute
width = atr "width"

-- ** Traversal

-- | Return the current node.
this :: Perch
this = perch pure

-- | Goes to the parent node of the first and execute the second.
goParent :: Perch -> Perch -> Perch
goParent ch pe = perch $ \e -> do
  fs <- build ch e
  pr <- parent fs
  build pe pr

-- ** Manipulation

-- | Delete the current node and return the parent.
delete :: Perch
delete = perch $ \e ->
  do par <- parent e
     removeChild e par
     pure par

-- | Delete all children of the current node.
clear :: Perch
clear = perch <<< withPerch $ clearChildren

-- | Replace the current node with a new one
outer :: Perch -> Perch -> Perch
outer olde newe = perch $ \e ->
  do o <- build olde e
     n <- build newe e
     replace o n


-- | JQuery-like DOM manipulation.  It applies the Perch DOM manipulation for
-- each found element using @querySelectorAll@ function.
forElems :: String -> Perch -> Perch
forElems query action = perch <<< withPerch <<< const $
  do els <- queryAll query
     traverse_ (build action) els

-- | Like 'forElems', but works in Effect monad.
-- Example:
--
-- @
-- import GHCJS.Foreign.Callback (asyncCallback1)
--
-- main = do
--   body <- getBody
--   makeRed \<- asyncCallback1 (\\ _ -\> do
--     forElems_ ".changeable" $
--       this ! style "color:red")
--   (flip build) body . div $ do
--      div ! atr "class" "changeable" $ \"Changeable\"
--      div \"Static\"
--      div ! atr "class" "changeable" $ \"Changeable\"
--      addEvent this Click makeRed
-- @
forElems_ :: String -> Perch -> Effect Unit
forElems_ els action = do
  unit <$ build (forElems els action) undefined
  where
    undefined = unsafeCoerce unit

-- | Decalarative synonym for @flip forElems@.
--
-- Examples:
--
-- @
-- doAction \``withElems`\` ".item"
-- `forElems` ".item" doAction
-- @
withElems ::  Perch -> String -> Perch
withElems = flip forElems

-- | A declarative synonym of @flip forElements@.
withElems_ :: Perch -> String -> Effect Unit
withElems_ = flip forElems_

-- | Apply action to perch with given identifier.
forElemId :: String -> Perch -> Perch
forElemId eid act = perch <<< withPerch <<< const $
  do el <- getElemById eid
     build act el

-- | Effect version of 'forElemId_'.
forElemId_ :: String -> Perch -> Effect Unit
forElemId_ act eid =
  unit <$ flip build undefined (forElemId act eid)
  where
    undefined = unsafeCoerce unit

-- | A synonym to @flip forElemId@.
withElemId :: Perch -> String -> Perch
withElemId = flip forElemId

-- | A synonym to @flip forElemId_@.
withElemId_ :: Perch -> String -> Effect Unit
withElemId_ = flip forElemId_

withPerch :: forall a. (Elem -> Effect a) -> Elem -> Effect Elem
withPerch act e = act e *> pure e

withPerchBuild :: forall a b. PerchM a -> (Elem -> Effect b) -> Elem -> Effect Elem
withPerchBuild pr act e =
  do x <- build pr e
     _ <- act x
     pure x
