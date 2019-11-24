module Miso.STM.BoundView 
    ( BoundView (..)
    , AttrM (..)
    , InpType
    , Attrs
    , SubViews
    , ActionM (..)
    , getView
    ) where

import Miso
import Miso.String

data ActionM m 
    = UIEvent (m -> m)
    | SrvEvent m
    | NoAction

type Attrs m = [AttrM m]
type ElemType m = [Attribute (ActionM m)] -> [View (ActionM m)] -> View (ActionM m)
type InpType m = [Attribute (ActionM m)] -> View (ActionM m)
type SubViews m = [BoundView m]    
data BoundView m 
    = El (ElemType m) (Attrs m) (SubViews m)
    | Br (Attrs m)
    | Txt MisoString 
    | DynTxt (m -> MisoString) 
    | Inp (Attrs m)
    | Link (Attrs m)

data AttrM m 
    = A (Attribute (ActionM m))    -- plain attribute
    | BA (m -> Attribute (ActionM m))
    | BE (ActionM m -> Attribute (ActionM m)) (m -> m) 
    | OnChecked (Bool -> m -> m)
    | OnInput (MisoString -> m -> m)


getAttrib :: m -> AttrM m -> Attribute (ActionM m)
getAttrib _m (A attr) = attr
getAttrib m (BA af) = af m 
getAttrib _m (BE af mf) = af (UIEvent mf) 
getAttrib _m (OnChecked boolSetter) = onChecked (\(Checked fl) -> UIEvent (boolSetter fl)) 
getAttrib _m (OnInput strSetter) = onInput (UIEvent . strSetter) 

getView :: BoundView m -> m -> View (ActionM m)
getView (El elFunc attrs subViews) m = elFunc (fmap (getAttrib m) attrs) (fmap (flip getView m) subViews)
getView (Br attrs) m = br_ (fmap (getAttrib m) attrs) 
getView (Txt txt) _m = text txt
getView (DynTxt f) m = text (f m)
getView (Inp attrs) m = input_ (fmap (getAttrib m) attrs) 
getView (Link attrs) m = link_ (fmap (getAttrib m) attrs) 
    
