module Miso.STM.ViewHelpers 
  ( checkbox
  , edit
  ) where

import Miso
import Miso.STM.BoundView
import Miso.String

checkbox :: (m -> Bool) -> (Bool -> m -> m) -> MisoString -> [BoundView m] 
checkbox getter setter caption =
  [ Inp 
      [ A $ type_ "checkbox"
      , BA (checked_ . getter)
      , OnChecked setter
      ] 
  , Txt caption
  ]
      

edit :: (m -> MisoString) -> (MisoString -> m -> m) -> BoundView m 
edit getter setter =
  Inp 
    [ A $ type_ "text"
    , BA (value_ . getter)
    , OnInput setter
    ] 
  

