# miso-stm
Small library on top of miso framework that allows bi-directional data binding data to HTML elements using STM TVar 

* Just define you markup with direct binding to your model and no ```update``` method is needed.
* No messages (actions) are required as well.
* Your model is stored and TVar so it can be safely accessed and updated for other threads

## Classic Elm style counter example


```
type Model = Int 

defaultModel :: Model
defaultModel = 0

testView :: BoundView Model
testView = 
  El div_ []
    [ El div_ [] 
        [ El button_ [BE onClick (+1)] [Txt "+"] ] 
    , El div_ [] 
        [DynTxt (toMisoString.show)]
    , El div_ [] 
        [ El button_ [BE onClick (\i -> i-1)] [Txt "-"] ] 
    ]  

```
