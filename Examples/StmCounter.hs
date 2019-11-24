module Main where

import Miso hiding (go)
import Miso.String
import Language.Javascript.JSaddle.Warp as JSaddle
import Control.Concurrent.STM
import Miso.STM.MisoSTM

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

main :: IO ()
main = do
  JSaddle.run 8000 $ startApp =<< mkBoundApp (srvApp defaultModel) defaultModel testView


srvApp :: Model -> TVar Model -> IO ()
srvApp initModel tmodel = do 
    putStrLn "started serverApp"
    go initModel 
  where 
    go curModel = do
      newModel <- monitorModel curModel tmodel
      putStrLn ("model changed was:" <> show curModel <> " now: " <> show newModel)
      go newModel 

monitorModel :: Model -> TVar Model -> IO Model
monitorModel model tmodel = atomically $ do
  curModel <- readTVar tmodel
  if curModel == model
    then retry
    else return curModel
  
