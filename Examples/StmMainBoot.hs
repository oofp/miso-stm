module Main where

import Miso hiding (go)
import Miso.String
import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Warp as JSaddle
import Control.Concurrent.STM
import Data.Text
import Miso.STM.MisoSTM

data Model = Model 
    { inputText :: Text
    , isChecked :: Bool
    , clicked :: Bool
    } deriving (Eq, Show)

defaultModel :: Model
defaultModel = Model "" False False

testView :: BoundView Model
testView = 
  El div_ [A$ class_ "container"]
    [ El div_ [] 
        [ El button_ [A $ class_ "btn btn-success", BE onClick (\m -> m {clicked = True})] [Txt "ClickMe"] ] 
    , El div_ [A $ class_ "view"] 
        (checkbox isChecked (\fl m -> m {isChecked = fl}) "CheckMe")
    , El div_ [] 
        [edit (ms . inputText) (\txt m -> m {inputText = fromMisoString txt})]
    , El div_ [] 
        [ El button_ [A $ class_ "btn btn-danger", BE onClick (\m -> m {inputText = ""})] [Txt "Clear text"] ] 
    , Link
        [ A $ rel_ "stylesheet"
        , A $ href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" 
        , A $ textProp "integrity" "sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" 
        , A $ textProp "crossorigin" "anonymous"
        ]    
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
      liftIO $ atomically $ modifyTVar tmodel (\m -> m {clicked=False})
      go newModel 

monitorModel :: Model -> TVar Model -> IO Model
monitorModel model tmodel = atomically $ do
  curModel <- readTVar tmodel
  if curModel == model
    then retry
    else return curModel
  
