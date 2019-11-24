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
  El div_ []
    [ El div_ [] 
        [ El button_ [BE onClick (\m -> m {clicked = True})] [Txt "ClickMe"] ] 
    , El div_ [A $ class_ "view"] 
        (checkbox isChecked (\fl m -> m {isChecked = fl}) "CheckMe")
    , El div_ [] 
        [edit (ms . inputText) (\txt m -> m {inputText = fromMisoString txt})]
    , El div_ [] 
        [ El button_ [BE onClick (\m -> m {inputText = ""})] [Txt "Clear text"] ] 
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
  
