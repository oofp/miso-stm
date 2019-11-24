module Miso.STM.BoundApp 
  ( mkBoundApp
  ) where


import Miso hiding (go)
--import Miso.String
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM
import Miso.STM.BoundView

mkBoundApp :: Eq model => (TVar model -> IO ()) -> model -> BoundView model -> JSM (App model (ActionM model))
mkBoundApp task initModel boundView = do
  tmodel <- liftIO $ newTVarIO initModel 
  void $ liftIO $ async (task tmodel) 
  let view' = getView boundView
      sub = initTVarSub initModel tmodel
      update' = updateModel tmodel
      app = App
              { model = initModel
              , view = view'
              , update = update'
              , subs = [sub]
              , events = defaultEvents
              , initialAction = NoAction
              , mountPoint = Nothing
              }   
  return app             

updateModel :: TVar model -> ActionM model -> model -> Effect (ActionM model) model
updateModel tvar (UIEvent fm) model =  
  model <# (liftIO $ atomically $ writeTVar tvar (fm model) >> return NoAction)
updateModel _tvar (SrvEvent newModel) _model =
  noEff newModel  
updateModel _tvar NoAction model =
  noEff model  
  
-- type Sink action = action -> IO ()      -- Defined in `Miso.Effect'
-- type Sub action = Sink action -> JSM ()
initTVarSub :: (Eq model) => model -> TVar model -> Sub (ActionM model) 
initTVarSub initModel tvar sink = 
    liftIO $ void $ async $ go initModel
  where  
    go curModel = do 
      newModel <- atomically $ do
        theModel <- readTVar tvar
        if (theModel == curModel) 
          then retry
          else return theModel
      sink (SrvEvent newModel)
      go newModel     
  
