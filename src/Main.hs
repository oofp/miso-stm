module Main where

import Miso
import Miso.String
import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Warp as JSaddle

type Model = Int

defaultModel :: Model
defaultModel = 0

data Event
    = NoEvent
    | Inc
    | Dec
    | SayHello
    | PromptResp String
    | StartPrompt
    deriving (Eq, Show)

prompt = do     
    liftIO $ putStrLn "Enter '+' or '-'"
    inp <- liftIO $ getLine
    pure $ PromptResp inp

update' :: Event -> Model -> Effect Event Model
update' event model = case event of
    NoEvent -> noEff model
    Inc     -> noEff $ succ model
    Dec     -> noEff $ pred model
    SayHello -> model <# do
        liftIO $ putStrLn "Hello!"
        pure NoEvent
    PromptResp inp-> 
        case inp of 
            "+" -> succ model <# prompt
            "-" -> pred model <# prompt
            _ -> model <# prompt
    StartPrompt -> model <# prompt        
    
view' :: Model -> View Event
view' model = div_ []
    [ text "Hello ~ Haskell GUI"
    , br_ []
    , button_ [ onClick Inc ] [ text "+" ]
    , text $ ms model
    , button_ [ onClick Dec ] [ text "-" ]
    , br_ []
    , button_ [ onClick SayHello ] [ text "Say Hello!" ]
    ]

main :: IO ()
main = JSaddle.run 8000 $ startApp App {..}
  where
    initialAction = StartPrompt
    model  = defaultModel
    update = update'
    view   = view'
    events = defaultEvents
    subs   = []
    mountPoint = Nothing