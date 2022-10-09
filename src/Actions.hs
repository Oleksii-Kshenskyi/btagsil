module Actions (
    execute,
    exitMessage,
    ControlFlow (..)
) where

import Parse
import qualified Data.Text as T

data ControlFlow = EmptyResponse
                 | TextResponse T.Text
                 | ExitGame T.Text


exitMessage :: T.Text
exitMessage = T.pack "Thanks for playing!"

unknownMessage :: T.Text -> T.Text
unknownMessage what = T.pack $ "I'm sorry, I don't know what '" ++ T.unpack what ++ "' is."

execute :: Action -> ControlFlow
execute action =
    case action of
        Echo echoed -> TextResponse echoed
        Unknown what -> TextResponse $ unknownMessage what
        Exit -> ExitGame exitMessage
        Empty -> EmptyResponse
