module Actions (
    execute,
    exitMessage
) where

import Parse
import qualified Data.Text as T

exitMessage :: T.Text
exitMessage = T.pack "Thanks for playing!"

unknownMessage :: T.Text -> T.Text
unknownMessage what = T.pack $ "I'm sorry, I don't know what '" ++ T.unpack what ++ "' is."

execute :: Action -> Maybe T.Text
execute action = case action of
    Echo echoed -> Just echoed
    Unknown what -> Just $ unknownMessage what
    Exit -> Just exitMessage
    Empty -> Nothing
