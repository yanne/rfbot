import IrcCore
import Control.Monad.Error
import Data.ConfigFile
import Data.List
import Data.Either.Utils
import Text.Regex


issueRegex :: Regex
issueRegex = mkRegex "(rf|ride) ([0-9]+)"

matchAganstIssueRegex :: (String -> String -> t) -> t -> String -> t
matchAganstIssueRegex m n s = case matchRegex issueRegex s of
    Just([project, number]) -> m project number
    Nothing        -> n


isIssueLink :: String -> Bool
isIssueLink = matchAganstIssueRegex (\x y -> True) False

issueLink :: String -> String
issueLink = matchAganstIssueRegex match noMatch where
    match p n = "http://code.google.com/p/robotframework" ++ (projectName p) ++ "/issues/detail?id=" ++ n
    projectName p = if p == "rf" then "" else "-" ++ p
    noMatch = ""

handleMessage :: Config -> String -> Net ()
handleMessage cfg "!quit"              = write "QUIT" ":Exiting" >> exit
handleMessage cfg m | isIssueLink m    = privmsg cfg (issueLink m)
handleMessage _  _                     = return () -- ignore everything else


-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = do
        config <- runErrorT $
            do
                cp <- join $ liftIO $ readfile emptyCP "rfbotrc"
                let x = cp
                nick <- get x "DEFAULT" "nick"
                password <- get x "DEFAULT" "password"
                channel <- get x "DEFAULT" "channel"
                let config = Config nick password channel
                return (config)
        let cfg = fromRight config
        runIrcBot cfg (handleMessage cfg)
