module IrcCore where

import Data.List
import Network
import Text.Printf
import System.IO
import System.Exit
import Control.Exception
import Control.Monad.Reader
import Prelude hiding (catch)

-- This code is adapted from:
-- http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot


server = "irc.freenode.org"
port   = 6667

data Config = Config String String ChannelName deriving Show
type ChannelName = String


-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

runIrcBot config handleMessage = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catch (runReaderT (run config handleMessage) st) (\(SomeException _) -> return ())

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Config -> ([Char] -> Net ()) -> Net ()
run (Config nick password channel) messageHandler = do
    write "NICK" nick
    write "PRIVMSG"  ("nickserv identify " ++ nick ++ password)
    write "USER" (nick++" 0 * :Robot Framework bot")
    write "JOIN" channel
    asks socket >>= (listen messageHandler)

-- Process each line from the server
listen :: ([Char] -> Net ()) -> Handle -> Net ()
listen messageHandler h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else messageHandler (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)


-- Send a privmsg to the current chan + server
-- TODO: chan should come from config
privmsg :: Config -> String -> Net ()
privmsg (Config _ _ channel) s = write "PRIVMSG" (channel ++ " :" ++ s)

-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

exit :: Net ()
exit = io (exitWith ExitSuccess)

-- Convenience.
io :: IO a -> Net a
io = liftIO
