{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import qualified Options.Applicative as OP
import Data.Monoid ((<>))
import Control.Monad (when, forever)

data Config = Config { configPort :: String
                     , configFilePath :: String
                     , configFileName :: String
                     , configExecAfter :: Bool
                     }

main :: IO ()
main = do
    config <- OP.execParser opts
    let scriptName = B.pack (configFileName config)
    let scriptPath = configFilePath config
    let port = configPort config

    s <- openSerial port espSettings
    putStrLn "Press reset button on board."
    readUntilPrompt s B.empty >>= B.putStr
    scriptContents <- B.readFile scriptPath
    writeToFile s scriptName scriptContents
    when (configExecAfter config) (sendReadPrint s (doFileCommand scriptName) >> forever (recv s 128 >>= B.putStr))
    closeSerial s
    putStrLn ""

-- Write a file to the board's fs. Assumes the device is immediately
-- ready for commands.
writeToFile :: SerialPort -> B.ByteString -> B.ByteString -> IO ()
writeToFile s name contents = do
    sendReadPrint s (openCommand name)
    _ <- traverse (sendReadPrint s . writeLineCommand) (B.lines contents)
    sendReadPrint s closeCommand
    return ()

-- Send a command and print the response.
sendReadPrint :: SerialPort -> B.ByteString -> IO ()
sendReadPrint s bs = sendRead s bs >>= B.putStr

-- Send a command and return the response.
sendRead :: SerialPort -> B.ByteString -> IO B.ByteString
sendRead s bs = do
    send_ s bs
    readUntilPrompt s B.empty

-- Continue reading from the serial port until we get a prompt.
-- Return what was read in.
readUntilPrompt :: SerialPort -> B.ByteString -> IO B.ByteString
readUntilPrompt s acc = do
    output <- recv s 128
    let newAcc = acc `B.append` output
    maybe (readUntilPrompt s newAcc) (return) (containsPrompt newAcc)

-- Returns the text before the prompt if the input string
-- contains a prompt.
containsPrompt :: B.ByteString -> Maybe B.ByteString
containsPrompt bs =
    if not (isPrompt post) then Nothing else (Just bs)
    where 
        (_, post) = B.breakSubstring promptChars bs
        isPrompt str = str == promptChars
        promptChars = (endOfCommand `B.append` "> ")

-- Send and discard the result.
send_ :: SerialPort -> B.ByteString -> IO ()
send_ s bs = send s bs *> pure ()

writeLineCommand :: B.ByteString -> B.ByteString
writeLineCommand ln = "file.writeline([[" `B.append` ln `B.append` "]])" `B.append` endOfCommand

closeCommand :: B.ByteString
closeCommand = "file.close()" `B.append` endOfCommand

openCommand :: B.ByteString -> B.ByteString
openCommand fileName = "file.open(\"" `B.append` fileName `B.append` "\", \"w+\")" `B.append` endOfCommand

doFileCommand :: B.ByteString -> B.ByteString
doFileCommand fileName = "dofile(\"" `B.append` fileName `B.append` "\")" `B.append` endOfCommand

-- Character sequence for ending a command.
endOfCommand :: B.ByteString
endOfCommand = "\r\n"

-- Serial settings for the ESP module.
espSettings :: SerialPortSettings
espSettings = defaultSerialSettings

opts :: OP.ParserInfo Config
opts = OP.info (OP.helper <*> argumentParser)
     (  OP.fullDesc
     <> OP.progDesc "Uploads a file to an esp8266 board running nodemcu."
     )

argumentParser :: OP.Parser Config
argumentParser = Config
    <$> OP.strOption
            (  OP.short 'd'
            <> OP.long "device"
            <> OP.metavar "DEVICE"
            <> OP.help "Serial device."
            )
    <*> OP.strOption
            (  OP.short 'f'
            <> OP.long "file"
            <> OP.metavar "FILE"
            <> OP.help "File to send."
            )
    <*> OP.strOption
            (  OP.short 'n'
            <> OP.long "name"
            <> OP.metavar "NAME"
            <> OP.help "Name on board."
            )
    <*> OP.switch
            (  OP.long "exec"
            <> OP.help "Immediately execute the script and watch output."
            )