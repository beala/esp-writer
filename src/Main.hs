{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as B
import qualified Options.Applicative as OP
import Data.Monoid ((<>))
import Control.Monad (when)

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
    when (configExecAfter config) (sendReadPrint s ("dofile(\"" `B.append` scriptName `B.append` "\")" `B.append` endOfCommand))
    closeSerial s
    putStrLn ""

-- Write a file to the board's fs. Assumes the device is immediately
-- ready for commands.
writeToFile :: SerialPort -> B.ByteString -> B.ByteString -> IO ()
writeToFile s name contents = do
    sendReadPrint s openCommand
    _ <- traverse (sendReadPrint s . writeLine) (B.lines contents)
    sendReadPrint s closeCommand
    return ()

    where 
        openCommand  = "file.open(\"" `B.append` name `B.append` "\", \"w+\")" `B.append` endOfCommand
        writeLine ln = "file.writeline([[" `B.append` ln `B.append` "]])" `B.append` endOfCommand
        closeCommand = "file.close()" `B.append` endOfCommand

-- Send a command and print the read response.
sendReadPrint :: SerialPort -> B.ByteString -> IO ()
sendReadPrint s bs = sendRead s bs >>= B.putStr

-- Send a command and return the read response.
sendRead :: SerialPort -> B.ByteString -> IO B.ByteString
sendRead s bs = do
    send_ s bs
    readUntilPrompt s B.empty

-- Continue reading from the serial port until we get a prompt.
-- Return the read strings.
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

-- Character sequence for ending a command.
endOfCommand :: B.ByteString
endOfCommand = "\r\n"

-- Serial settings for the ESP module.
espSettings :: SerialPortSettings
espSettings = defaultSerialSettings

opts :: OP.ParserInfo Config
opts = OP.info (OP.helper <*> argumentParser)
     (  OP.fullDesc
     <> OP.progDesc "Uploads a file to a HUZZAH board."
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
            <> OP.help "Name once uploaded to board."
            )
    <*> OP.switch
            (  OP.long "exec"
            <> OP.help "Immediately execute the script and watch output."
            )