{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module JiaHe.LM.App.App
    ( main
    , mainReadOnly
    ) where

import           Control.Monad      (join, (>=>), (>>))
import qualified Data.ByteString    as BS
import qualified Data.Either.Utils  as EitherUtils
import qualified Data.Text          as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO       as TextIO
import qualified JiaHe.LM.License   as Lcs
import           System.Environment
import           System.Exit

main :: IO ()
main = mainBuilder allMappings

mainReadOnly :: IO ()
mainReadOnly = mainBuilder readOnlyMappings

type CommandMappings = [(T.Text, [T.Text] -> Either T.Text T.Text)]

mainBuilder :: CommandMappings -> IO ()
mainBuilder mappings = do
  result <- fmap ((splitCmdArgs >=> execute mappings) . fmap T.pack) getArgs
  either
    (\s -> TextIO.putStrLn ("错误: " `mappend` s) >> exitFailure)
    (\s -> TextIO.putStrLn s >> exitSuccess)
    result

execute :: CommandMappings -> (T.Text, [T.Text]) -> Either T.Text T.Text
execute mappings (cmd, args) = join $ EitherUtils.maybeToEither "找不到命令"
  (lookup cmd mappings <*> pure args)

readOnlyMappings :: CommandMappings
readOnlyMappings = [ ("dec", decrypt) ]

allMappings :: CommandMappings
allMappings = ("enc", encrypt) : readOnlyMappings

encrypt :: [T.Text] -> Either T.Text T.Text
encrypt args =
  if length args /= 5
    then Left "参数数量不正确"
    else case args of
      (key':fields) ->
        Lcs.readLicense (T.intercalate "\n" fields)
        >>= fmap (T.pack . show . BS.unpack)
          . Lcs.encryptLicense (Encoding.encodeUtf8 key')
      [] -> error "Impossible code path."

decrypt  :: [T.Text] -> Either T.Text T.Text
decrypt args =
  if length args /= 2
    then Left "参数数量不正确"
    else case args of
      [key', ctext'] ->
        Lcs.decryptLicense
          (Encoding.encodeUtf8 key')
          ((BS.pack . read . T.unpack) ctext')
        >>= pure . Lcs.writeLicense
      _ -> error "Impossible code path."

splitCmdArgs :: [T.Text] -> Either T.Text (T.Text, [T.Text])
splitCmdArgs = splitAtHead "参数过少（命令为空）"

splitAtHead :: T.Text -> [a] -> Either T.Text (a, [a])
splitAtHead msg []     = Left msg
splitAtHead _ (x:xs) = Right (x, xs)
