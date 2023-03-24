{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Serialization where

import Relude.Applicative ((*>), (<*), (<*>), (<|>))
import Circuit
import Control.Monad qualified as Monad
import Data.Array.BitArray as BitArray
import Data.Array.BitArray.ByteString as BitArray
  ( fromByteString,
    toByteString,
  )
import Data.ByteString.Base64 qualified as Base64
import Data.List qualified as List
import Data.Text qualified as Text
import Relude hiding (many)
import Text.Megaparsec (MonadParsec (takeP), Parsec, errorBundlePretty, parse, runParser, oneOf, takeWhile1P, sepEndBy1)
import Text.Megaparsec.Char (char, space, newline, space1)
import Text.Megaparsec.Char qualified
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Utils
import Data.Text.Lazy.Builder.Int (hexadecimal)
import qualified Data.Vector as Vector
import Text.Printf (printf)

class Serialize a where
  serialize :: a -> Text
  parser :: TextParser a

instance Serialize Cmd where
  serialize :: Cmd -> Text
  serialize Cmd {..} = Text.pack $ printf "%2h %2h" (toWord8 control) (toWord8 target)
  parser :: TextParser Cmd
  parser = do
    control :: BitArray Word8 <- fromWord8 <$> (Lexer.hexadecimal <* space1)
    target :: BitArray Word8 <- fromWord8 <$> Lexer.hexadecimal
    pure Cmd {..}

instance Serialize Circuit where
  serialize :: Circuit -> Text
  serialize circuit = "[" <> Text.intercalate "," (Vector.toList $ Vector.map serialize circuit) <> "]"
  parser :: TextParser Circuit
  parser = do
    cmdList :: [Cmd] <- char '[' *> sepEndBy1 parser "," <* char ']'
    pure $ Vector.fromList cmdList

instance Serialize [Circuit] where
  serialize :: [Circuit] -> Text
  serialize = Text.intercalate "\n" . List.map serialize
  parser :: TextParser [Circuit]
  parser = sepEndBy1 parser newline

deserialize :: Serialize a => String -> FilePath -> Text -> IO a
deserialize onError fileName bs =
  case parse parser fileName bs of
    Left err -> die $ onError <> errorBundlePretty err
    Right a -> pure a

type TextParser = Parsec Void Text
