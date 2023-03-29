{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Serialization where

import Circuit
import Control.Monad qualified as Monad
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Builder.Int (hexadecimal)
import Data.Vector qualified as Vector
import Formatting hiding (char)
import Relude hiding (many)
import Relude.Applicative ((*>), (<*), (<*>), (<|>))
import Text.Megaparsec (MonadParsec (takeP), Parsec, errorBundlePretty, oneOf, parse, runParser, sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, newline, space, space1)
import Text.Megaparsec.Char qualified
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Utils

class Serialize a where
  serialize :: a -> Text
  parser :: TextParser a

instance Serialize Cmd where
  serialize :: Cmd -> Text
  serialize Cmd {..} = LText.toStrict $ format (hex % stext % hex) control " " target
  parser :: TextParser Cmd
  parser = do
    control :: Word8 <- Lexer.hexadecimal <* space1
    target :: Word8 <- Lexer.hexadecimal
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
