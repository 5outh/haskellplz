{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.String as MP
import Data.Monoid
import Data.List
import Text.StringTemplate
import Data.List.Split
import Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (getHomeDirectory)

data HaskellPlz = HaskellPlz
  { cmd :: Command
  } deriving (Show, Eq)

data Command
  = CMake Entity
  | CDelete Entity
  deriving (Show, Eq)

data Entity =
  Module String
  deriving (Show, Eq)

-- e.g. ["Data","List"]
data ModuleDesc =
  ModuleDesc [String]
  deriving (Show, Eq)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

entity :: Parser Entity
entity =
  Module <$> strArgument (metavar "MODULE" <> help "Module to operate on")


-- TOOO: This is broken
command_ :: Parser Command
command_ =
  CMake <$>
  ((subparser $ command "module" (entity `withInfo` "Make a new haskell module"))
  )

haskellPlz :: Parser HaskellPlz
haskellPlz =
  HaskellPlz <$>
  ((subparser $ command "make" (command_ `withInfo` "Make new stuff")) <|>
   (subparser $ command "delete" (command_ `withInfo` "Delete stuff")))

haskellPlzInfo :: ParserInfo HaskellPlz
haskellPlzInfo =
  info
    (helper <*> haskellPlz)
    (fullDesc <> progDesc "Generate haskell-related stuff" <>
     header "haskellplz - Generate haskell-related files easily")

capital :: MP.Parser Char
capital = MP.oneOf ['A' .. 'Z']

ident :: MP.Parser String
ident = do
  let identChar =
        MP.oneOf $ ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['_']
  c <- capital
  rest <- MP.optional (MP.many identChar)
  pure $
    case rest of
      Nothing -> [c]
      Just rest_ -> c : rest_

moduleDesc :: MP.Parser ModuleDesc
moduleDesc = ModuleDesc <$> ident `MP.sepBy1` (MP.char '.')

makeModule :: ModuleDesc -> ReaderT String IO ()
makeModule (ModuleDesc desc) = do
  home <- ask
  let splitModule = "src" : desc
      dir = init splitModule
      dirpath = T.pack $ (intercalate "/" dir) <> "/"
      file = (intercalate "/" splitModule) <> ".hs"
      moduleName = intercalate "." desc
  liftIO . putStrLn $ "Creating new module " <> moduleName
  moduleTemplate :: StringTemplate T.Text <-
    liftIO $
    newSTMP <$> readFile (home <> "/.haskellplz/templates/module.hstring")
  let rendered = render $ setAttribute "module" moduleName moduleTemplate
  shell ("mkdir -p " <> dirpath) empty
  liftIO $ TIO.writeFile file rendered

deleteModule :: ModuleDesc -> ReaderT String IO ()
deleteModule (ModuleDesc desc) = do
  home <- ask
  let splitModule = "src" : desc
      dir = init splitModule
      dirpath = T.pack $ (intercalate "/" dir) <> "/"
      file = (intercalate "/" splitModule) <> ".hs"
      moduleName = intercalate "." desc
  -- TODO ask for permission
  liftIO . putStrLn $ "Deleting module " <> moduleName
  rm (fromText $ T.pack file)

-- TODO subparser to call haskellplz make module, for example
main :: IO ()
main = do
  home <- getHomeDirectory
  parsed <- execParser haskellPlzInfo
  case parsed of
    HaskellPlz (CMake (Module moduleName)) ->
      case MP.parse moduleDesc "(interactive)" moduleName of
        Left err -> putStrLn $ moduleName <> " is not a valid module name."
        Right desc -> runReaderT (makeModule desc) home
    HaskellPlz (CDelete (Module moduleName)) ->
      case MP.parse moduleDesc "(interactive)" moduleName of
        Left err -> putStrLn $ moduleName <> " is not a valid module name."
        Right desc -> runReaderT (deleteModule desc) home
