{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, RecordWildCards                                #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-unused-do-bind         #-}
module Main where
import           Control.Monad
import           Data.Monoid
import qualified Data.Text             as T
import           Options.Applicative
import           Shelly
import           Text.Shakespeare.Text

default (T.Text)

dotTeX :: T.Text -> T.Text
dotTeX = flip T.append ".tex"

omakeroot :: T.Text
omakeroot = [st|
open build/LaTeX
DefineCommandVars()
.SUBDIRS: .
|]

data Settings = Settings { texDocs   :: [T.Text]
                         , deps      :: [String]
                         , git       :: Bool
                         , latex     :: String
                         , bibtex    :: String
                         , dvipdfm   :: String
                         , makeindex :: String
                         } deriving (Read, Show, Eq, Ord)

settings :: ParserInfo Settings
settings = info (helper <*> parser)
                (fullDesc
              <> header "omaketex - OMakefile generation for latex. "
              <> progDesc "Generates a OMakefile for latex files. It also supports git." )
  where
    parser = Settings <$> arguments1 (fmap T.pack <$> str) (metavar "TeXs" <> help "TeX files to typeset.")
                      <*> many (strOption (metavar "FILE" <> help "File(s) tex depends on (e.g. bib, sty...)"
                                        <> long "deps" <> short 'd')
                               )
                      <*> switch (long "git" <> short 'g' <> help "whether commit & push after typesettings success.")
                      <*> strOption (long "latex" <> short 't' <> value "platex" <> showDefault
                                 <> help "tex engine")
                      <*> strOption (long "bibtex" <> short 'b' <> value "pbibtex" <> showDefault
                                 <> help "bibtex")
                      <*> strOption (long "dvipdfm" <> short 'b' <> value "dvipdfmx" <> showDefault
                                 <> help "dvipdfm")
                      <*> strOption (long "mendex" <> short 'm' <> value "mendex -U" <> showDefault
                                 <> help "mendex")

omakefile :: Settings -> T.Text
omakefile (Settings ins dps gits latex bibtex dvipdfm mendex) = T.tail [st|
LATEX = #{latex}
MAKEINDEX = #{mendex}
DVIPDFM = #{dvipdfm}
BIBTEX = #{bibtex}

TEXINPUTS = #{T.unwords $ map dotTeX ins}
#{T.unlines $ map latexDocument ins}
TEXDEPS[] =
#{T.unlines $ map indent $ map T.pack dps}
.DEFAULT: #{T.unwords $ concatMap dviAndPdf ins}
.BUILD_SUCCESS:
  /usr/bin/open -aSkim -g #{T.unwords $ map dotPdf ins}
  #{gitmessage gits}
|]

gitmessage :: Bool -> T.Text
gitmessage True = [st|(git commit -am "$(shell date +%Y-%m-%d\ %T\ %Z)" && git push origin master) || echo "nothing to commit nor push"|]
gitmessage False = ""

dotPdf :: T.Text -> T.Text
dotPdf x = x <> ".pdf"

dviAndPdf :: T.Text -> [T.Text]
dviAndPdf x = [x <> ".pdf", x <> ".dvi"]

indent :: T.Text -> T.Text
indent = T.append "    "

latexDocument :: T.Text -> T.Text
latexDocument base = [st|LaTeXDocument(#{base}, #{base})|]

main :: IO ()
main = do
  set@Settings{..} <- execParser settings
  shelly $ do
    forM_ texDocs $ \fname -> do
      let fp | fname `hasExt` "tex" = fromText fname
             | otherwise           = fname <.> "tex"
      touchfile fp
      when git $ cmd "git" "add" fp
    writefile "OMakeroot" omakeroot
    writefile "OMakefile" $ omakefile set
    when git $ do
      cmd "git" "add" "OMakeroot"
      cmd "git" "add" "OMakefile"
