module ArgParser where

import           Data.Monoid
import           Options.Applicative

data Options = Options { inputFile       :: FilePath
                       , outputFile      :: FilePath
                       , applicationMode :: Mode
                       }
data Mode = TranslationOnly | Rendering FilePath Format
data Format = PS | PDF | SVG | PNG | GIF | JPG | JSON

parseOptions :: IO Options
parseOptions = execParser optionParser

optionParser :: ParserInfo Options
optionParser = info (options <**> helper)
    (  fullDesc
    <> progDesc "Converts T2-transition-system files (*.t2) to DOT-files, and renders them.")

options :: Parser Options
options = Options <$> inFile <*> outFile <*> mode

inFile :: Parser FilePath
inFile = strOption
    (  long "input"
    <> short 'i'
    <> metavar "FILENAME"
    <> help "Input file")

outFile :: Parser FilePath
outFile = strOption
    (  long "output"
    <> short 'o'
    <> metavar "FILENAME"
    <> help "Output file")

mode :: Parser Mode
mode = translationOnly <|> rendering

translationOnly :: Parser Mode
translationOnly = flag TranslationOnly TranslationOnly
    (  long "translation-only"
    <> short 't'
    <> help "Only translate and dont render (default behaviour)")

rendering :: Parser Mode
rendering = Rendering <$> renderTarget <*> format

renderTarget :: Parser FilePath
renderTarget = strOption
    (  long "rendering-output"
    <> short 'r'
    <> metavar "FILENAME"
    <> help "Render DOT-file to another format")

format :: Parser Format
format = psFlag <|> pdfFlag <|> svgFlag <|> pngFlag <|> gifFlag <|> jpgFlag <|> jsonFlag

psFlag :: Parser Format
psFlag = flag' PS
    (  long "ps"
    <> help "Render to PostScript")

pdfFlag :: Parser Format
pdfFlag = flag' PDF
    (  long "pdf"
    <> help "Render to PDF")

svgFlag :: Parser Format
svgFlag = flag' SVG
    (  long "svg"
    <> help "Render to SVG")

pngFlag :: Parser Format
pngFlag = flag' PNG
    (  long "png"
    <> help "Render to PNG")

gifFlag :: Parser Format
gifFlag = flag' GIF
    (  long "gif"
    <> help "Render to GIF")

jpgFlag :: Parser Format
jpgFlag = flag' JPG
    (  long "jpg"
    <> help "Render to JPG")

jsonFlag :: Parser Format
jsonFlag = flag' JSON
    (  long "json"
    <> help "Render to JSON")
