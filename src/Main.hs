module Main where

import           ArgParser
import           T2Parser
import           Text.Parsec

import           Language.Dot

import           System.Process

main :: IO ()
main = parseOptions >>= runMain

runMain :: Options -> IO ()
runMain (Options src dest mode) = do
    content <- readFile src
    case parse t2 src content of
        Left e      -> print e
        Right graph -> buildOutput mode dest graph

buildOutput :: Mode -> FilePath -> Graph -> IO ()
buildOutput mode dest graph = do
    writeFile dest (renderDot graph)
    case mode of
        TranslationOnly -> return ()
        Rendering p f   -> callProcess "dot" [renderOpt f, dest, "-o", p]

renderOpt :: Format -> String
renderOpt PS   = "-Tps"
renderOpt PDF  = "-Tpdf"
renderOpt SVG  = "-Tsvg"
renderOpt PNG  = "-Tpng"
renderOpt GIF  = "-Tgif"
renderOpt JPG  = "-Tjpg"
renderOpt JSON = "-Tjson"
