{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Config
import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Json
import           System.Environment
import           System.Process     (system)

colorifyText :: String -> Text
colorifyText color  = Text.pack $ (if head color /= '#' then "#" else "") <> color

colorify :: String -> String
colorify color  = (if head color /= '#' then "#" else "") <> color

mogrify :: String -> String -> String -> String
mogrify targetColor sourceColor mask =
    "mogrify -fill " ++ "'" ++ target ++ "'" ++ " -opaque " ++ "'" ++ source ++ "' " ++ mask
  where target = colorify targetColor
        source = colorify sourceColor

makeConfig :: String -> String -> ByteString
makeConfig accentColor lightOrDarkColor = json
    [ "accentColor"  |: str (colorifyText accentColor)
    , "lightOrDark"  |: str (colorifyText lightOrDarkColor)
    , "isRoundStyle" |: bool False
    , "url"          |: "https://cyberforeman.com/"
    , "api"          |: "api/basic"
    ]

quizify :: String -> Bool -> FilePath -> FilePath -> IO ()
quizify accentColor isDark imageSource dist = do
    _ <- system cmd
    Json.writeJson
        (dist ++ "../locales/config.json")
        (makeConfig accentColor (if isDark then Config.darkColor else Config.lightColor)
        )
  where
    distMask = dist ++ "/*.{jpg,png}"
    cmd = init . init . init $ concatMap (++ " && ")
        [ "echo 'Replacing colors, output to " ++ dist ++ " ...'"
        , "mkdir -p " ++ dist ++ "/ " ++ dist ++ "/.." ++ "/locales/"
        , "cp -r " ++ imageSource ++ "/ " ++ dist
        , mogrify accentColor Config.lightColor distMask
        , if not isDark
            then mogrify Config.lightColor Config.darkColor distMask
            else "echo 'FINISH!'"
        ]

main :: IO ()
main = do
    args <- getArgs
    case length args of
        4 -> quizify (head args) (args !! 1 == "dark") (args !! 2) (args !! 3 ++ "/images/")
        3 -> quizify (head args) (args !! 1 == "dark") "images" (args !! 2 ++ "/images/")
        2 -> quizify (head args) True "images" (args !! 1 ++ "/images/")
        1 ->
            let arg = head args
            in if any (== arg) [ "help", "-help", "--help" ]
                then showHelp
                else quizify (head args) True "images/" "dist/"
        _ -> showHelp

  where showHelp = putStrLn "Usage examples:\n\n4 args: quizify #fff light images/ dist/\n3 args: quizify #fff light dist/\n2 args: quizify #fff dist/\n1 arg:\n  - quizify #fff\n  - quizify help\n  - quizify -help\n  - quizify --help\n\nwhere:\n  - #fff: changeable accent color\n  - light: quiz theme, can be light or dark\n  - images/: source img path\n  - dist/: output directory"
