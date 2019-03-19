{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Config
import           Control.Monad      (when)
import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Json
import           System.Environment
import           System.Process     (system)

colorifyText :: String -> Text
colorifyText color = Text.pack $ (if head color /= '#' then "#" else "") <> color

colorify :: String -> String
colorify color = (if head color /= '#' then "#" else "") <> color

mogrify :: String -> String -> String -> String
mogrify targetColor sourceColor mask =
    "mogrify -fill " ++ "'" ++ target ++ "'" ++ " -opaque " ++ "'" ++ source ++ "' " ++ mask
  where target = colorify targetColor
        source = colorify sourceColor

makeConfig :: String -> String -> Text -> ByteString
makeConfig accentColor _lightOrDarkColor email = json
    [ "accentColor"  |: str (colorifyText accentColor)
    , "lightOrDark"  |: str (colorifyText Config.darkColor)
    , "isRoundStyle" |: bool False
    , "email"        |: str email
    ]

quizify :: String -> Bool -> String -> FilePath -> FilePath -> FilePath -> IO ()
quizify accentColor isDark email logoPath imageSource dist = do
    _ <- system cmd

    when (logoPath /= "") $
        system ("cp " ++ imageSource ++ "/../*.json " ++ dist ++ "../locales/") >> pure ()

    _ <- system $ "cp " ++ logoPath ++ " " ++ dist ++ "../images/"
    Json.writeJson
        (dist ++ "../locales/config.json")
        (makeConfig
            accentColor
            (if isDark then Config.darkColor else Config.lightColor)
            (Text.pack email)
        )
  where
    distMask = dist ++ "/*.{jpg,png}"
    cmd = init . init . init $ concatMap (++ " && ")
        [ "echo 'Replacing colors, output to " ++ dist ++ " ...'"
        , "mkdir -p " ++ dist ++ "/ " ++ dist ++ "/.." ++ "/locales/"
        , "cp -r " ++ imageSource ++ "/ " ++ dist
        , mogrify accentColor Config.lightColor distMask
        , if not isDark
            then mogrify Config.lightColor Config.darkColor distMask ++ " && echo 'FINISH!'"
            else "echo 'FINISH!'"
        ]

main :: IO ()
main = do
    args <- getArgs
    case length args of
        6 -> quizify
            (head args) -- accentColor
            (args !! 1 == "dark") -- lightOrDark
            (args !! 2) -- email
            (args !! 3) -- logoPath
            (args !! 4) -- imgSource
            (args !! 5 ++ "/images/") -- distPath
        5 -> quizify
            (head args)
            (args !! 1 == "dark")
            (args !! 2)
            ""
            (args !! 3)
            (args !! 4 ++ "/images/")

        4 -> quizify
            (head args)
            (args !! 1 == "dark")
            (args !! 2)
            ""
            "~/quiziy/images"
            (args !! 3 ++ "/images/")
        3 -> quizify
            (head args)
            (args !! 1 == "dark")
            (args !! 2)
            ""
            "~/quizify/images"
            "dist/"
        2 -> quizify
            (head args)
            True
            (args !! 1)
            ""
            "~/quizify/images"
            "dist/"
        1 ->
            let arg = head args
            in if any (== arg) [ "help", "-help", "--help" ]
                then showHelp
                else quizify
                    (head args)
                    True
                    "rg@kurbikus.digital"
                    ""
                    "images/"
                    "dist/"
        _ -> showHelp

  where
    template  = "The template: quizify <#color> <dark|light> <email> <logoPath> <sourcePath> <distPath>"
    help6Args = "6 args: quizify #fff light test@test logo.png images/ dist/"
    helpMsg = template
    showHelp  = putStrLn "Usage examples:\n\n4 args: quizify #fff light images/ dist/\n3 args: quizify #fff light dist/\n2 args: quizify #fff dist/\n1 arg:\n  - quizify #fff\n  - quizify help\n  - quizify -help\n  - quizify --help\n\nwhere:\n  - #fff: changeable accent color\n  - light: quiz theme, can be light or dark\n  - images/: source img path\n  - dist/: output directory"
