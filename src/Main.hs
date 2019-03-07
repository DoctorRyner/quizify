module Main where

import System.Environment
import System.Process (system)
import qualified Config

mogrify :: String -> String -> String -> String
mogrify targetColor srcColor mask =
    "mogrify -fill " ++ "'" ++ targetColor ++ "'" ++ " -opaque " ++ "'" ++ srcColor ++ "' " ++ mask

quizify :: String -> Bool -> FilePath -> FilePath -> IO ()
quizify accentColor isDark imageSource dist = system cmd >> pure ()
  where
    distMask = dist ++ "/*.{jpg,png}"
    cmd = init . init . init $ concatMap (++ " && ")
        [ "echo 'Replacing colors, output to " ++ dist ++ " ...'"
        , "mkdir -p " ++ dist ++ "/"
        , "cp -r " ++ imageSource ++ "/ " ++ dist
        , mogrify accentColor Config.lightColor distMask
        , if not isDark
            then mogrify Config.darkColor Config.lightColor distMask
            else "echo 'FINISH!'"
        ]

main :: IO ()
main = do
    args <- getArgs
    case length args of
        4 -> quizify (head args) (args !! 1 == "dark") (args !! 2) (args !! 3)
        3 -> quizify (head args) (args !! 1 == "dark") "images" (args !! 2)
        2 -> quizify (head args) True "images" (args !! 1)
        1 ->
            let arg = head args
            in if any (== arg) [ "help", "-help", "--help" ]
                then showHelp
                else quizify (head args) True "images/" "dist/"
        _ -> showHelp

  where showHelp = putStrLn "Usage examples:\n\n4 args: quizify #fff light images/ dist/\n3 args: quizify #fff light dist/\n2 args: quizify #fff dist/\n1 arg:\n  - quizify #fff\n  - quizify help\n  - quizify -help\n  - quizify --help\n\nwhere:\n  - #fff: changeable accent color\n  - light: quiz theme, can be light or dark\n  - images/: source img path\n  - dist/: output directory"