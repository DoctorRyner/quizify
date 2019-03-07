module Main where

import System.Environment
import System.Process (system)
import qualified Config

quizify :: String -> Bool -> FilePath -> FilePath -> IO ()
quizify accentColor isDark imageSource dist = do
    let distMask = dist ++ if last dist /= '/' then "/*.{jpg,png}" else "*.{jpg,png}"
        cmd = concat
            [ "mkdir -p " ++ dist ++ if last dist /= '/' then " && " else "/ && "
            , "cp -r " ++ imageSource ++ if last imageSource /= '/' then "/ " else " " ++ dist
            , "mogrify -fill "
                ++ accentColor
                ++ " -opaque "
                ++ Config.darkColor
                ++ distMask
            , if not isDark
                then "mogrify -fill "
                    ++ Config.darkColor
                    ++ " -opaque "
                    ++ Config.lightColor
                    ++ distMask
                else ""
            ]
    _ <- system cmd
    pure ()

{-
replaceAccentColor: mogrify -fill <accentColor> -opaque <light> <distPath>*.{jpg,png}

replaceDarkToLight: mogrify -fill <light> -opaque <dark> <distPath>*.{jpg,png}
replaceLightToDark: mogrify -fill <dark> -opaque <light> <distPath>*.{jpg,png}
-}

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
                else quizify (args !! 0) True "images" "dist"
        _ -> showHelp

  where showHelp = putStrLn "Usage examples:\n\n4 args: quizify #fff light images/ dist/\n3 args: quizify #fff light dist/\n2 args: quizify #fff dist/\n1 arg:\n  - quizify #fff\n  - quizify help\n  - quizify -help\n  - quizify --help\n\nwhere:\n  - #fff: changeable accent color\n  - light: quiz theme, can be light or dark\n  - images/: source img path\n  - dist/: output directory"