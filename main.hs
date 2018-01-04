import LVM.Parse
import LVM.Reduce
import System.Console.Readline
import Data.Map as Map

main :: IO ()
main = main' $ Map.fromList [("succ", succ), ("+", plus), ("pred", pred), ("iszero", iszero)]
    where ("", succ)    = parse "\\z -> \\f -> \\x -> f (z f x)"
          ("", plus)    = parse "\\m -> \\n -> \\f -> \\x -> m f (n f x)"
          ("", pred)    = parse "\\n -> \\f -> \\x -> n (\\g -> \\h -> h (g f)) (\\u -> x) (\\u -> u)"
          ("", iszero)  = parse "\\n -> n (\\x -> False) True"

main' :: Env -> IO ()
main' env = do
    str <- readline "λir> "
    case str of
        Just x -> do
            let res = parse x
            case res of
                ("", x) -> do
                    putStrLn $ show $ reduce env x
                    main' env
                (name, value) -> main' (insert name (reduce env value) env)
        Nothing -> putStrLn $ "\nGoodbye!"