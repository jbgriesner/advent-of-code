module Utils (divide) where

divide :: Int -> IO ()
divide s = putStrLn $ "--- result of day " <> show s <> " is:"
