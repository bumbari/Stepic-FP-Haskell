untilJust :: Monad m => m (Maybe a) -> m a
untilJust m = go
    where
        go = do
            x <- m
            case x of
                Nothing -> go
                Just x  -> return x

maybeGetName :: IO (Maybe String)
maybeGetName = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  if null name then return Nothing else return (Just name)

main' :: IO ()
main' = do
  name <- untilJust maybeGetName
  putStrLn ("Hi, " ++ name ++ "!")

main :: IO ()
main = main'