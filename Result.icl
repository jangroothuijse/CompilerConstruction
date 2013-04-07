implementation module Result

isError :: (Result a) -> Bool
isError (Err _) = True
isError _ = False