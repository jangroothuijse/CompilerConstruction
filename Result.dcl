definition module Result

:: Result a = Res a | Err Errors
:: Errors :== [String]

isError :: (Result a) -> Bool