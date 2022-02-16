

cadastrarComoCliente:: String -> String -> String -> Animal -> IO Cliente
cadastrarComoCliente = do
    putStrLn "\nInsira seu nome:"
    nome <- getLine
    putStrLn "\nInsira seu email:"
    email <- getLine
    putStrLn "\nInsira sua senha:"
    senha <- getLine
    putStrLn "\nInsira seu animais:"
    animais <- getLine





    