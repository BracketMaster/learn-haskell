func a b = 
    a + b + c + d
    where
        c = if False
            then 1
            else case a of
                0 -> 10
                1 -> 20
                _ -> 30
        d = 2

main = putStrLn $ show $ func 1 2