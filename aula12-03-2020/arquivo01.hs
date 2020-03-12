umaRaiz :: Float -> Float -> Float -> Float
umaRaiz a b c = -b / (2.0*a)

duasRaizes :: Float -> Float -> Float -> (Float, Float)
duasRaizes a b c = (d-e, d+e)
    where
        d = -b / (2.0*a)
        e = sqrt(b^2 - 4.0*a*c) / (2.0*a)

segundoGrau :: Float -> Float -> Float -> String
segundoGrau a b c
 | (b^2) > (4.0 * a * c) = show f ++ " " ++ show s
 | (b^2) == (4.0 * a * c) = show (umaRaiz a b c)
 | otherwise = "Nao ha raizes"
    where 
        (f,s) = (duasRaizes a b c)

-- Sinonimos de tipo

type Nome = String
type Idade = Int
type Pessoa = (Nome, Idade)

nome :: Pessoa -> Nome
nome (n, _) = n

