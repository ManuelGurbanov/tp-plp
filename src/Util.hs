module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.

alinearDerecha :: Int -> String -> String
alinearDerecha n s = foldr (\_ r -> if n - length r <= 0 then s else ' ' : r) s [1..n - length s]
-- Hacemos un bucle sobre la lista entre 1 y length de s porque no nos cambia el contenido de la lista,
-- si está vacía salta directo al caso base que es s

-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f = zipWith (\i x -> if i == n then f x else x) [0 ..]

-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
