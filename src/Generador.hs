-- | Hay 3 formas de crear un generador:
--
--   1. `genFijo` es un generador que siempre devuelve el valor medio del rango.
--   2. `genNormalConSemilla`, @genNormalConSemilla n@ con @n :: Int@ es un generador que produce números aleatorios con distribución normal. La semilla `n` permite reproducibilidad.
--   3. `genNormal` es un generador que produce números aleatorios con distribución normal usando una semilla aleatoria.
--
-- La última opción no es pura. Dado una función @f :: Gen -> a@ podemos evaluarla usando
-- @f genFijo@, @f (genNormalConSemilla 0)@, @f (genNormalConSemilla 1)@, etc.
--
-- Si queremos evaluar @f@ con un generador aleatorio podemos usar @conGenNormal f@ que devuelve el resultado de f.
-- @conGenNormal@ tampoco es una función pura, pero nos permite aplicar funciones puras en forma cómoda.
--
--
-- En ghci podemos probar:
--
-- @
-- ghci> dameUno (1, 5) genFijo
-- (3.0,<Gen>)
--
-- ghci> dameUno (1, 5) (genNormalConSemilla 0)
-- (2.7980492,<Gen>)
--
-- ghci> conGenNormal (dameUno (1, 5))
-- (1.2677777,<Gen>)
-- ghci> conGenNormal (dameUno (1, 5))
-- (0.3479743,<Gen>)
-- ghci> conGenNormal (dameUno (1, 5))
-- (1.4352515,<Gen>)
-- @
--
-- Siempre que se trabaje con generadores como parámetro de una función vamos a querer
-- devolver otro generador como resultado. Si no perdemos el "nuevo estado" del generador y generariamos
-- el mismo número nuevamente.
--
-- Por ejemplo @dameUno :: (Float, Float) -> Gen -> (Float, Gen)@. La última parte indica que
-- toma un generador y devuelve un número y el generador resultante.
--
-- Se dispone del tipo @G a@ que es un sinónimo de tipo para @Gen -> (a, Gen)@.
-- Usando @G a@ el tipo de `dameUno` queda @dameUno :: (Float, Float) -> G Float@, pero es el mismo tipo!
--
-- El tipo @G a@ es una función que toma un generador y devuelve un valor de tipo @a@ y el generador resultante.
--
-- Dada una función @f :: Gen a@ y @n :: Int@, entonces @muestra f n :: Gen [a]@. Aplica @f@ @n@ veces devolviendo los resultados en una lista.
--
-- La función @rango95 :: [Float] -> (Float, Float)@ toma una secuencia finita de números reales y devuelve un rango
-- con un 95% de confianza que contiene los valores del generador asumiendo que los valores son una muestra de una distribución normal.
-- Si todos los números son iguales, devuelve el rango @(valor-1, valor+1)@.
-- La lista debe tener al menos un elemento.
--
-- Un `Gen` representa un generador de números reales dentro de un rango o intervalo de confianza.
--
-- Usando `dameUno` se puede obtener un número indicando el rango deseado.
--
-- Si @g :: Gen@, @dameUno (a, b) g = (x, g')@ donde @x@ es un número entre @a@ y @b@ con un 95% de confianza
-- y @g'@ es un nuevo `Gen` que debe ser usado para obtener el siguiente número a @x@.
module Generador
  ( Gen,
    G,
    dameUno,
    muestra,
    genFijo,
    genNormalConSemilla,
    genNormal,
    conGenNormal,
    rango95,
  )
where

import qualified Data.Random.Normal as RN
import qualified System.Random as R

newtype Gen = Gen {siguiente :: (Float, Gen)}

instance Show Gen where
  show _ = "<Gen>"

type G a = Gen -> (a, Gen)

-- | Es un generador que siempre devuelve el valor medio del rango.
genFijo :: Gen
genFijo = fromList (repeat 0)

-- | Generador de números aleatorios con distribución normal.
genNormal :: IO Gen
genNormal = fromList <$> RN.normalsIO

-- | Aplica la función a un generador de números aleatorios con distribución normal.
conGenNormal :: (Gen -> a) -> IO a
conGenNormal f = f <$> genNormal

-- | Generador de números aleatorios con distribución normal a partir de una semilla. La semilla se usa para reproducibilidad.
genNormalConSemilla :: Int -> Gen
genNormalConSemilla s = fromList (RN.mkNormals s)

-- | Dados un rango y un generador devuelve un número que está dentro del rango con un 95% de confianza.
-- Es una función total @dameUno (l, u)@ con u < l equivale a @dameUno (u, l)@.
-- Si @l == u@ devuelve @l@.
dameUno :: (Float, Float) -> G Float
dameUno (l, u) g =
  let (x, siguienteGen) = siguiente g
      s = (u - l) / 2.0
      medio = l + s
   in (medio + s / 1.96 * x, siguienteGen)

-- | Aplica una función múltiples veces a un generador.
muestra :: G a -> Int -> G [a]
muestra _ 0 g = ([], g)
muestra f n g = (x : xs, sf)
  where
    (x, s1) = f g
    (xs, sf) = muestra f (n - 1) s1

fromList :: [Float] -> Gen
fromList [] = error "Fin del generador"
fromList (x : xs) = Gen {siguiente = (x, fromList xs)}

-- | Dada una lista finita no vacía de números reales devuelve un
-- rango con un 95% de confianza que contienen los valores de la lista
-- asumiendo que es una muestra de una distribución normal.
-- Si todos los números son iguales, devuelve el rango @(valor-1, valor+1)@.
rango95 :: [Float] -> (Float, Float)
rango95 xs = (promedio - s, promedio + s)
  where
    cantidad = fromIntegral (length xs)
    promedio = sum xs / cantidad
    desviacion = sqrt $ sum [(x - promedio) ^ 2 | x <- xs] / cantidad
    s = if desviacion == 0 then 1 else desviacion * 1.96

testRango95 :: (Float, Float) -> Int -> Gen -> (Float, Float)
testRango95 (l, u) n g = rango95 $ fst $ muestra (dameUno (l, u)) n g

-- >>> conGenNormal (testRango95 (1, 5) 100000)
-- (0.9996053,4.9987354)

-- >>> testRango95 (1, 5) 100 genFijo
-- (2.0,4.0)

-- >>> testRango95 (1, 5) 100000 (genNormalConSemilla 0)
-- (1.0097816,4.998564)

-- >>> testRango95 (1, 5) 100000 (genNormalConSemilla 1)
-- (0.9968296,4.995655)
