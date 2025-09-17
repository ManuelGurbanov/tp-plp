module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Data.List
import Util

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Ejercicio 3 |
-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l distancia (replicate (n + 2) 0)
  where
    distancia = (u - l) / fromIntegral n


-- | Ejercicio 4 |
-- | Agrega un valor al histograma
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma inicio salto casilleros) = Histograma inicio salto (actualizarElem obtenerIndice (+1) casilleros)
  where obtenerIndice = max (min (length casilleros - 1) (floor ((x - inicio)/salto) + 1)) 0


-- | Ejercicio 5 |
-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma cant rango = foldr (\n rec -> agregar n rec) (vacio cant rango)


-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p


-- | Ejercicio 6 |
-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros (Histograma inicio salto casillas) = zipWith4 (\min max cant porc -> Casillero min max cant porc) listaMins listaMaxs casillas listaPorcs
  where listaMins  = infinitoNegativo : [inicio, inicio+salto .. inicio + salto * fromIntegral (length casillas - 2)]
        listaMaxs  = [inicio, inicio+salto .. inicio + salto * fromIntegral (length casillas - 2) ] ++ [infinitoPositivo]
        listaPorcs =  map (\cantCasilleros -> if sum casillas == 0 then 0 else fromIntegral (100 * cantCasilleros) / fromIntegral (sum casillas)) casillas
