-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
   -- histograma,
   -- Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    --casilleros,
  )
where

import Util
import Data.List

data Histograma = Histograma Float Float [Int]
--1er float -> inicio del intervalo de la segunda casilla
--2do ->  tamaño del intervalo de cada casillero
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l distancia (replicate (n+2) 0)
  where distancia = (u-l) / fromIntegral n

-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma inicio t casilleros) 
  | x < inicio = Histograma inicio t (actualizarElem 0 (+1) casilleros)
  | x > inicio + t*(fromIntegral (length casilleros) - 1) = Histograma inicio t (actualizarElem (fromIntegral (length casilleros) - 1) (+1) casilleros)
  | otherwise = Histograma inicio t (actualizarElem (foldr (\i rec -> if inicio + t*fromIntegral i <= x && inicio + (t+1)*fromIntegral i > x then fromIntegral i else rec) (-1) [0..length casilleros -1]) (+1)  casilleros)


-- verificar si no la usamos, cambiarlo
--encontrarIndice :: Float -> Histograma -> Int
--encontrarIndice e (Histograma min tam l) = foldr (\(min,max) rec -> 
--  if (e >= min && e < max)  
--  then
--  else (rec + 1)
--) 0 [(i + kt, i + (k+1)*t) | k <- [0 .. length cs - 3]]


-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
--histograma :: Int -> (Float, Float) -> [Float] -> Histograma
--histograma cantCasilleros (min,max) numerosAagregar = Histograma min (max-min) 
--                         (foldr (\numeroAagregar rec -> agregar numeroAagregar Histograma min (max,min) rec) (replicate cantCasilleros 0))
--
-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
--                          min  max cant porcentaje
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

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros h = casilleros h 0
  foldr (\cantCasillaI rec -> (Casillero (minimoC min tam) (maximoC min tam) cantCasillaI porcentaje) : rec) [] cantXCasilleros
  where porcentaje = (((sum cantXCasilleros) * cantCasillaI) / 100)
  
  -- [Casillero min max cantidad porcentaje]

casillerosAux :: Histograma -> Int -> [Casillero]
casillerosAux Histograma min tam indice cantXCasilleros = 
  foldr (\cantCasillaI rec -> (Casillero (minimoC min indice tam) (maximoC min indice tam) cantCasillaI porcentaje) : rec) [] cantXCasilleros
  where porcentaje = (((sum cantXCasilleros) * cantCasillaI) / 100)


minimoC :: Int -> Int -> Int
minimoC 

