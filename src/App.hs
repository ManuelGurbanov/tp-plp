module App where

import Expr
import Expr.Parser
import Generador
import Histograma
import qualified Numeric
import Util

-- | Función auxiliar para probar el histograma con generadores de números aleatorios.
probarHistograma :: (Float, Float) -> Int -> Gen -> Histograma
probarHistograma rango cantidadMuestras g =
  fst $ armarHistograma 11 cantidadMuestras (dameUno rango) g

-- Desde el repl
--
-- % make repl
-- ghci> probarHistograma (1, 5) 100000 (genNormalConSemilla 0)
-- Histograma 1.0030481 0.36353552 [2492,2980,5165,7968,10871,13425,14175,13315,10957,7967,5291,2900,2494]

-- O directamente con eval-plugin desde el editor:
-- >>> probarHistograma (1, 5) 100000 (genNormalConSemilla 0)
-- Histograma 1.0030481 0.36353552 [2492,2980,5165,7968,10871,13425,14175,13315,10957,7967,5291,2900,2494]

-- >>> conGenNormal (probarHistograma (1, 5) 100)
-- Histograma 0.7638993 0.40677726 [4,4,4,5,8,13,17,19,8,7,7,1,3]

-- ¿Cuántas muestras serán necesarias para que, independientemente de la semilla, el histograma
-- resultante sea más o menos el mismo?
-- >>> probarHistograma (1, 5) 100 (genNormalConSemilla 1)
-- >>> probarHistograma (1, 5) 100 (genNormalConSemilla 2)
-- Histograma 0.93081164 0.347976 [2,4,6,5,11,17,13,12,9,9,6,4,2]
-- Histograma 0.69377136 0.380056 [1,4,4,10,14,13,8,17,13,6,4,3,3]

-- >>> probarHistograma (1, 5) 100000 (genNormalConSemilla 1)
-- >>> probarHistograma (1, 5) 100000 (genNormalConSemilla 2)
-- Histograma 0.9968296 0.3635296 [2531,2920,5207,7938,11088,13217,14180,13206,11121,7886,5312,2894,2500]
-- Histograma 0.99077463 0.3648204 [2587,2980,5128,7935,10882,13265,14180,13359,11033,8026,5220,2957,2448]

-- También podemos ver los casilleros resultantes
-- >>> casilleros (probarHistograma (1, 5) 100 (genNormalConSemilla 0))
-- [Casillero (-Infinity) 1.2849519 1 1.0,Casillero 1.2849519 1.6316414 6 6.0,Casillero 1.6316414 1.9783309 7 7.0,Casillero 1.9783309 2.3250203 9 9.0,Casillero 2.3250203 2.67171 7 7.0,Casillero 2.67171 3.0183992 9 9.0,Casillero 3.0183992 3.365089 14 14.0,Casillero 3.365089 3.7117784 14 14.0,Casillero 3.7117784 4.058468 15 15.000001,Casillero 4.058468 4.405157 7 7.0,Casillero 4.405157 4.751847 6 6.0,Casillero 4.751847 5.0985365 3 3.0,Casillero 5.0985365 Infinity 2 2.0]

-- >>> conGenNormal (casilleros . probarHistograma (1, 5) 100)
-- [Casillero (-Infinity) 1.021881 3 3.0,Casillero 1.021881 1.3792903 2 2.0,Casillero 1.3792903 1.7366998 4 4.0,Casillero 1.7366998 2.094109 9 9.0,Casillero 2.094109 2.4515185 11 11.0,Casillero 2.4515185 2.808928 17 17.0,Casillero 2.808928 3.1663375 13 13.0,Casillero 3.1663375 3.5237465 12 12.0,Casillero 3.5237465 3.881156 8 8.0,Casillero 3.881156 4.2385654 10 10.0,Casillero 4.2385654 4.595975 5 5.0,Casillero 4.595975 4.9533844 3 3.0,Casillero 4.9533844 Infinity 3 3.0]

-- O usar mostrarHistrograma para verlo en pantalla directamente.
--
-- % make repl
-- ghci> conGenNormal (mostrarHistograma . probarHistograma (1, 5) 100) >>= putStrLn
-- ghci> conGenNormal (mostrarHistograma . probarHistograma (1, 5) 100000) >>= putStrLn

cantidadDeMuestras :: Int
cantidadDeMuestras = 100000

mostrarHistograma :: Histograma -> String
mostrarHistograma h =
  unlines (map mostrarCasillero cs)
  where
    cs = reverse (casilleros h)

    mostrarLimites :: Casillero -> String
    mostrarLimites c = mostrarFloat (casMinimo c) ++ " - " ++ mostrarFloat (casMaximo c)

    espacioLimites :: Int
    espacioLimites = maximum (map (length . mostrarLimites) cs)

    mostrarCasillero :: Casillero -> String
    mostrarCasillero c =
      concat
        [ alinearDerecha espacioLimites (mostrarLimites c),
          " |",
          replicate (tamBarra c) '▒',
          if casPorcentaje c == maxPorcentaje
            then
              " " ++ mostrarFloat (casPorcentaje c) ++ "%"
            else
              ""
        ]

    maxPorcentaje :: Float
    maxPorcentaje = maximum (map casPorcentaje cs)

    tamBarra :: Casillero -> Int
    tamBarra c =
      if maxPorcentaje == 0
        then 0
        else round (30 * casPorcentaje c / maxPorcentaje)

mostrarFloat :: Float -> String
mostrarFloat x =
  if isInfinite x
    then if x > 0 then "+inf" else "-inf"
    else Numeric.showGFloat (Just 2) x ""
