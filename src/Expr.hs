{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    -- eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)


-- | Ejercicio 7 |
recrExpr :: (Float -> b) -> (Float -> Float -> b) -> (Expr -> b -> Expr -> b -> b) -> (Expr -> b -> Expr -> b -> b) -> (Expr -> b -> Expr -> b -> b) -> (Expr -> b -> Expr -> b -> b) -> Expr -> b
recrExpr fConst fRango fSuma fResta fMult fDiv exp = case exp of
    Const a -> fConst a
    Rango a b -> fRango a b
    Suma expr1 expr2 -> fSuma expr1 (rec expr1) expr2 (rec expr2)
    Resta expr1 expr2 -> fResta expr1 (rec expr1) expr2 (rec expr2)
    Mult expr1 expr2 -> fMult expr1 (rec expr1) expr2 (rec expr2)
    Div expr1 expr2 -> fDiv expr1 (rec expr1) expr2 (rec expr2)
  where rec = recrExpr fConst fRango fSuma fResta fMult fDiv

foldExpr :: (Float -> b) -> (Float -> Float -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b
foldExpr fConst fRango fSuma fResta fMult fDiv exp = case exp of
    Const a -> fConst a
    Rango a b -> fRango a b
    Suma expr1 expr2 -> fSuma (rec expr1) (rec expr2)
    Resta expr1 expr2 -> fResta (rec expr1) (rec expr2)
    Mult expr1 expr2 -> fMult (rec expr1) (rec expr2)
    Div expr1 expr2 -> fDiv (rec expr1) (rec expr2)
  where rec = foldExpr fConst fRango fSuma fResta fMult fDiv


-- | Ejercicio 8 |
-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> Gen -> (Float, Gen)
eval = foldExpr (\a gen -> (a, gen))
                (\a b gen -> dameUno (a, b) gen)
                (\ev1 ev2 gen -> (fst (ev1 gen) + fst (ev2 (snd (ev1 gen))), snd (ev2 (snd (ev1 gen)))))
                (\ev1 ev2 gen -> (fst (ev1 gen) - fst (ev2 (snd (ev1 gen))), snd (ev2 (snd (ev1 gen)))))
                (\ev1 ev2 gen -> (fst (ev1 gen) * fst (ev2 (snd (ev1 gen))), snd (ev2 (snd (ev1 gen)))))
                (\ev1 ev2 gen -> (fst (ev1 gen) / fst (ev2 (snd (ev1 gen))), snd (ev2 (snd (ev1 gen)))))
{-
  Dudas sobre este ejercicio:
    Más allá de la declaratividad y legibilidad, ¿es correcto? ¿el generador que devolvemos
    como segunda coordenada de las funciones de casos recursivos, ¿cambia efectivamente 
    porque a fin de cuentas el del caso base de Rango cambia?
-}


-- eval (Rango a b) gen = dameUno (a, b) gen
-- eval (Suma expr1 expr2) gen = (fst ev1 + fst ev2, snd ev2)
-- eval (Suma expr1 expr2) gen = (fst ev1 + fst ev2, snd ev2)
-- eval (Suma expr1 expr2) gen = (fst ev1 + fst ev2, snd ev2)
-- eval (Suma expr1 expr2) gen = (fst ev1 + fst ev2, snd ev2)
--   where ev1 = eval expr1 gen
--         ev2 = eval expr2 (snd ev1)

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = error "COMPLETAR EJERCICIO 9"

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = error "COMPLETAR EJERCICIO 10"

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
