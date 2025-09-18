# Ejercicio 12 - Demostración






sacar espeacios aca estoy probando
















## Consigna
Necesitamos demostrar que toda expresión tiene un literal más que su cantidad de operadores. Los literales son las constantes y los rangos. Para esto se dispone de las siguientes definiciones:

```haskell
data Nat = Z | S Nat

suma :: Nat → Nat → Nat
suma Z m = m                                                  -- { S1 }
suma ( S n ) m = S ( suma n m )                               -- { S2 }

cantLit :: Expr → Nat
cantLit ( Const _ ) = S Z                                     -- { L1 }
cantLit ( Rango _ _ ) = S Z                                   -- { L2 }
cantLit ( Suma a b ) = suma ( cantLit a ) ( cantLit b )       -- { L3 }
cantLit ( Resta a b ) = suma ( cantLit a ) ( cantLit b )      -- { L4 }
cantLit ( Mult a b ) = suma ( cantLit a ) ( cantLit b )       -- { L5 }
cantLit ( Div a b ) = suma ( cantLit a ) ( cantLit b )        -- { L6 }

cantOp :: Expr → Nat
cantOp ( Const _ ) = Z                                        -- { O1 }
cantOp ( Rango _ _ ) = Z                                      -- { O2 }
cantOp ( Suma a b ) = S ( suma ( cantOp a ) ( cantOp b ))     -- { O3 }
cantOp ( Resta a b ) = S ( suma ( cantOp a ) ( cantOp b ))    -- { O4 }
cantOp ( Mult a b ) = S ( suma ( cantOp a ) ( cantOp b ))     -- { O5 }
cantOp ( Div a b ) = S ( suma ( cantOp a ) ( cantOp b ))      -- { O6 }
```
La propiedad a demostrar queda expresada de la siguiente manera:
```
∀e :: Expr. cantLit e = S (cantOp e)
```
Se pide:

a. Definir el predicado unario correspondiente a una demostración por inducción estructural (¿en qué estructura?) de esta propiedad.

Vamos a hacer inducción en la estructura de `e :: Expr`.

Predicado unario:
```
P(e) ≡ cantLit e = S (cantOp e)
```
b. Definir el esquema formal de inducción estructural correspondiente a dicha demostración. Incluir todos los cuantificadores necesarios (los cuantificadores son los ∀s y los ∃s).

Para demostrar que la propiedad vale, nos alcanza con demostrar que valen:

    * Casos base: 

        ∀ f :: Float. P(Const f)

        ∀ min :: Float. ∀ max :: Float. P(Rango min max)

    * Casos inductivos:

        ∀ e1 :: Expr. ∀ e2 :: Expr. ((P(e1) ∧ P(e2)) => P(Suma e1 e2))

        ∀ e1 :: Expr. ∀ e2 :: Expr. ((P(e1) ∧ P(e2)) => P(Resta e1 e2))

        ∀ e1 :: Expr. ∀ e2 :: Expr. ((P(e1) ∧ P(e2)) => P(Mult e1 e2))

        ∀ e1 :: Expr. ∀ e2 :: Expr. ((P(e1) ∧ P(e2)) => P(Div e1 e2))
        
c. Demostrar los casos correspondientes a los casos base y al constructor Suma. Los demás casos inductivos son análogos a este último, y por eso les pedimos que no los escriban para este trabajo práctico. En general en la materia siempre tendrán que escribir todos los casos, aunque sean análogos o similares, excepto que les digamos explícitamente que no es necesario.

- Todos los pasos de la demostración deben estar debidamente justificados usando las herramientas que vimos en clase.
- Pueden asumir el siguiente lema como válido. No hace falta demostrarlo:

```
{CONMUT} ∀n, m :: Nat · suma n m = suma m n
```

### Casos base
```
P(Const f) ≡ cantLit (Const f) = S (cantOp (Const f))

cantLit (Const f)
= S Z                          {L1}
= S (cantOp (Const f))         {O1}
```

```
P(Rango min max) ≡ cantLit (Rango min max) = S (cantOp (Rango min max))

cantLit (Rango min max)
= S Z                          {L2}
= S (cantOp (Rango min max))   {O2}
```


### Casos inductivos
```
∀ e1 :: Expr. ∀ e2 :: Expr. ((P(e1) ∧ P(e2)) => P(Suma e1 e2))

HI: (cantLit e1 = S (cantOp e1)) ∧ (cantLit e2 = S (cantOp e2))

TI: P(Suma e1 e2) = cantLit (Suma e1 e2) = S (cantOp (Suma e1 e2))

cantLit (Suma e1 e2)
= suma (cantLit e1) (cantLit e2)           {L3}
= suma (S (cantOp e1)) (S (cantOp e2))     {HI}
= S (suma (cantOp e1) (S (cantOp e2)))     {S2}
= S (suma (S (cantOp e2)) (cantOp e1))     {CONMUT}
= S (S (suma (cantOp e2) (cantOp e1)))     {S2}
= S (S (suma (cantOp e1) (cantOp e2)))     {CONMUT}
= S (cantOp (Suma e1 e2))                  {O3}
```

Luego, el caso inductivo sobre el constructor Suma es válido.

Los demás casos inductivos para los constructores Resta, Mult y Div son análogos al de Suma, como lo aclara la consigna.

Conclusión: La propiedad `P(e) ≡ cantLit e = S (cantOp e)` vale ∀e :: Expr.