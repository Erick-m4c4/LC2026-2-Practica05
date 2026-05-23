module Practica05 where

import Terminos

-- Sustituimos un termino
-- Si el término es una variable, busca si está en la sustitución y la reemplaza.
-- Si es una función, se sustituyen los argumentos
apsubT :: Term -> Subst -> Term
apsubT (Var x) s =
    case lookup x s of
        Just t  -> t
        Nothing -> Var x
apsubT (Fun f args) s = Fun f (aplicarLista args s)

-- Aplica una sustitución a cada término
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista ts s = map (`apsubT` s) ts

-- Elimina de una sustitución todos los pares triviales
simpSus :: Subst -> Subst
simpSus = filter (\(x, t) -> t /= Var x)

-- Calcula la composición σ1σ2 de dos sustituciones.
-- primero aplica σ1, luego σ2":
-- Actualiza los términos de σ1 aplicándoles σ2.
-- Agrega los pares de σ2 cuyas variables no están en el dominio de σ1.
-- Luego simplifica eliminando pares triviales.
compSus :: Subst -> Subst -> Subst
compSus s1 s2 =
    simpSus $ s1actualizada ++ s2filtrada
  where
    -- Aplica s2 a cada s1
    s1actualizada = [(x, apsubT t s2) | (x, t) <- s1]
    domS1 = map fst s1
    s2filtrada = [(x, t) | (x, t) <- s2, x `notElem` domS1]

-- | Unifica dos términos. Devuelve una lista o lista vacia si no se pueden unir
unifica :: Term -> Term -> [Subst]
unifica (Var x) (Var y)
    | x == y    = [[]]                  
    | otherwise = [[( x, Var y)]]       -- sustituir x por y
unifica (Var x) t
    | ocurre x t = []                   
    | otherwise  = [[(x, t)]]
unifica t (Var x)
    | ocurre x t = []
    | otherwise  = [[(x, t)]]
unifica (Fun f args1) (Fun g args2)
    | f /= g                        = []   -- distintos nombres de función
    | length args1 /= length args2  = []   
    | otherwise                     = unificaListas args1 args2

-- Occur check: verifica si la variable x aparece en el termino t.
ocurre :: Nombre -> Term -> Bool
ocurre x (Var y)      = x == y
ocurre x (Fun _ args) = any (ocurre x) args

-- Unifica dos listas de componente a componente,
-- componiendo las sustituciones parciales.
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas [] [] = [[]]
unificaListas (t1:ts1) (t2:ts2) =
    case unifica t1 t2 of
        []    -> []
        [mu1] ->
            -- Aplica mu1 al resto y unifica
            case unificaListas (aplicarLista ts1 mu1) (aplicarLista ts2 mu1) of
                []    -> []
                [mu2] -> [compSus mu1 mu2]
                _     -> []
        _ -> []
unificaListas _ _ = []

-- Unifica un conjunto de terminos.
-- Estrategia: unifica los dos primeros, aplica el unificador al resto y repite
unificaConj :: [Term] -> [Subst]
unificaConj []  = [[]]
unificaConj [_] = [[]]
unificaConj (t1:t2:ts) =
    case unifica t1 t2 of
        []    -> []
        [mu1] ->
            let resto = aplicarLista (t2:ts) mu1
            in case unificaConj resto of
                []    -> []
                [mu2] -> [compSus mu1 mu2]
                _     -> []
        _ -> []
