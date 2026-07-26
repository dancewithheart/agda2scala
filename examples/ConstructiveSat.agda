module examples.ConstructiveSat where

open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.List using (List; []; _∷_)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)

------------------------------------------------------------------------
-- Boolean operations

not : Bool -> Bool
not false = true
not true  = false
{-# COMPILE AGDA2SCALA not #-}

and : Bool -> Bool -> Bool
and false _ = false
and true  b = b
{-# COMPILE AGDA2SCALA and #-}


or : Bool -> Bool -> Bool
or false b = b
or true  _ = true
{-# COMPILE AGDA2SCALA or #-}

implies : Bool -> Bool -> Bool
implies false _ = true
implies true  b = b
{-# COMPILE AGDA2SCALA implies #-}

-- Three propositional variables

data Variable : Set where
  p q r : Variable
{-# COMPILE AGDA2SCALA Variable #-}

record Environment : Set where
  constructor newEnv
  field
    pValue : Bool
    qValue : Bool
    rValue : Bool
{-# COMPILE AGDA2SCALA Environment #-}

open Environment

lookup : Environment -> Variable -> Bool
lookup environment p = pValue environment
lookup environment q = qValue environment
lookup environment r = rValue environment
{-# COMPILE AGDA2SCALA lookup #-}

-- Propositional formulas

data Formula : Set where
  atom : Variable -> Formula
  neg  : Formula -> Formula
  conj : Formula -> Formula -> Formula
  disj : Formula -> Formula -> Formula
  impl : Formula -> Formula -> Formula
{-# COMPILE AGDA2SCALA Formula #-}

evaluate : Environment -> Formula -> Bool
evaluate env (atom var)   = lookup env var
evaluate env (neg f)      = not (evaluate env f)
evaluate env (conj lf rf) = and (evaluate env lf) (evaluate env rf)
evaluate env (disj lf rf) = or (evaluate env lf) (evaluate env rf)
evaluate env (impl lf rf) = implies (evaluate env lf) (evaluate env rf)
{-# COMPILE AGDA2SCALA evaluate #-}

-- All valuations of p, q and r
allEnvironments : List Environment
allEnvironments =
  newEnv false false false ∷
  newEnv false false true  ∷
  newEnv false true  false ∷
  newEnv false true  true  ∷
  newEnv true  false false ∷
  newEnv true  false true  ∷
  newEnv true  true  false ∷
  newEnv true  true  true  ∷
  []
{-# COMPILE AGDA2SCALA allEnvironments #-}

-- Solver
findSolution : Formula -> List Environment -> Maybe Environment
findSolution formula [] = nothing
findSolution formula (environment ∷ environments)
  with evaluate environment formula
... | true = just environment
... | false = findSolution formula environments
{-# COMPILE AGDA2SCALA findSolution #-}

solve : Formula -> Maybe Environment
solve formula = findSolution formula allEnvironments
{-# COMPILE AGDA2SCALA solve #-}

-- Example formulas
exampleSat : Formula
exampleSat = conj (atom p) (neg (atom q))
{-# COMPILE AGDA2SCALA exampleSat #-}

exampleUnsat : Formula
exampleUnsat = conj (atom p) (neg (atom p))
{-# COMPILE AGDA2SCALA exampleUnsat #-}
