{-# LANGUAGE TemplateHaskell #-}
-- Some partial functions are used safely in this module.
-- This turns their use into a warning, not an error.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Control.Effect.Replay.TH (
  deriveReplayable,
) where

import Control.Applicative ((<|>))
import Control.Effect.Replay
import Control.Monad (replicateM)
import Language.Haskell.TH

deriveReplayable :: Name -> Q [Dec]
deriveReplayable tyName = do
  TyConI (DataD _ctx _nm _tyVars _kind tyCons _deriv) <- reify tyName
  sequence
    -- instance Replayable MyEffect where
    [ instanceD
        (pure [])
        (appT [t|Replayable|] (conT tyName))
        -- replayDecode :: Value -> Value -> Parser (MyEffect Void, Some)
        -- replayDecode = ...
        [ replayDecodeMethod tyCons
        ]
    ]

replayDecodeMethod :: [Con] -> Q Dec
replayDecodeMethod cons = do
  keyValNm <- newName "keyVal"
  valValNm <- newName "valVal"
  -- clauses = [decodeSingle Constructor1, decodeSingle DataConstructor2, ...]
  let clauses = map (decodeSingle keyValNm valValNm) cons
  -- combinedClauses = decodeSingle Constructor1 <|> decodeSingle Constructor2 <|> ...
  let combinedClauses = foldl1 (\a b -> [e|$a <|> $b|]) clauses
  funD
    'replayDecode
    [ clause
        -- replayDecode keyVal valVal = combinedClauses
        [varP keyValNm, varP valValNm]
        (normalB (combinedClauses))
        []
    ]

-- Given some constructor `Bar :: a -> b -> MyGadt Foo`..
decodeSingle :: Name -> Name -> Con -> Q Exp
decodeSingle keyVal valVal con = do
  -- the variable name used for the "result value" of the effect constructor
  resultNm <- newName "decodedResultValue"

  -- constructorArgNames = [a,b]
  constructorArgNames <- mkArgs con

  let -- constructorE = Bar
      constructorE :: ExpQ
      constructorE = conE (conNm con)

  let -- constructorArgsE = [a,b]
      constructorArgsE :: [ExpQ]
      constructorArgsE = map varE constructorArgNames

  let -- conAppliedE = Bar a b
      conAppliedE :: ExpQ
      conAppliedE = foldl1 appE (constructorE : constructorArgsE)

  let -- constructorNameP = ("MyGadt.Bar" :: String)
      constructorNameP :: PatQ
      constructorNameP = sigP (litP (stringL (show (conNm con)))) (conT ''String)

  let -- constructorArgsP = [a,b]
      constructorArgsP :: [PatQ]
      constructorArgsP = map varP constructorArgNames

  let -- constructorTupleP = (("MyGadt.Bar" :: String), a, b)
      constructorTupleP :: PatQ
      constructorTupleP = tupP (constructorNameP : constructorArgsP)

  -- do
  doE
    -- (("MyGadt.Bar" :: String), a, b) <- fromRecordedValue keyVal
    [ bindS constructorTupleP [e|fromRecordedValue $(varE keyVal)|]
    , -- decodedResultValue <- fromRecordedValue valVal
      bindS (sigP (varP resultNm) (getGadtTy con)) [e|fromRecordedValue $(varE valVal)|]
    , -- pure (EffectResult (Bar a b) decodedResultValue)
      noBindS [e|pure (EffectResult $(conAppliedE) $(varE resultNm))|]
    ]

-- | Get the Name of a data constructor
conNm :: Con -> Name
conNm (NormalC nm _) = nm
conNm (RecC nm _) = nm
conNm (InfixC _ nm _) = nm
conNm (ForallC _ _ con) = conNm con
-- 'head' is safe here because that field is documented to be non-empty:
-- https://hackage.haskell.org/package/template-haskell-2.22.0.0/docs/Language-Haskell-TH.html#v:GadtC
conNm (GadtC nms _ _) = head nms
conNm (RecGadtC nms _ _) = head nms

-- | Create a Name for each argument of a data constructor
mkArgs :: Con -> Q [Name]
mkArgs (NormalC _ tys) = replicateM (length tys) (newName "a")
mkArgs (RecC _ tys) = replicateM (length tys) (newName "a")
mkArgs InfixC{} = replicateM 2 (newName "a")
mkArgs (ForallC _ _ con) = mkArgs con
mkArgs (GadtC _ tys _) = replicateM (length tys) (newName "a")
mkArgs (RecGadtC _ tys _) = replicateM (length tys) (newName "a")

-- | Get the last type parameter for a GADT constructor
--
-- data Foo a where
--   Bar :: Foo Int
--
-- getGadtTy on Bar produces Int
getGadtTy :: Con -> TypeQ
getGadtTy (GadtC _ _ (AppT _ ty)) = pure ty
getGadtTy con = fail $ "Expecting a GADT constructor; encountered " <> show con
