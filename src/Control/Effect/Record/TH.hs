{-# LANGUAGE TemplateHaskell #-}
-- Some partial functions are used safely in this module, so don't warn or error.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Control.Effect.Record.TH (
  deriveRecordable,
) where

import Control.Effect.Record
import Control.Monad (replicateM)
import Language.Haskell.TH

-- | For the given effect type, derive an instance of 'Recordable'
deriveRecordable :: Name -> Q [Dec]
deriveRecordable tyName = do
  TyConI (DataD _ctx _nm _tyVars _kind tyCons _deriv) <- reify tyName
  sequence
    -- instance Recordable MyEffect where
    [ instanceD
        (pure [])
        (appT [t|Recordable|] (conT tyName))
        [ -- recordEff :: ...
          recordEffMethod tyCons
        ]
    ]

recordEffMethod :: [Con] -> DecQ
recordEffMethod cons = funD 'recordEff (map recordEffClause cons)

recordEffClause :: Con -> ClauseQ
recordEffClause con = do
  args <- mkArgs con
  resultValueNm <- newName "resultValue"
  clause
    -- recordKey (DataCon a b c) resultValue =
    [conP (conNm con) (map varP args), varP resultValueNm]
    -- (toRecordedValue ("DataCon",a,b,c), toRecordedValue resultValue)
    (normalB [e|(toRecordedValue $(toJsonTuple con args), toRecordedValue $(varE resultValueNm))|])
    []

toJsonTuple :: Con -> [Name] -> ExpQ
toJsonTuple con nms =
  case nms of
    -- When a constructor has no arguments, don't form a tuple over it. It will
    -- otherwise build a "Unit tuple"(?!) over it with one element
    [] -> [e|constructor :: String|]
    _ -> tupE ([e|constructor :: String|] : map varE nms)
  where
    constructor = show $ conNm con

conNm :: Con -> Name
conNm (NormalC nm _) = nm
conNm (RecC nm _) = nm
conNm (InfixC _ nm _) = nm
conNm (ForallC _ _ con) = conNm con
-- 'head' is safe here because that field is documented to be non-empty:
-- https://hackage.haskell.org/package/template-haskell-2.22.0.0/docs/Language-Haskell-TH.html#v:GadtC
conNm (GadtC nms _ _) = head nms
conNm (RecGadtC nms _ _) = head nms

mkArgs :: Con -> Q [Name]
mkArgs (NormalC _ tys) = replicateM (length tys) (newName "a")
mkArgs (RecC _ tys) = replicateM (length tys) (newName "a")
mkArgs InfixC{} = replicateM 2 (newName "a")
mkArgs (ForallC _ _ con) = mkArgs con
mkArgs (GadtC _ tys _) = replicateM (length tys) (newName "a")
mkArgs (RecGadtC _ tys _) = replicateM (length tys) (newName "a")
