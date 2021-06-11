{-# LANGUAGE TemplateHaskell #-}

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
    -- instance Recordable (MyEffect m) where
    [ instanceD
        (pure [])
        (appT [t|Recordable|] (appT (conT tyName) (varT (mkName "m"))))
        -- recordKey :: ...
        [ recordKeyMethod tyCons
        , -- recordValue :: ...
          recordValueMethod tyCons
        ]
    ]

recordKeyMethod :: [Con] -> Q Dec
recordKeyMethod cons = funD 'recordKey (map recordKeyClause cons)

recordKeyClause :: Con -> Q Clause
recordKeyClause con = do
  args <- mkArgs con
  clause
    -- recordKey (DataCon a b c) =
    [conP (conNm con) (map varP args)]
    -- toRecordedValue ("DataCon",a,b,c)
    (normalB (appE [e|toRecordedValue|] (toJsonTuple con args)))
    []

recordValueMethod :: [Con] -> Q Dec
recordValueMethod cons = funD 'recordValue (map recordValueClause cons)

recordValueClause :: Con -> Q Clause
recordValueClause con = do
  args <- mkArgs con
  clause
    -- recordKey (DataCon _ _ _) val =
    [conP (conNm con) (map (const wildP) args), [p|resultValue|]]
    -- toRecordedValue val
    (normalB [e|toRecordedValue resultValue|])
    []

toJsonTuple :: Con -> [Name] -> Q Exp
toJsonTuple con nms = tupE ([e|constructor :: String|] : map varE nms)
  where
    constructor = show $ conNm con

conNm :: Con -> Name
conNm (NormalC nm _) = nm
conNm (RecC nm _) = nm
conNm (InfixC _ nm _) = nm
conNm (ForallC _ _ con) = conNm con
conNm (GadtC nms _ _) = head nms
conNm (RecGadtC nms _ _) = head nms

mkArgs :: Con -> Q [Name]
mkArgs (NormalC _ tys) = replicateM (length tys) (newName "a")
mkArgs (RecC _ tys) = replicateM (length tys) (newName "a")
mkArgs InfixC{} = replicateM 2 (newName "a")
mkArgs (ForallC _ _ con) = mkArgs con
mkArgs (GadtC _ tys _) = replicateM (length tys) (newName "a")
mkArgs (RecGadtC _ tys _) = replicateM (length tys) (newName "a")
