{-# LANGUAGE TemplateHaskell #-}

module Control.Effect.Replay.TH
  ( deriveReplayable,
  )
where

import Control.Effect.Replay
import Language.Haskell.TH
import Control.Monad (replicateM)
import Data.Aeson
import Data.Aeson.Types (parse)

deriveReplayable :: Name -> Q [Dec]
deriveReplayable tyName = do
  TyConI (DataD _ctx _nm _tyVars _kind tyCons _deriv) <- reify tyName
  sequence
    -- instance Replayable (MyEffect m) where
    [ instanceD
        (pure [])
        (appT [t|Replayable|] (appT (conT tyName) (varT (mkName "m"))))
        -- replay :: ...
        [ replayMethod tyCons
        ]
    ]

replayMethod :: [Con] -> Q Dec
replayMethod cons = funD 'replay (map replayClause cons)

replayClause :: Con -> Q Clause
replayClause con = do
  args <- mkArgs con
  clause
    [conP (conNm con) (map (const wildP) args), [p|resultValue|]]
    (normalB (appE [e|tryDecode|] [e|resultValue|]))
    []

tryDecode :: ReplayableValue a => Value -> Maybe a
tryDecode val =
  case parse fromRecordedValue val of
    Error _ -> Nothing
    Success a -> Just a

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
mkArgs InfixC {} = replicateM 2 (newName "a")
mkArgs (ForallC _ _ con) = mkArgs con
mkArgs (GadtC _ tys _) = replicateM (length tys) (newName "a")
mkArgs (RecGadtC _ tys _) = replicateM (length tys) (newName "a")
