{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use for_" #-}
module Data.Toml.Extra (
  tableMap'
) where

import Toml.Codec.BiMap (BiMap (..), TomlBiMap)
import Toml.Codec.Code (execTomlCodec)
import Toml.Codec.Combinator.Common (whenLeftBiMapError)
import Toml.Codec.Types (Codec (..), TomlCodec, TomlEnv, TomlState (..))
import Toml.Type.Key
import Toml.Type.TOML (TOML (..), insertTable, insertTableArrays)
import qualified Toml.Type.PrefixTree as Prefix
import Control.Applicative (empty)
import Control.Monad (forM_)
import Control.Monad.State (gets, modify)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import Validation (Validation (..))
import Data.Map.Strict (Map)
import Prelude hiding (lookup)
import Debug.Trace (trace)
import qualified Data.List.NonEmpty as NE
import Toml.Codec (TomlDecodeError)
-- import qualified Strategy.Python.Poetry.PyProject as group.group.dev.dev

-- tables :: 

applicableKey :: Show a => Key -> Prefix.PrefixMap a -> [Key]
applicableKey key prefMap = map simplify $ filter (compareKey' key) allKeys
    where
        listed = trace ("input.simplify.listed': " ++ show (Prefix.toList prefMap)) $ Prefix.toList prefMap

        allKeys :: [Key]
        allKeys =  trace ("input.simplify.allKeys': " ++ show (map (simplify . fst) $ Prefix.toList prefMap)) $ map (simplify . fst) $ listed

        simplify :: Key -> Key
        simplify key' = trace ("input.simplify': " ++ show key') $ Key . removeConsecutiveDuplicates . unKey $ key'

removeConsecutiveDuplicates :: Eq a => NE.NonEmpty a -> NE.NonEmpty a
removeConsecutiveDuplicates (x NE.:| []) = x NE.:| []
removeConsecutiveDuplicates (x NE.:| (y:ys))
    | x == y    = removeConsecutiveDuplicates (x NE.:| ys)
    | otherwise = case NE.nonEmpty (y:ys) of
        Just ne -> NE.singleton x <> (removeConsecutiveDuplicates ne)
        _ -> x NE.:| []

tableMap'
    :: forall k v
    .  (Ord k, Show k, Show v)
    => TomlBiMap Key k
    -- ^ Bidirectional converter between TOML 'Key's and 'Map' keys
    -> (Key -> TomlCodec v)
    -- ^ Codec for 'Map' values for the corresponding 'Key'
    -> Key
    -- ^ Table name for 'Map'
    -> TomlCodec (Map k v)
tableMap' = internalTableMap Map.empty Map.toList Map.fromList Map.unions

-- type TomlEnv a = TOML -> Validation [TomlDecodeError] a

internalTableMap
    :: forall map k v
    .  Show map => map  -- ^ empty map
    -> (map -> [(k, v)])  -- ^ toList function
    -> ([(k, v)] -> map)  -- ^ fromList function
    -> ([map] -> map) -- ^ union function
    -> TomlBiMap Key k
    -- ^ Bidirectional converter between TOML 'Key's and Map keys
    -> (Key -> TomlCodec v)
    -- ^ Codec for Map values for the corresponding 'Key'
    -> Key
    -- ^ Table name for Map
    -> TomlCodec map
internalTableMap emptyMap toListMap fromListMap unionsMap keyBiMap valCodec tableName =
    Codec input output
  where
    -- tableNames :: TOML -> [Key]
    -- tableNames t = applicableKey "group.*.dependencies" (tomlTables t)

    getTable :: Key -> TomlEnv map
    getTable lookupKey = \t -> case Prefix.lookup lookupKey $ tomlTables t of
        Nothing -> Success emptyMap
        Just toml' ->
            let valKeys = HashMap.keys $ tomlPairs toml'
                tableKeys = fmap (:|| []) $ HashMap.keys $ tomlTables toml'
                tableArrayKey = HashMap.keys $ tomlTableArrays toml'
            in fmap fromListMap $ for (valKeys <> tableKeys <> tableArrayKey) $ \key ->
                whenLeftBiMapError key (forward keyBiMap key) $ \k ->
                    (k,) <$> codecRead (valCodec key) toml'

    merge :: [Validation [TomlDecodeError] map] -> Validation [TomlDecodeError] map
    merge validations =
        case sequenceA validations of
            Failure errors -> Failure errors
            Success values -> Success $ unionsMap values

    input :: TomlEnv map
    input = \t -> do
        let viableKeys =  trace ("input.viableKeys:" ++ show (applicableKey "group.*.dependencies" (tomlTables t))) $ applicableKey "group.*.dependencies" (tomlTables t)
        let maps = merge $ map (`getTable` t) viableKeys
        maps

    input' :: TomlEnv map
    input' = \t -> case Prefix.lookup tableName $ tomlTables t of
        Nothing -> Success emptyMap
        Just toml ->
            let valKeys = HashMap.keys $ tomlPairs toml
                tableKeys = fmap (:|| []) $ HashMap.keys $ tomlTables toml
                tableArrayKey = HashMap.keys $ tomlTableArrays toml
            in fmap fromListMap $ for (valKeys <> tableKeys <> tableArrayKey) $ \key ->
                whenLeftBiMapError key (forward keyBiMap key) $ \k ->
                    (k,) <$> codecRead (valCodec key) toml

        -- for tableNames $ \tableName' -> 
        --     case lookup tableName' tables of
        --         Nothing -> pure Nothing
        --         Just toml -> Just toml

        -- maps :: [Maybe (Key, TOML)] <- for tableNames $ \tableName' ->
        --     case lookup tableName' tables of
        --         Nothing -> pure Nothing
        --         Just toml -> pure . Just $ (tableName', toml)

                -- Just toml -> trace ("input.toml: " ++ show toml) $ do
                --     let valKeys = trace ("input.valKeys: " ++ show (HashMap.keys $ tomlPairs toml)) $ HashMap.keys $ tomlPairs toml
                --     let tableKeys = trace ("input.tableKeys: " ++ show (HashMap.keys $ tomlTables toml)) $ fmap (:|| []) $ HashMap.keys $ tomlTables toml

                --     for (valKeys <> tableKeys) $ \key -> do
                --         whenLeftBiMapError key (forward keyBiMap key) $ \k ->  trace ("input.whenLeftBiMapError.key: " ++ show key) $
                --             (k,) <$> codecRead (valCodec key) toml

        -- Success emptyMap

    output :: map -> TomlState map
    output m = do
        mTable <- gets $ lookup tableName . tomlTables
        let toml = fromMaybe mempty mTable
        let (_, newToml) = unTomlState updateMapTable toml
        m <$ modify (insertTable tableName newToml)
      where
        updateMapTable :: TomlState ()
        updateMapTable = forM_ (toListMap m) $ \(k, v) -> case backward keyBiMap k of
            Left _    -> empty
            Right key -> codecWrite (valCodec key) v


lookupT :: Key -> Prefix.PrefixTree a -> Maybe a
lookupT lk (Prefix.Leaf k v) = trace ("input.lookupT.Prefix.Leaf => k:" ++ show k ++ " lk:" ++ show lk) $ if compareKey lk k then Just v else Nothing
lookupT lk (Prefix.Branch pref mv prefMap) = trace ("input.lookupT.Prefix.Branch => pref:" ++ show pref ++ " lk:" ++ show lk) $
    case keysDiff' pref lk of
        Equal       -> trace ("input.lookupT.Prefix.Branch.Equal:") $ mv
        NoPrefix    -> trace ("input.lookupT.Prefix.Branch.NoPrefix:") $ Nothing
        Diff _ _ _  -> trace ("input.lookupT.Prefix.Branch.Diff:") $ Nothing
        SndIsPref _ -> trace ("input.lookupT.Prefix.Branch.SndIsPref:") $ Nothing
        -- The first key is the prefix of the second one.
        FstIsPref k -> trace ("input.lookupT.Prefix.Branch.FstIsPref: k:" ++ show k) $ lookup k prefMap

lookup :: Key -> Prefix.PrefixMap a -> Maybe a
lookup k@(p :|| _) prefMap = HashMap.lookup p prefMap >>= lookupT k



compareKey :: Key -> Key -> Bool
compareKey lhs rhs = lhs == rhs

keysDiff' :: Key -> Key -> KeysDiff
keysDiff' (x :|| xs) (y :|| ys)
    | x == y    = trace ("x <> y:" ++ show x ++ ".." ++ show y) $ listSame xs ys []
    | x == "*"  = trace ("x <> y:" ++ show x ++ ".." ++ show y) $ listSame xs ys []
    | otherwise = trace ("x <> y:" ++ show x ++ ".." ++ show y) $ NoPrefix
  where
    listSame :: [Piece] -> [Piece] -> [Piece] -> KeysDiff
    listSame [] []     _ = Equal
    listSame [] (s:ss) _ = FstIsPref $ s :|| ss
    listSame (f:fs) [] _ = SndIsPref $ f :|| fs
    listSame (f:fs) (s:ss) pr =
        if f == s
        then listSame fs ss (pr ++ [f])
        else Diff (x :|| pr) (f :|| fs) (s :|| ss)

compareKey' :: Key -> Key -> Bool
compareKey' (x :|| xs) (y :|| ys) = trace ("input.compareKey' => lhs:" ++ show x ++ " rhs:" ++ show y) $
    if (x == "*" || x == y)
    then do
        case (mkKey xs, mkKey ys) of
            (Just lhs', Just rhs') -> trace ("input.compareKey' => lhs':" ++ show lhs' ++ " rhs':" ++ show rhs') $ compareKey' lhs' rhs'
            (Nothing, Nothing) -> True
            _ -> False
    else False

mkKey :: [Piece] -> Maybe Key
mkKey xs = Key <$> NE.nonEmpty xs



