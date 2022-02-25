{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Catalyst.Db.Gen where

import Hedgehog (MonadGen, fromGenT, toGenT, GenBase, Gen)
import Data.ByteString (ByteString)
import Data.Word (Word64, Word32, Word16)
import Data.Int (Int32, Int64, Int16)
import Data.Time.Clock (UTCTime)
import Data.Functor.Identity (Identity)
import Cardano.CLI.Voting.Signing (voteVerificationKeyStakeAddressHashRaw, VoteVerificationKey, serialiseVoteVerificationKeyToBech32, voteVerificationKeyHashRaw)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (StateT(..), lift, MonadTrans)
import Control.Monad (replicateM_)
import Data.Set (Set)

import qualified Cardano.Db as Db
import qualified Control.Monad.State.Class as State
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Database.Persist.Sql as Persist
import qualified Data.Time.Clock.POSIX as Time
import qualified Gen.Cardano.Api.Metadata as Cardano
import qualified Cardano.Api as Cardano
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import qualified Data.Binary as Bin
import qualified Data.Binary.Put as Put

import Cardano.Catalyst.Db

genUTCTime :: MonadGen m => m UTCTime
genUTCTime =
  (Time.posixSecondsToUTCTime . fromIntegral)
  <$> (Gen.word32 Range.linearBounded)

genHash32 :: MonadGen m => m ByteString
genHash32 = Gen.bytes (Range.singleton 32)

genUniqueHash32
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m ByteString
genUniqueHash32 =
  -- Discard shrink tree (i.e. don't shrink). If we shrink we risk duplicate
  -- hashes.
  -- Gen.prune $ do
  do
    -- Get a Word32 from our list
    xs <- State.get
    -- Make sure this Word32 is not used again
    State.put $ tail xs
    let
      -- A Word32 is 4 bytes, but we need a 32-byte hash. So replicate the 4
      -- bytes 8 times. This doesn't cover all possible 32-bytes, but it should
      -- be enough.
      bytes = Put.runPut $ do
        let word32 = head xs
        replicateM_ 8 $ Put.putWord32be $ word32
    pure $ BSL.toStrict bytes

genUniqueHash28
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m ByteString
genUniqueHash28 = do
  hash32 <- genUniqueHash32
  -- Get the 28 most significant bytes of the 32-byte hash. Due to the way
  -- genUniqueHash32 is implemented, this won't result in duplicate hashes.
  pure $ BS.take 28 hash32

uniquely :: (MonadGen m, MonadState (Set a) m, Ord a) => m a -> m a
uniquely gen = do
  let
    loop = do
      as <- State.get
      a <- gen
      if a `Set.member` as
        then Debug.trace "discarding..." Gen.discard
        else do
          _ <- State.put $ a `Set.insert` as
          pure a
  loop

runUniquely :: s -> StateT s m a -> m (a, s)
runUniquely state = flip runStateT state

genHash28 :: MonadGen m => m ByteString
genHash28 = Gen.bytes (Range.singleton 28)

genSlotLeader :: (MonadGen m, MonadState [Word32] m) => m Db.SlotLeader
genSlotLeader = Db.SlotLeader
  <$> genUniqueHash28
  -- ^ Addresses use a 28 byte hash (as do StakeholdIds).
  <*> pure Nothing
  <*> Gen.text (Range.linear 0 64) Gen.unicodeAll

genWord64 :: MonadGen m => m Word64
genWord64 = Gen.word64 Range.linearBounded

genWord32 :: MonadGen m => m Word32
genWord32 = Gen.word32 Range.linearBounded

genUInteger :: (MonadGen m, Num int) => m int
genUInteger = fromIntegral <$> genWord16

genWord63 :: MonadGen m => m Word64
genWord63 = Gen.word64 (Range.linear 0 (2 ^ 63))

genWord16 :: MonadGen m => m Word16
genWord16 = Gen.word16 Range.linearBounded

genInt64 :: MonadGen m => m Int64
genInt64 = Gen.int64 Range.linearBounded

genBlock
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m Db.Block
genBlock = Db.Block
  <$> genUniqueHash32
  -- ^ Hash
  <*> Gen.maybe genUInteger
  -- ^ epoch number
  <*> Gen.maybe genUInteger
  -- ^ slot number
  <*> Gen.maybe genUInteger
  -- ^ epoch slot no
  <*> Gen.maybe genUInteger
  -- ^ block no
  <*> pure Nothing
  -- ^ previous block id
  <*> (Persist.toSqlKey <$> genInt64)
  -- ^ slot leader id
  <*> genUInteger
  -- ^ block size
  <*> genUTCTime
  --- ^ block time
  <*> genWord64
  -- ^ Tx count
  <*> genWord16
  -- ^ Protocol major ver
  <*> genWord16
  -- ^ Protocol major ver
  <*> Gen.maybe (Gen.text (Range.singleton 65) Gen.ascii)
  -- ^ vrf key
  <*> Gen.maybe genHash32
  -- ^ op cert
  <*> Gen.maybe genWord63
  -- ^ op cert counter

genLovelace :: MonadGen m => m Db.DbLovelace
genLovelace = Db.DbLovelace <$> genWord64

genTx
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m Db.Tx
genTx = Db.Tx
  <$> genUniqueHash32
  <*> (Persist.toSqlKey <$> genInt64)
  -- ^ Block id
  <*> genUInteger
  -- ^ Block index
  <*> genLovelace
  -- ^ out_sum
  <*> genLovelace
  -- ^ fee
  <*> genInt64
  -- ^ deposit
  <*> genUInteger
  -- ^ size
  <*> Gen.maybe (Db.DbWord64 <$> genWord64)
  -- ^ invalid before (slot)
  <*> Gen.maybe (Db.DbWord64 <$> genWord64)
  -- ^ invalid after (slot)
  <*> Gen.bool
  -- ^ script validity
  <*> genUInteger
  -- ^ script size

genTxMetadata
  :: ( MonadGen m
     , GenBase m ~ Identity
     )
  => m Db.TxMetadata
genTxMetadata = do
  mMetadata <- Gen.maybe $ fromGenT Cardano.genTxMetadata

  let
    (jsonText, jsonBytes) = case mMetadata of
        Nothing ->
          (Nothing, "")
        Just meta ->
          let
            jsonValue =
              Cardano.metadataToJson Cardano.TxMetadataJsonDetailedSchema meta
            jsonBytes = BSL.toStrict $ Aeson.encode jsonValue
            jsonText = T.decodeUtf8 jsonBytes
          in
            (Just jsonText, jsonBytes)

  Db.TxMetadata
    <$> (Db.DbWord64 <$> genWord64)
    <*> pure jsonText
    <*> pure jsonBytes
    <*> (Persist.toSqlKey <$> genInt64)

genTransaction
  :: ( MonadGen m
     , MonadState [Word32] m
     )
  => m Transaction
genTransaction =
  Transaction <$> genTx <*> genBlock <*> genSlotLeader

genMetadataEntry
  :: ( MonadGen m
     , GenBase m ~ Identity
     )
  => StateT [Word32] m MetadataEntry
genMetadataEntry =
  MetadataEntry <$> lift genTxMetadata <*> genTransaction

genStakeAddress :: (MonadGen m, MonadState [Word32] m) => m Db.StakeAddress
genStakeAddress = Db.StakeAddress
  <$> genUniqueHash32
  <*> Gen.text (Range.linear 0 256) Gen.unicodeAll
  <*> pure Nothing
  <*> (Persist.toSqlKey <$> genInt64)

genTxOut :: MonadGen m => m Db.TxOut
genTxOut = Db.TxOut
  <$> (Persist.toSqlKey <$> genInt64)
  -- ^ tx id
  <*> (fromIntegral <$> Gen.int16 (Range.linear 0 (maxBound :: Int16)))
  -- ^ index (uses smallint >= 0)
  <*> Gen.text (Range.linear 0 103) Gen.ascii
  -- ^ address
  <*> genHash32
  -- ^ address raw
  <*> Gen.bool
  -- ^ has script
  <*> Gen.maybe genHash28
  -- ^ Payment credential
  <*> Gen.maybe (Persist.toSqlKey <$> genInt64)
  -- ^ stake address id
  <*> genLovelace
  -- ^ Value
  <*> Gen.maybe genHash32
  -- ^ Data hash

genTxIn :: MonadGen m => m Db.TxIn
genTxIn = Db.TxIn
  <$> (Persist.toSqlKey <$> genInt64)
  -- ^ Tx in id
  <*> (Persist.toSqlKey <$> genInt64)
  -- ^ Tx out id
  <*> genWord16
  -- ^ Tx out index
  <*> pure Nothing
  -- ^ Redeemer id

genContribution
  :: ( MonadGen m
     , MonadState [Word32] m
     ) => m Contribution
genContribution = Contribution
  <$> genTxOut
  <*> genStakeAddress
  <*> genTxIn
  <*> genTransaction
  <*> genTransaction

genContributionForStakeAddress
  :: (MonadGen m, MonadState [Word32] m)
  => VoteVerificationKey
  -> m Contribution
genContributionForStakeAddress verKey = do
  contribution <- genContribution
  let verKeyHashRaw = voteVerificationKeyStakeAddressHashRaw Cardano.Mainnet verKey
      verKeyView = serialiseVoteVerificationKeyToBech32 verKey

      stakeAddress = contributionTxOutStaking contribution
      stakeAddress' = stakeAddress { Db.stakeAddressHashRaw = verKeyHashRaw
                                   , Db.stakeAddressView = verKeyView
                                   }
  pure $ contribution { contributionTxOutStaking = stakeAddress' }

-- tx <- Gen.transaction
-- vote <- Gen.vote
-- let txMetadataAll = voteToTxMetadata vote
-- voteToTxMetadata vote

-- generate vote signing key
-- any slot
-- any rewards address
-- any votepub
--
-- gen tx and submit
-- Create vote payload tx metadata
--
