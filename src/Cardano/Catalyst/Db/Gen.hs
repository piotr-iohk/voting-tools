
module Cardano.Catalyst.Db.Gen where

import Hedgehog (MonadGen)
import Data.ByteString (ByteString)
import Data.Word (Word64, Word16)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)

import qualified Cardano.Db as Db
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Database.Persist.Sql as Persist
import qualified Data.Time.Clock.POSIX as Time

import Cardano.Catalyst.Db

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = (Time.posixSecondsToUTCTime . fromIntegral) <$> genWord64

genHash :: MonadGen m => m ByteString
genHash = Gen.bytes (Range.singleton 56)

genSlotLeader :: MonadGen m => m Db.SlotLeader
genSlotLeader = Db.SlotLeader
  <$> genHash
  <*> pure Nothing
  <*> Gen.text (Range.linear 0 64) Gen.unicodeAll

genWord64 :: MonadGen m => m Word64
genWord64 = Gen.word64 Range.linearBounded

genWord16 :: MonadGen m => m Word16
genWord16 = Gen.word16 Range.linearBounded

genInt64 :: MonadGen m => m Int64
genInt64 = Gen.int64 Range.linearBounded

genBlock :: MonadGen m => m Db.Block
genBlock = Db.Block
  <$> genHash
  -- ^ Hash
  <*> Gen.maybe genWord64
  -- ^ epoch number
  <*> Gen.maybe genWord64
  -- ^ slot number
  <*> Gen.maybe genWord64
  -- ^ epoch slot no
  <*> Gen.maybe genWord64
  -- ^ block no
  <*> pure Nothing
  -- ^ previous block id
  <*> (Persist.toSqlKey <$> genInt64)
  -- ^ slot leader id
  <*> genWord64
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
  <*> Gen.maybe genHash
  -- ^ op cert
  <*> Gen.maybe genWord64
  -- ^ op cert counter

genLovelace :: MonadGen m => m Db.DbLovelace
genLovelace = Db.DbLovelace <$> genWord64

genTx :: MonadGen m => m Db.Tx
genTx = Db.Tx
  <$> genHash
  <*> (Persist.toSqlKey <$> genInt64)
  -- ^ Block id
  <*> genWord64
  -- ^ Block index
  <*> genLovelace
  -- ^ out_sum
  <*> genLovelace
  -- ^ fee
  <*> genInt64
  -- ^ deposit
  <*> genWord64
  -- ^ size
  <*> Gen.maybe (Db.DbWord64 <$> genWord64)
  -- ^ invalid before (slot)
  <*> Gen.maybe (Db.DbWord64 <$> genWord64)
  -- ^ invalid after (slot)
  <*> Gen.bool
  -- ^ script validity
  <*> genWord64
  -- ^ script size

genTxMetadata :: MonadGen m => m Db.TxMetadata
genTxMetadata = Db.TxMetadata
  <$> (Db.DbWord64 <$> genWord64)
  <*> Gen.maybe (Gen.text (Range.linear 0 256) Gen.unicodeAll)
  <*> Gen.bytes (Range.linear 0 512)
  <*> (Persist.toSqlKey <$> genInt64)

genTransaction :: MonadGen m => m Transaction
genTransaction =
  Transaction <$> genTx <*> genBlock <*> genSlotLeader

genMetadataEntry :: MonadGen m => m MetadataEntry
genMetadataEntry =
  MetadataEntry <$> genTxMetadata <*> genTransaction

genStakeAddress :: MonadGen m => m Db.StakeAddress
genStakeAddress = Db.StakeAddress
  <$> genHash
  <*> Gen.text (Range.linear 0 256) Gen.unicodeAll
  <*> Gen.maybe (Gen.bytes (Range.linear 0 512))
  <*> (Persist.toSqlKey <$> genInt64)
