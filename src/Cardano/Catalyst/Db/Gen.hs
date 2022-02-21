{-# LANGUAGE GADTs #-}

module Cardano.Catalyst.Db.Gen where

import Hedgehog (MonadGen, fromGenT, toGenT, GenBase, Gen)
import Data.ByteString (ByteString)
import Data.Word (Word64, Word32, Word16)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.Functor.Identity (Identity)

import qualified Cardano.Db as Db
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Database.Persist.Sql as Persist
import qualified Data.Time.Clock.POSIX as Time
import qualified Gen.Cardano.Api.Metadata as Cardano
import qualified Cardano.Api as Cardano
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL

import Cardano.Catalyst.Db

genUTCTime :: MonadGen m => m UTCTime
genUTCTime =
  (Time.posixSecondsToUTCTime . fromIntegral)
  <$> (Gen.word32 Range.linearBounded)

genHash32 :: MonadGen m => m ByteString
genHash32 = Gen.bytes (Range.singleton 32)

genHash28 :: MonadGen m => m ByteString
genHash28 = Gen.bytes (Range.singleton 28)

genSlotLeader :: MonadGen m => m Db.SlotLeader
genSlotLeader = Db.SlotLeader
  <$> genHash28
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

genBlock :: MonadGen m => m Db.Block
genBlock = Db.Block
  <$> genHash32
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

genTx :: MonadGen m => m Db.Tx
genTx = Db.Tx
  <$> genHash32
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

genTxMetadata :: (MonadGen m, GenBase m ~ Identity) => m Db.TxMetadata
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

genTransaction :: MonadGen m => m Transaction
genTransaction =
  Transaction <$> genTx <*> genBlock <*> genSlotLeader

genMetadataEntry :: (MonadGen m, GenBase m ~ Identity) => m MetadataEntry
genMetadataEntry =
  MetadataEntry <$> genTxMetadata <*> genTransaction

genStakeAddress :: MonadGen m => m Db.StakeAddress
genStakeAddress = Db.StakeAddress
  <$> genHash32
  <*> Gen.text (Range.linear 0 256) Gen.unicodeAll
  <*> Gen.maybe (Gen.bytes (Range.linear 0 512))
  <*> (Persist.toSqlKey <$> genInt64)

genTxOut :: MonadGen m => m Db.TxOut
genTxOut = Db.TxOut
  <$> (Persist.toSqlKey <$> genInt64)
  -- ^ tx id
  <*> genWord16
  -- ^ index
  <*> Gen.text (Range.linear 0 103) Gen.ascii
  -- ^ address
  <*> genHash32
  -- ^ address raw
  <*> Gen.bool
  -- ^ has script
  <*> Gen.maybe genHash32
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
