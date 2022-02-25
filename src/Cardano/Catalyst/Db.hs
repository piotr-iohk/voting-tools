{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.Catalyst.Db where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader, runReaderT, ask)
import           Database.Persist.Postgresql (Key(..), PersistValue(..))

import qualified Database.Persist.Class as Sql
import qualified Database.Persist.Sql as Sql
import qualified Cardano.Db as Db

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Cardano.Api as Cardano
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T
import Data.Text.Encoding as T
import           Cardano.CLI.Voting.Metadata (Vote, voteToTxMetadata, signatureMetaKey, metadataMetaKey)
import qualified Data.Map.Strict as M

import qualified Control.Monad.State.Strict as State

-- | A transaction consists of a Tx and the Block it was accepted into the
-- chain, also the SlotLeader for that Block.
data Transaction = Transaction
  { transactionTx         :: Db.Tx
  , transactionBlock      :: Db.Block
  , transactionSlotLeader :: Db.SlotLeader
  }

-- | Metadata consists of the metadata, along with the @Transaction@ it was
-- submitted in.
data MetadataEntry = MetadataEntry
  { metaMetadata    :: Db.TxMetadata
  , metaTransaction :: Transaction
  }

-- | The registration is simply a @MetadataEntry@.
type Registration = MetadataEntry

-- | The signature is simply a @MetadataEntry@.
type Signature = MetadataEntry

-- | A TxOut contributes some value to a stake address.
data Contribution = Contribution
  { contributionTxOut        :: Db.TxOut
  , contributionTxOutStaking :: Db.StakeAddress
  , contributionFromTxIn     :: Db.TxIn
  , contributionTx           :: Transaction
  , contributionFromTx       :: Transaction
  }

-- | The following are functions that allow us to test the postgres backend by
-- inserting registrations into the database and associating voting power with
-- them. They are not intended to be used outside of a testing environment, as
-- voting-tools is a read-only tool.

writeTransaction
  :: ( MonadIO m
     , MonadReader backend m
     , Sql.PersistRecordBackend Db.Tx backend
     , Sql.PersistRecordBackend Db.Block backend
     , Sql.PersistRecordBackend Db.SlotLeader backend
     , Sql.PersistStoreWrite backend
     )
  => Transaction -> m (Key Db.Tx, Transaction)
writeTransaction (Transaction tx block slotLeader) = do
  r <- ask

  (flip runReaderT) r $ do
    slotLeaderId <- Sql.insert slotLeader
    let block' = block { Db.blockSlotLeaderId = slotLeaderId }
    blockId <- Sql.insert block'
    let tx' = tx { Db.txBlockId = blockId }
    txId <- Sql.insert tx'
    pure (txId, Transaction tx' block' slotLeader)

writeTxMetadata
  :: ( MonadIO m
     , MonadReader backend m
     , Sql.PersistRecordBackend Db.TxMetadata backend
     , Sql.PersistRecordBackend Db.Tx backend
     , Sql.PersistRecordBackend Db.Block backend
     , Sql.PersistRecordBackend Db.SlotLeader backend
     , Sql.PersistStoreWrite backend
     )
  => MetadataEntry -> m (Key Db.TxMetadata, MetadataEntry)
writeTxMetadata (MetadataEntry metadata transaction) = do
  r <- ask

  (flip runReaderT) r $ do
    (txId, transaction') <- writeTransaction transaction
    let metadata' = metadata { Db.txMetadataTxId = txId }
    metadataId <- Sql.insert metadata'
    pure (metadataId, MetadataEntry metadata' transaction')

x :: Maybe Cardano.TxMetadataValue -> (Maybe Text, ByteString)
x Nothing     = (Nothing, "")
x (Just meta) =
  let
    jsonValue =
      Cardano.metadataValueToJsonNoSchema meta
    jsonBytes = BSL.toStrict $ Aeson.encode jsonValue
    jsonText = T.decodeUtf8 jsonBytes
  in
    (Just jsonText, jsonBytes)

register'
  :: ( MonadIO m
     , MonadReader backend m
     , Sql.PersistRecordBackend Db.TxMetadata backend
     , Sql.PersistStoreWrite backend
     )
  => Key Db.Tx
  -> Vote
  -> m ( (Key Db.TxMetadata, Db.TxMetadata)
       , (Key Db.TxMetadata, Db.TxMetadata)
       )
register' txId vote = do
  let
    (Cardano.TxMetadata metaMap) = voteToTxMetadata vote
    mSig = M.lookup signatureMetaKey metaMap
    mPayload = M.lookup metadataMetaKey metaMap

    (sigJSON, sigBytes) = x mSig
    (payloadJSON, payloadBytes) = x mPayload

    payloadMeta = Db.TxMetadata (Db.DbWord64 metadataMetaKey) payloadJSON payloadBytes txId
    sigMeta = Db.TxMetadata (Db.DbWord64 signatureMetaKey) sigJSON sigBytes txId

  r <- ask
  (flip runReaderT) r $ do
    payloadId <- Sql.insert payloadMeta
    sigId <- Sql.insert sigMeta

    pure ( (payloadId, payloadMeta)
         , (sigId, sigMeta)
         )

register
  :: ( MonadIO m
     , MonadReader backend m
     , Sql.PersistRecordBackend Db.TxMetadata backend
     , Sql.PersistRecordBackend Db.Tx backend
     , Sql.PersistRecordBackend Db.Block backend
     , Sql.PersistRecordBackend Db.SlotLeader backend
     , Sql.PersistStoreWrite backend
     )
  => Registration
  -> Maybe Signature
  -> m ( (Key Db.TxMetadata, MetadataEntry)
       , Maybe (Key Db.TxMetadata, MetadataEntry)
       )
register rego mSig = do
  metadataInfo <- writeTxMetadata rego
  mMetadataInfo <- sequence $ writeTxMetadata <$> mSig
  pure (metadataInfo, mMetadataInfo)

contribute
  :: ( MonadIO m
     , MonadReader backend m
     , Sql.PersistRecordBackend Db.Tx backend
     , Sql.PersistRecordBackend Db.Block backend
     , Sql.PersistRecordBackend Db.SlotLeader backend
     , Sql.PersistRecordBackend Db.StakeAddress backend
     , Sql.PersistRecordBackend Db.TxOut backend
     , Sql.PersistRecordBackend Db.TxIn backend
     , Sql.PersistStoreWrite backend
     )
  => Contribution -> m ( Key Db.TxOut
                       , Contribution
                       )
contribute (Contribution txOut stakeAddress txIn transaction fromTransaction) = do
-- contribute (Contribution txOut stakeAddress transaction fromTransaction) = do
  (txId, transaction') <- writeTransaction transaction
  (fromTxId, fromTransaction') <- writeTransaction fromTransaction

  r <- ask

  (flip runReaderT) r $ do
    stakeAddressId <- Sql.insert
      $ stakeAddress { Db.stakeAddressRegisteredTxId = txId }
    let txOut' = txOut { Db.txOutTxId = txId
                       , Db.txOutStakeAddressId = Just stakeAddressId
                       }
    txOutId <- Sql.insert txOut'
    -- let txIn' = txIn { Db.txInTxInId = fromTxId
    --                  , Db.txInTxOutId = txId
    --                  , Db.txInTxOutIndex = Db.txOutIndex txOut'
    --                  }
    -- txInId <- Sql.insert txIn'
    pure $ ( txOutId
           , Contribution txOut' stakeAddress txIn transaction' fromTransaction'
           )

s :: State.MonadState [Int] m => m Int
s = do
  x <- State.get
  State.put $ tail x
  pure $ head x

runS = State.evalState s
