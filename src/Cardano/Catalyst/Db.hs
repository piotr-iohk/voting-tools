{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.Catalyst.Db where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader (MonadReader, runReaderT, ask)
import           Database.Persist.Postgresql (Key)

import qualified Database.Persist.Class as Sql
import qualified Cardano.Db as Db

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
  => Contribution -> m ( Key Db.TxIn
                       , Key Db.TxOut
                       , Contribution
                       )
contribute (Contribution txOut stakeAddress txIn transaction fromTransaction) = do
  (txId, transaction') <- writeTransaction transaction
  (fromTxId, fromTransaction') <- writeTransaction fromTransaction

  r <- ask

  (flip runReaderT) r $ do
    stakeAddressId <- Sql.insert stakeAddress
    let txOut' = txOut { Db.txOutTxId = txId
                       , Db.txOutStakeAddressId = Just stakeAddressId
                       }
    txOutId <- Sql.insert txOut'
    let txIn' = txIn { Db.txInTxInId = fromTxId
                     , Db.txInTxOutId = txId
                     , Db.txInTxOutIndex = Db.txOutIndex txOut'
                     }
    txInId <- Sql.insert txIn'
    pure $ ( txInId
           , txOutId
           , Contribution txOut' stakeAddress txIn' transaction' fromTransaction'
           )
