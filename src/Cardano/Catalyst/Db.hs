
module Cardano.Catalyst.Db where

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
  { txOut    :: Db.TxOut
  , staking  :: Db.StakeAddress
  , fromTxIn :: Db.TxIn
  , tx       :: Transaction
  }

-- register :: Registration -> Maybe Signature -> _
-- contribute :: Contribution -> _
