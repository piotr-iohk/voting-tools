{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Cardano.CLI.Query where

import           Cardano.Db (DbLovelace (DbLovelace), TxId)
import           Control.Monad (foldM)
import           Control.Monad.Except (runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (MonadReader, ask, runReaderT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Either (partitionEithers)
import           Data.Foldable (for_)
import           Data.List (foldl')
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Database.Esqueleto.Legacy (BackendCompatible, Single (Single), fromSqlKey)
import           Database.Persist.Postgresql (PersistValue(..), IsolationLevel (Serializable), SqlBackend, PersistField(..), Entity(..),
                   SqlPersistT, rawExecute, rawSql, runSqlConnWithIsolation)
import           System.IO (hPutStr, hPutStrLn, stderr)

import           Ouroboros.Network.Block (unSlotNo)

import           Data.Int (Int64)
import           Data.Word (Word16)
import           Cardano.Api (SlotNo)
import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import           Cardano.CLI.Fetching (RegistrationInfo (..))
import           Cardano.CLI.Voting.Metadata (MetadataParsingError, Vote, metadataMetaKey,
                   parseMetadataFromJson, prettyPrintMetadataParsingError, signatureMetaKey,
                   voteFromTxMetadata, voteRegistrationVerificationKey)
import           Cardano.CLI.Voting.Signing (getStakeHash)
import           Control.Lens ((#))
import           Control.Lens.TH (makeClassyPrisms)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Cardano.Db as Db

data MetadataRetrievalError
  = MetadataFailedToRetrieveMetadataField !TxId
  | MetadataFailedToRetrieveSignatureField !TxId
  | MetadataFailedToDecodeMetadataField !TxId !Text
  | MetadataFailedToDecodeSignatureField !TxId !Text
  | MetadataFailedToDecodeTxMetadata !TxId !Api.TxMetadataJsonError
  | MetadataFailedToParseVoteRegistration !TxId !MetadataParsingError
  deriving (Eq, Show)

makeClassyPrisms ''MetadataRetrievalError

prettyPrintMetadataRetrievalError :: MetadataRetrievalError -> Text
prettyPrintMetadataRetrievalError (MetadataFailedToRetrieveMetadataField txId) =
  "There was no metadata JSON (key \"61284\") value associated with the tx \""
  <> prettyPrintTxId txId <> "\""
prettyPrintMetadataRetrievalError (MetadataFailedToRetrieveSignatureField txId) =
  "There was no signature JSON (key \"61285\") value associated with the tx \""
  <> prettyPrintTxId txId <> "\""
prettyPrintMetadataRetrievalError (MetadataFailedToDecodeMetadataField txId err) =
  "Couldn't decode JSON value at metadata key \"61284\" for transaction \""
  <> prettyPrintTxId txId <> "\". Error was: \"" <> err <> "\""
prettyPrintMetadataRetrievalError (MetadataFailedToDecodeSignatureField txId err) =
  "Couldn't decode JSON value at signature key \"61285\" for transaction \""
  <> prettyPrintTxId txId <> "\". Error was: \"" <> err <> "\""
prettyPrintMetadataRetrievalError (MetadataFailedToDecodeTxMetadata txId err) =
  "The metadata entry associated with transaction \"" <> prettyPrintTxId txId
  <> "\" was not valid TxMetadata. Parsing failed with error: \""
  <> T.pack (Api.displayError err) <> "\""
prettyPrintMetadataRetrievalError (MetadataFailedToParseVoteRegistration txId err) =
  "The TxMetadata associated with transaction \"" <> prettyPrintTxId txId
  <> "\" was not a valid vote registration. Parsing failed with error: \""
  <> prettyPrintMetadataParsingError err <> "\""

retrievalErrorTxId :: MetadataRetrievalError -> TxId
retrievalErrorTxId (MetadataFailedToRetrieveMetadataField txId)   = txId
retrievalErrorTxId (MetadataFailedToRetrieveSignatureField txId)  = txId
retrievalErrorTxId (MetadataFailedToDecodeMetadataField txId _)   = txId
retrievalErrorTxId (MetadataFailedToDecodeSignatureField txId _)  = txId
retrievalErrorTxId (MetadataFailedToDecodeTxMetadata txId _)      = txId
retrievalErrorTxId (MetadataFailedToParseVoteRegistration txId _) = txId

prettyPrintTxId :: TxId -> Text
prettyPrintTxId = T.pack . show . fromSqlKey

queryStake
  :: ( MonadIO m
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     )
  => Api.NetworkId
  -> Api.Hash Api.StakeKey
  -> m Api.Lovelace
queryStake nw stakeHash = do
  r <- ask
  let
      -- TODO: figure out how to obtain this from NetworkId via cardano-api
      stakeAddress = Api.makeStakeAddress nw (Api.StakeCredentialByKey stakeHash)
      stakeAddressHex = T.pack (BC.unpack $ Api.serialiseToRawBytesHex stakeAddress)
      stakeQuerySql = "SELECT SUM(utxo_snapshot.value) FROM utxo_snapshot WHERE stake_credential = decode('" <> stakeAddressHex <> "', 'hex');"
  (stakeValues :: [Single (Maybe DbLovelace)]) <- (flip runReaderT) r $ rawSql stakeQuerySql []
  case stakeValues of
    _x1:(_x2:_xs)                         -> error "Too many stake values found for stake sum, is SUM missing from your query?"
    []                                    -> pure 0
    (Single Nothing):[]                   -> pure 0
    (Single (Just (DbLovelace stake))):[] -> pure $ fromIntegral stake

queryVoteRegistrationInfo
  :: ( MonadIO m
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     )
  => Api.NetworkId
  -> Maybe SlotNo
  -> m [(Vote, Integer)]
queryVoteRegistrationInfo nw mSlotNo  = do
  regos <- queryVoteRegistration mSlotNo

  liftIO $ hPutStr stderr $ "\nFound " <> show (length regos) <> " vote registrations. Of which, "

  let
      (badRegos, goodRegos) = partitionEithers regos

  liftIO $ hPutStr stderr $ "\n\t" <> show (length goodRegos) <> " were good vote registrations."
  liftIO $ hPutStr stderr $ "\n\t" <> show (length badRegos) <> " were bad vote registrations."

  liftIO $ hPutStr stderr $ "\n Failed transactions:"
  for_ badRegos $ \err ->
    liftIO $ hPutStr stderr $ T.unpack
      $ "\n - tx: '" <> prettyPrintTxId (retrievalErrorTxId err) <> "',"
      <> " reason: '" <> prettyPrintMetadataRetrievalError err <> "'"

  let
    -- Each stake key has one public voting key it can stake to
    --
    -- Choose the latest vote registration by taking the vote registration with
    -- the highest txid.
    xs :: Map (Api.Hash Api.StakeKey) Vote
    xs = fmap snd $ foldl' (\acc (txid, rego) ->
             let
               verKeyHash = getStakeHash $ voteRegistrationVerificationKey $ rego
             in
               case M.lookup verKeyHash acc of
                 Nothing      -> M.insert verKeyHash (txid, rego) acc
                 -- DECISION #07:
                 --   We successfully parsed a vote registration, but we also
                 --   found a new registration so we'll use that one instead.
                 --
                 --   Found entry
                 --   └── It had the right metadata keys
                 --       └── The metadata was JSON
                 --           └── The metadata was valid cardano-api TxMetadata
                 --               └── The metadata constituted a valid vote
                 --                   └── But we found a later vote.
                 Just (t, _)  -> if txid >= t
                                 then M.insert verKeyHash (txid, rego) acc
                                 else acc
           ) mempty goodRegos

  liftIO $ hPutStrLn stderr $ "\nKeys eligible: " <> show (length xs)

  let
    xs' = zip [1..] (M.toList xs)
  -- In the following, txIn.tx_in_id shows the transaction where this was used
  -- as an input. Voting power must be calculated from unspent UTxOs, so they
  -- cannot have been used as an input.
  stakeTempTableSql <- case mSlotNo of
    Nothing     -> do
      let stake_credential_index = "CREATE INDEX IF NOT EXISTS utxo_snapshot_stake_credential ON utxo_snapshot(stake_credential);"
          analyze_table = "ANALYZE utxo_snapshot;"
          utxo_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS utxo_snapshot AS (SELECT tx_out.*, stake_address.hash_raw AS stake_credential FROM tx_out LEFT OUTER JOIN tx_in ON tx_out.tx_id = tx_in.tx_out_id AND tx_out.index = tx_in.tx_out_index INNER JOIN stake_address ON stake_address.id = tx_out.stake_address_id WHERE tx_in.tx_in_id IS NULL);"
      pure $ utxo_snapshot <> stake_credential_index <> analyze_table
    Just slotNo -> do
      let tx_out_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS tx_out_snapshot AS (\
            \ SELECT tx_out.*,\
            \ stake_address.hash_raw AS stake_credential\
              \ FROM tx_out\
              \ INNER JOIN tx ON tx_out.tx_id = tx.id\
              \ INNER JOIN block ON tx.block_id = block.id\
              \ INNER JOIN stake_address ON stake_address.id = tx_out.stake_address_id\
              \ WHERE block.slot_no <= " <> T.pack (show $ unSlotNo slotNo) <> ");"
          tx_in_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS tx_in_snapshot AS (\
            \ SELECT tx_in.* FROM tx_in\
              \ INNER JOIN tx ON tx_in.tx_in_id = tx.id\
              \ INNER JOIN block ON tx.block_id = block.id\
              \ WHERE block.slot_no <= " <> T.pack (show $ unSlotNo slotNo) <> ");"
          utxo_snapshot = "CREATE TEMPORARY TABLE IF NOT EXISTS utxo_snapshot AS (\
            \ SELECT tx_out_snapshot.* FROM tx_out_snapshot\
              \ LEFT OUTER JOIN tx_in_snapshot\
                \ ON tx_out_snapshot.tx_id = tx_in_snapshot.tx_out_id\
                \ AND tx_out_snapshot.index = tx_in_snapshot.tx_out_index\
              \ WHERE tx_in_snapshot.tx_in_id IS NULL);"
          stake_credential_index = "CREATE INDEX IF NOT EXISTS utxo_snapshot_stake_credential ON utxo_snapshot(stake_credential);"
          analyze_tx_out_snapshot = "ANALYZE tx_out_snapshot;"
          analyze_tx_in_snapshot = "ANALYZE tx_in_snapshot;"
          analyze_utxo_snapshot = "ANALYZE utxo_snapshot;"
      pure $ tx_out_snapshot
           <> analyze_tx_out_snapshot
           <> tx_in_snapshot
           <> analyze_tx_in_snapshot
           <> utxo_snapshot
           <> stake_credential_index
           <> analyze_utxo_snapshot
  r <- ask

  _ :: () <- (flip runReaderT) r $ rawExecute stakeTempTableSql []
  foldM (\acc (idx, (_, rego)) -> do
    let verKeyHash = getStakeHash . voteRegistrationVerificationKey $ rego
    (Api.Lovelace stake) <- queryStake nw verKeyHash

    liftIO $ hPutStr stderr $ "\rProcessing vote stake " <> show (idx :: Integer) <> " of " <> show (length xs)
    pure $ (rego, stake):acc
        ) mempty xs'

queryVotingFunds
  :: ( MonadIO m
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     )
  => Api.NetworkId
  -> Maybe SlotNo
  -> m [RegistrationInfo]
queryVotingFunds nw mSlotNo = do
  -- Get each registration and it's voting power
  (info :: [(Vote, Integer)]) <- queryVoteRegistrationInfo nw mSlotNo

  pure $ (\(vote, amt) -> RegistrationInfo vote amt) <$> info

queryVoteRegistration
  :: ( MonadIO m
     , MonadReader backend m
     , BackendCompatible SqlBackend backend
     )
  => Maybe SlotNo
  -> m [Either MetadataRetrievalError (TxId, Vote)]
queryVoteRegistration mSlotNo =
  let
    -- Join the transaction information with the metadata information for that
    -- transaction. The metadata we are interested in is made up of two parts:
    -- the metadata value under key '61284' (voting metadata) and the metadata
    -- under the key '61285' (voting signature metadata).
    sqlBase = "WITH meta_table AS (select tx_id, json AS metadata from tx_metadata where key = '" <> T.pack (show metadataMetaKey) <> "') , sig_table AS (select tx_id, json AS signature from tx_metadata where key = '" <> T.pack (show signatureMetaKey) <> "') SELECT tx.hash,tx_id,metadata,signature FROM meta_table INNER JOIN tx ON tx.id = meta_table.tx_id INNER JOIN sig_table USING(tx_id)"
  in do
    let
      sql = case mSlotNo of
        Just slot -> (sqlBase <> "INNER JOIN block ON block.id = tx.block_id WHERE block.slot_no <= " <> T.pack (show $ unSlotNo slot) <> " ORDER BY metadata -> '4' ASC;")
        -- ^ TODO handle lower bound on slot no too
        Nothing   -> (sqlBase <> " ORDER BY metadata -> '4' ASC;")
    r <- ask
    (results :: [(Single ByteString, Single TxId, Single (Maybe Text), Single (Maybe Text))]) <- (flip runReaderT) r $ rawSql sql []
    sequence $ fmap runExceptT $ (flip fmap) results $ \(Single _txHash, Single txId, Single mMetadata, Single mSignature) -> do
      let
          handleEither f =
              either (throwError . f) pure
      -- DECISION #01:
      --   When querying the transaction/metadata/signature information, the
      --   given row did not have a metadata entry under the key '61284' (i.e.
      --   it did not have any voting metadata).
      --
      --   Found entry
      --   └── But it contained no voting metadata
      --
      -- FIXME: Isn't this prevented by the query? Wouldn't it always be Just?
      -- Answer is yes - there is no need to make this a "Maybe".
      metadata  <-
          maybe
          (throwError $ _MetadataFailedToRetrieveMetadataField # txId)
          pure
          mMetadata
      -- DECISION #02:
      --   When querying the transaction/metadata/signature information, the
      --   given row did not have a metadata signature entry under the key
      --   '61285' (i.e. it did not have any signature metadata).
      --
      --   Found entry
      --   └── But it contained no signature information
      --
      -- FIXME: Isn't this prevented by the query? Wouldn't it always be Just?
      -- Answer is yes - there is no need to make this a "Maybe".
      signature <-
          maybe
          (throwError $ _MetadataFailedToRetrieveSignatureField # txId)
          pure
          mSignature

      -- DECISION #03:
      --   We found an entry with the right keys but failed to parse the voting
      --   metadata because it wasn't a JSON value.
      --
      --   Found entry
      --   └── It had the right metadata keys
      --       └── But the voting metadata value wasn't JSON
      --
      -- This is programmer error - the database should only accept JSON values
      -- into the 'json' column, and even if it doesn't, the tool that submits
      -- the data should only submit valid json. This isn't the voter's fault.
      metadataObj <-
          handleEither (\e -> _MetadataFailedToDecodeMetadataField # (txId, T.pack e))
          $ Aeson.eitherDecode' $ TL.encodeUtf8 $ TL.fromStrict $ metadata
      -- DECISION #04:
      --   We found an entry with the right keys but failed to parse the signature
      --   metadata because it wasn't a JSON value.
      --
      --   Found entry
      --   └── It had the right metadata keys
      --       └── But the signature metadata value wasn't JSON
      --
      -- This is programmer error - the database should only accept JSON values
      -- into the 'json' column, and even if it doesn't, the tool that submits
      -- the data should only submit valid json. This isn't the voter's fault.
      signatureObj <-
          handleEither (\e -> _MetadataFailedToDecodeSignatureField # (txId, T.pack e))
          $ Aeson.eitherDecode' $ TL.encodeUtf8 $ TL.fromStrict $ signature

      let
        metaObj :: Aeson.Object
        metaObj = HM.fromList
          [ (T.pack $ show metadataMetaKey, metadataObj)
          , (T.pack $ show signatureMetaKey, signatureObj)
          ]

      -- DECISION #05:
      --   We found the JSON metadata, but it failed to parse as cardano-api
      --   TxMetadata.
      --
      --   Found entry
      --   └── It had the right metadata keys
      --       └── The metadata was JSON
      --           └── The metadata wasn't valid cardano-api TxMetadata
      --
      -- This could be programmer or user error - Ideally the tool that submits
      -- tx metadata to the database should ensure it's valid cardano-api
      -- TxMetadata, but if it doesn't, the user may have submitted some
      -- metadata that parses as JSON but not TxMetadata (see cardano-api for
      -- more information on how TxMetadata is a subset of JSON).
      meta <- handleEither (\e -> _MetadataFailedToDecodeTxMetadata # (txId, e))
          $ parseMetadataFromJson (Aeson.Object metaObj)
      -- DECISION #06:
      --   We found the TxMetadata, but we were unable to parse a vote from it.
      --
      --   Found entry
      --   └── It had the right metadata keys
      --       └── The metadata was JSON
      --           └── The metadata was valid cardano-api TxMetadata
      --               └── The metadata did not constitute a valid vote
      --
      -- This could be programmer or user error - The tool which submits the
      -- data should validate votes, but if it doesn't the user may have
      -- submitted a malformed vote.

      -- We now ignore any Metadata that fails to decode (generally will be due to entries missing the new "3" metadata field)
      rego <-
          handleEither (\e -> _MetadataFailedToParseVoteRegistration # (txId, e))
          $ voteFromTxMetadata meta

      pure $ (txId, rego)

runQuery :: (MonadIO m) => SqlBackend -> SqlPersistT IO a -> m a
runQuery backend query = liftIO $ runSqlConnWithIsolation query backend Serializable
