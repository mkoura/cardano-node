{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLI.Shelley.Run.Query
  ( ShelleyQueryCmdError
  , renderShelleyQueryCmdError
  , runQueryCmd
  ) where

import           Cardano.Prelude hiding (atomically, option, threadDelay)

import qualified Codec.CBOR.Term as CBOR
import           Control.Monad.Class.MonadSTM.Strict (MonadSTM, StrictTMVar,
                   atomically, newEmptyTMVarM, tryPutTMVar, takeTMVar)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT,
                   hoistEither, newExceptT)
import           Control.Tracer (Tracer)
import           Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Functor.Contravariant (contramap)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Void (Void)
--import           Numeric (showEFloat)

import           Cardano.Api (getLocalTip, textShow)
import           Cardano.Api.Typed hiding (localStateQueryClient)
import           Cardano.Api.Protocol.Shelley (mkNodeClientProtocolShelley)

import           Cardano.CLI.Shelley.Commands (QueryFilter(..))
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError, pPrintCBOR, renderHelpersError)
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))

import           Cardano.Config.Shelley.Orphans ()
import           Cardano.Config.Types (SocketPath(..))

--import           Cardano.Crypto.Hash.Class (getHashBytesAsHex)
import           Network.Mux (MuxMode(..), MuxTrace, WithMuxBearer)
import           Ouroboros.Consensus.Block (CodecConfig)

import           Ouroboros.Consensus.Cardano (protocolClientInfo)
import           Ouroboros.Consensus.Ledger.Abstract (Query)
import           Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Network.Block (Point, getTipPoint)
import           Ouroboros.Network.NodeToClient (ConnectionId, DictVersion, Handshake,
                   LocalAddress, NetworkConnectTracers (..), NodeToClientProtocols (..),
                   NodeToClientVersion, NodeToClientVersionData (..), TraceSendRecv,
                   Versions, withIOManager)
import qualified Ouroboros.Network.NodeToClient as NodeToClient


import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Credential              as Shelley
import           Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr(..))
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Ledger (PoolDistr(..))
import           Shelley.Spec.Ledger.LedgerState (EpochState)
import qualified Shelley.Spec.Ledger.LedgerState as Ledger
import           Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.TxData as Shelley (TxId (..), TxIn (..), TxOut (..))
import qualified Shelley.Spec.Ledger.UTxO as Ledger (UTxO(..))

import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                  (nodeToClientProtocolVersion, supportedNodeToClientVersions)
import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Network.Block (Serialised (..))
import           Ouroboros.Network.Mux (OuroborosApplication(..),
                   MuxPeer(..), RunMiniProtocol(..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Client
                   (ClientStAcquired (..), ClientStAcquiring (..), ClientStIdle (..),
                    ClientStQuerying (..), LocalStateQueryClient(..), localStateQueryClientPeer)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))



import           Cardano.Binary (decodeFull)
import           Cardano.BM.Data.Tracer (ToLogObject (..), nullTracer)
import           Cardano.BM.Trace (Trace, appendName, logInfo)





data ShelleyQueryCmdError
  = ShelleyQueryEnvVarSocketErr !EnvSocketError
  | NodeLocalStateQueryError !LocalStateQueryError
  | ShelleyQueryWriteProtocolParamsError !FilePath !IOException
  | ShelleyQueryWriteFilteredUTxOsError !FilePath !IOException
  | ShelleyQueryWriteStakeDistributionError !FilePath !IOException
  | ShelleyQueryWriteLedgerStateError !FilePath !IOException
  | ShelleyQueryWriteStakeAddressInfoError !FilePath !IOException
  | ShelleyHelpersError !HelpersError
  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    NodeLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryWriteProtocolParamsError fp ioException ->
      "Error writing protocol parameters at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteFilteredUTxOsError fp ioException ->
      "Error writing filtered UTxOs at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteStakeDistributionError fp ioException ->
      "Error writing stake distribution at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteLedgerStateError fp ioException ->
      "Error writing ledger state at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyQueryWriteStakeAddressInfoError fp ioException ->
      "Error writing stake address info at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyHelpersError helpersErr -> renderHelpersError helpersErr

runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters network mOutFile ->
      runQueryProtocolParameters network mOutFile
    QueryTip network mOutFile ->
      runQueryTip network mOutFile
    QueryStakeDistribution network mOutFile ->
      runQueryStakeDistribution network mOutFile
    QueryStakeAddressInfo addr network mOutFile ->
      runQueryStakeAddressInfo addr network mOutFile
    QueryLedgerState network mOutFile ->
      runQueryLedgerState network mOutFile
    QueryUTxO qFilter networkId mOutFile ->
      runQueryUTxO qFilter networkId mOutFile
    _ -> liftIO $ putStrLn $ "runQueryCmd: " ++ show cmd

runQueryProtocolParameters
  :: NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  pparams <- firstExceptT NodeLocalStateQueryError $
    queryPParamsFromLocalState network sockPath (getTipPoint tip)
  writeProtocolParameters mOutFile pparams

writeProtocolParameters :: Maybe OutputFile -> PParams -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolParameters mOutFile pparams =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteProtocolParamsError fpath) $
        LBS.writeFile fpath (encodePretty pparams)

runQueryTip
  :: NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  case mOutFile of
    Just (OutputFile fpath) -> liftIO . LBS.writeFile fpath $ encodePretty tip
    Nothing -> liftIO $ LBS.putStrLn (encodePretty tip)


runQueryUTxO
  :: QueryFilter
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO qfilter networkId mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
            getLocalTip iomgr ptclClientInfo networkId sockPath
  filteredUtxo <- firstExceptT NodeLocalStateQueryError $
    queryUTxOFromLocalState networkId sockPath qfilter (getTipPoint tip)
  writeFilteredUTxOs mOutFile filteredUtxo

runQueryLedgerState
  :: NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
            getLocalTip iomgr ptclClientInfo network sockPath
  els <- firstExceptT NodeLocalStateQueryError $
                      queryLocalLedgerState network sockPath (getTipPoint tip)
  case els of
    Right lstate -> writeLedgerState mOutFile lstate
    Left lbs -> do
      liftIO $ putTextLn "Version mismatch between node and consensus, so dumping this as generic CBOR."
      firstExceptT ShelleyHelpersError $ pPrintCBOR lbs

runQueryStakeAddressInfo
  :: StakeAddress
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo addr network mOutFile = do
    sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
    let ptclClientInfo = pClientInfoCodecConfig
                       . protocolClientInfo
                       $ mkNodeClientProtocolShelley
    tip <- liftIO $ withIOManager $ \iomgr ->
      getLocalTip iomgr ptclClientInfo network sockPath
    delegsAndRwds <- firstExceptT NodeLocalStateQueryError $
      queryDelegationsAndRewardsFromLocalState
        network
        sockPath
        (Set.singleton addr)
        (getTipPoint tip)
    writeStakeAddressInfo mOutFile delegsAndRwds

-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
data LocalStateQueryError
  = AcquireFailureError !AcquireFailure
--  | ByronAddressesNotSupportedError !(Set ByronAddress)
  -- ^ The query does not support Byron addresses.
 -- | NonStakeAddressesNotSupportedError !(Set Address)
  -- ^ The query does not support non-stake addresses.
  -- Associated with this error are the specific non-stake addresses that were
  -- provided.
  deriving (Eq, Show)

writeStakeAddressInfo
  :: Maybe OutputFile
  -> DelegationsAndRewards
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeAddressInfo mOutFile dr@(DelegationsAndRewards _delegsAndRwds) =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty dr)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteStakeAddressInfoError fpath)
        $ LBS.writeFile fpath (encodePretty dr)

writeLedgerState :: Maybe OutputFile -> EpochState TPraosStandardCrypto -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile lstate =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty lstate)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryWriteLedgerStateError fpath)
        $ LBS.writeFile fpath (encodePretty lstate)

writeFilteredUTxOs :: Maybe OutputFile -> Ledger.UTxO TPraosStandardCrypto -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs utxo
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyQueryWriteFilteredUTxOsError fpath) $ LBS.writeFile fpath (encodePretty utxo)

printFilteredUTxOs :: Ledger.UTxO TPraosStandardCrypto -> IO ()
printFilteredUTxOs (Ledger.UTxO utxo) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    mapM_ printUtxo $ Map.toList utxo
  where
    title :: Text
    title =
      "                           TxHash                                 TxIx        Lovelace"

    printUtxo :: (Shelley.TxIn TPraosStandardCrypto, Shelley.TxOut TPraosStandardCrypto) -> IO ()
    printUtxo (Shelley.TxIn (Shelley.TxId txhash) txin , Shelley.TxOut _ (Coin coin)) =
      Text.putStrLn $
        mconcat
          [ Text.pack (show txhash)
          , textShowN 6 txin
          , textShowN 18 coin -- enough to display maxLovelaceVal
          ]

    textShowN :: Show a => Int -> a -> Text
    textShowN len x =
      let str = show x
          slen = length str
      in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

runQueryStakeDistribution
  :: NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig
                     . protocolClientInfo
                     $ mkNodeClientProtocolShelley
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  stakeDist <- firstExceptT NodeLocalStateQueryError $
    queryStakeDistributionFromLocalState network sockPath (getTipPoint tip)
  writeStakeDistribution mOutFile stakeDist

writeStakeDistribution :: Maybe OutputFile
                       -> PoolDistr TPraosStandardCrypto
                       -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) (PoolDistr stakeDist) =
    handleIOExceptT (ShelleyQueryWriteStakeDistributionError outFile) $
      LBS.writeFile outFile (encodePretty stakeDist)

writeStakeDistribution Nothing _stakeDist = panic "TODO"
   -- liftIO $ printStakeDistribution stakeDist

{-
printStakeDistribution :: PoolDistr TPraosStandardCrypto -> IO ()
printStakeDistribution (PoolDistr stakeDist) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    sequence_
      [ putStrLn $ showStakeDistr poolId stakeFraction vrfKeyId
      | (poolId, (stakeFraction, vrfKeyId)) <- Map.toList stakeDist ]
  where
    title :: Text
    title =
      "                           PoolId                                 Stake frac"

    showStakeDistr :: PoolId
                   -> Rational
                   -> Hash VrfKey
                   -> String
    showStakeDistr (StakePoolKeyHash _poolId) stakeFraction _vrfKeyId =
      concat
        [ BS.unpack (panic "") --(getHashBytesAsHex )
        , "   "
        , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
-- TODO: we could show the VRF id, but it will then not fit in 80 cols
--      , show vrfKeyId
        ]
-}

-- From Cardano.Api

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryUTxOFromLocalState
  :: NetworkId
  -> SocketPath
  -> QueryFilter
  -> Point (ShelleyBlock TPraosStandardCrypto)
  -> ExceptT LocalStateQueryError IO (Ledger.UTxO TPraosStandardCrypto)
queryUTxOFromLocalState network socketPath qFilter point = do
  utxoFilter <- hoistEither $ applyUTxOFilter qFilter
  let pointAndQuery = (point, utxoFilter)
  newExceptT $ queryNodeLocalState
    nullTracer
    (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolShelley)
    network
    socketPath
    pointAndQuery

applyUTxOFilter
  :: (blk ~ ShelleyBlock TPraosStandardCrypto, c ~ TPraosStandardCrypto)
  => QueryFilter
  -> Either LocalStateQueryError (Query blk (Ledger.UTxO c))
applyUTxOFilter qFilter =
    case qFilter of
      FilterByAddress addrs -> Right . GetFilteredUTxO $ getShelleyAddresses addrs
      NoFilter -> Right GetUTxO


getShelleyAddresses :: Set (Address Shelley) -> Set (Addr TPraosStandardCrypto)
getShelleyAddresses addresses = Set.map toShelleyAddr addresses

getShelleyStakeCredentials :: Set StakeAddress -> Set (Shelley.StakeCredential TPraosStandardCrypto)
getShelleyStakeCredentials addrs =
  Set.map getStakeCredential addrs
 where
  getStakeCredential :: StakeAddress -> Shelley.StakeCredential TPraosStandardCrypto
  getStakeCredential (StakeAddress _ cred) = cred


renderLocalStateQueryError :: LocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    --ByronAddressesNotSupportedError byronAddrs ->
    --  "The attempted local state query does not support Byron addresses: " <> show byronAddrs
  --  NonStakeAddressesNotSupportedError addrs ->
  --    "The attempted local state query does not support non-stake addresses: " <> show addrs

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
newtype DelegationsAndRewards
  = DelegationsAndRewards
      (Map (RewardAcnt TPraosStandardCrypto) (Maybe (Hash StakePoolKey), Coin))
  deriving Generic
  deriving newtype NoUnexpectedThunks

instance NoUnexpectedThunks (Hash StakePoolKey) where
    whnfNoUnexpectedThunks _ _ = pure NoUnexpectedThunks

instance ToJSON DelegationsAndRewards where
  toJSON (DelegationsAndRewards delegsAndRwds) =
      Aeson.Object $
        Map.foldlWithKey' delegAndRwdToJson HMS.empty delegsAndRwds
    where
      delegAndRwdToJson
        :: HashMap Text Aeson.Value
        -> RewardAcnt TPraosStandardCrypto
        -> (Maybe (Hash StakePoolKey), Coin)
        -> HashMap Text Aeson.Value
      delegAndRwdToJson acc _k (_d, _r) =
        HMS.insert
          (panic "")--(addressToHex $ AddressShelleyReward k) Implement a SerialiseAsRawByres instance for rewardacnt
          (panic "") -- (Aeson.object ["delegation" .= d, "rewardAccountBalance" .= r])
          acc

-- | Query the current protocol parameters from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryPParamsFromLocalState
  :: blk ~ ShelleyBlock TPraosStandardCrypto
  => NetworkId
  -> SocketPath
  -> Point blk
  -> ExceptT LocalStateQueryError IO PParams
queryPParamsFromLocalState network socketPath point = do
  let pointAndQuery = (point, GetCurrentPParams)
  newExceptT $ liftIO $
    queryNodeLocalState
      nullTracer
      (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolShelley)
      network
      socketPath
      pointAndQuery

-- | Query the current stake distribution from a Shelley node via the local
-- state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryStakeDistributionFromLocalState
  :: blk ~ ShelleyBlock TPraosStandardCrypto
  => NetworkId
  -> SocketPath
  -> Point blk
  -> ExceptT LocalStateQueryError IO (Ledger.PoolDistr TPraosStandardCrypto)
queryStakeDistributionFromLocalState network socketPath point = do
  let pointAndQuery = (point, GetStakeDistribution)
  newExceptT $ liftIO $
    queryNodeLocalState
      nullTracer
      (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolShelley)
      network
      socketPath
      pointAndQuery

queryLocalLedgerState
  :: blk ~ ShelleyBlock TPraosStandardCrypto
  => NetworkId
  -> SocketPath
  -> Point blk
  -> ExceptT LocalStateQueryError IO (Either LByteString (Ledger.EpochState TPraosStandardCrypto))
queryLocalLedgerState network socketPath point = do
  lbs <- fmap unSerialised <$>
            newExceptT . liftIO $
              queryNodeLocalState
                nullTracer
                (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolShelley)
                network
                socketPath
                (point, GetCBOR GetCurrentEpochState) -- Get CBOR-in-CBOR version
  -- If decode as a LedgerState fails we return the ByteString so we can do a generic
  -- CBOR decode.
  case decodeFull lbs of
    Right lstate -> pure $ Right lstate
    Left _ -> pure $ Left lbs

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.
--
-- This one is Shelley-specific because the query is Shelley-specific.
--
queryDelegationsAndRewardsFromLocalState
  :: NetworkId
  -> SocketPath
  -> Set StakeAddress
  -> Point (ShelleyBlock TPraosStandardCrypto)
  -> ExceptT LocalStateQueryError IO DelegationsAndRewards
queryDelegationsAndRewardsFromLocalState network socketPath addrs point = do
    let creds = getShelleyStakeCredentials addrs
        pointAndQuery = (point, GetFilteredDelegationsAndRewardAccounts creds)
    res <- newExceptT $ liftIO $
      queryNodeLocalState
        nullTracer
        (pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolShelley)
        network
        socketPath
        pointAndQuery
    pure $ toDelegsAndRwds res
  where
    --toDelegsAndRwds :: (Map (Credential Staking TPraosStandardCrypto) (KeyHash StakePool TPraosStandardCrypto), Ledger.RewardAccounts TPraosStandardCrypto) -> DelegationsAndRewards
    toDelegsAndRwds _ = panic "TODO" --(delegs, rwdAcnts)
    --  DelegationsAndRewards $
    --    Map.mapWithKey
    --      (\k v -> (Map.lookup (Ledger.getRwdCred k) delegs, v))
    --      rwdAcnts

-- -------------------------------------------------------------------------------------------------

-- | Establish a connection to a node and execute the provided query
-- via the local state query protocol.
--
-- This one is not specific to any era.
--
queryNodeLocalState
  :: forall blk result. RunNode blk
  => Trace IO Text
  -> CodecConfig blk
  -> NetworkId
  -> SocketPath
  -> (Point blk, Query blk result)
  -> IO (Either LocalStateQueryError result)
queryNodeLocalState trce cfg nm (SocketPath socketPath) pointAndQuery = do
    logInfo trce $ "queryNodeLocalState: Connecting to node via " <> textShow socketPath
    NodeToClient.withIOManager $ \iocp -> do
      resultVar <- newEmptyTMVarM
      NodeToClient.connectTo
        (NodeToClient.localSnocket iocp socketPath)
        NetworkConnectTracers
          { nctMuxTracer = muxTracer
          , nctHandshakeTracer = handshakeTracer
          }
        (localInitiatorNetworkApplication trce cfg nm resultVar pointAndQuery)
        socketPath
      atomically $ takeTMVar resultVar
  where
    muxTracer :: Show peer => Tracer IO (WithMuxBearer peer MuxTrace)
    muxTracer = toLogObject $ appendName "Mux" trce

    handshakeTracer :: Tracer IO
                        (WithMuxBearer (ConnectionId LocalAddress)
                        (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)))
    handshakeTracer = toLogObject $ appendName "Handshake" trce

localInitiatorNetworkApplication
  :: forall blk result. RunNode blk
  => Trace IO Text
  -> CodecConfig blk
  -> NetworkId
  -> StrictTMVar IO (Either LocalStateQueryError result)
  -> (Point blk, Query blk result)
  -> Versions
      NodeToClientVersion
      DictVersion
      (OuroborosApplication 'InitiatorMode LocalAddress LByteString IO (Either LocalStateQueryError result) Void)
localInitiatorNetworkApplication trce cfg nm
                                 resultVar pointAndQuery =
    NodeToClient.foldMapVersions
      (\v ->
        NodeToClient.versionedNodeToClientProtocols
          (nodeToClientProtocolVersion proxy v)
          versionData
          (\_ _ -> protocols v))
      (supportedNodeToClientVersions proxy)
  where
    proxy :: Proxy blk
    proxy = Proxy

    versionData = NodeToClientVersionData { networkMagic = toNetworkMagic nm }

    protocols clientVersion =
        NodeToClientProtocols
          { localChainSyncProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  nullTracer
                  cChainSyncCodec
                  NodeToClient.chainSyncPeerNull

          , localTxSubmissionProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  nullTracer
                  cTxSubmissionCodec
                  NodeToClient.localTxSubmissionPeerNull

          , localStateQueryProtocol =
              InitiatorProtocolOnly $
                MuxPeer
                  (contramap (Text.pack . show) . toLogObject $
                    appendName "cardano-local-state-query" trce)
                  cStateQueryCodec
                  (localStateQueryClientPeer (localStateQueryClient pointAndQuery resultVar))
          }
      where
        Codecs
          { cChainSyncCodec
          , cTxSubmissionCodec
          , cStateQueryCodec
          } = defaultCodecs cfg clientVersion

-- | A 'LocalStateQueryClient' which executes the provided local state query
-- and puts the result in the provided 'StrictTMVar'.
localStateQueryClient
  :: forall block query m result. (Applicative m, MonadIO m, MonadSTM m)
  => (Point block, query result)
  -> StrictTMVar m (Either LocalStateQueryError result)
  -> LocalStateQueryClient block query m (Either LocalStateQueryError result)
localStateQueryClient (point, query) resultVar =
  LocalStateQueryClient $ pure $ SendMsgAcquire point $
    ClientStAcquiring
      { recvMsgAcquired = SendMsgQuery query $
          ClientStQuerying
            { recvMsgResult = \result -> do
                void $ atomically $ tryPutTMVar resultVar (Right result)
                pure $ SendMsgRelease $ SendMsgDone (Right result)
            }
      , recvMsgFailure = \failure -> do
          void $ atomically $ tryPutTMVar resultVar (Left $ AcquireFailureError failure)
          pure $ SendMsgDone (Left $ AcquireFailureError failure)
      }
