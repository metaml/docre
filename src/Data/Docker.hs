{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
module Data.Docker (Event(..), StartResponse(..), StartConfig(..), StartNetworkSettings(..), StartState(..)) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Data (Typeable, Data)
import Data.Aeson (FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Object, Array, defaultOptions, fieldLabelModifier)
import Data.Text (Text)
import Data.Char (toLower)
    
data Event = Event {_eStatus :: Text
                   ,_eId :: Text
                   ,_eFrom ::Text
                   ,_eTime :: Int
                   } deriving (Eq, Show, Typeable, Data, Generic)

data StartState = StartState {_ssRunning :: Bool
                             ,_ssPaused :: Bool
                             ,_ssRestarting :: Bool                                                       
                             ,_ssOOMKilled :: Bool
                             ,_ssDead :: Bool
                             ,_ssPid :: Int
                             ,_ssExitCode :: Int
                             ,_ssError :: Text
                             ,_ssStartedAt :: Text
                             ,_ssFinishedAt :: Text
                             } deriving (Eq, Show, Typeable, Data, Generic)
                      
data StartLogConfig = StartLogConfig {_slcType :: Text
                                     ,_slcConfig :: Object
                                     } deriving (Eq, Show, Typeable, Data, Generic)
                          
data StartRestartPolicy = StartRestartPolicy {_srpName :: Text
                                             ,_srpMaximumRetryCount :: Int
                                             } deriving (Eq, Show, Typeable, Data, Generic)

data StartHostConfig = StartHostConfig{_shcBinds :: Maybe Object
                                      ,_shcContainerIDFile :: Text
                                      ,_shcLxcConf :: Array
                                      ,_shcMemory :: Int
                                      ,_shcMemorySwap :: Int
                                      ,_shcCpuShares :: Int
                                      ,_shcCpuPeriod :: Int
                                      ,_shcCpusetCpus :: Text
                                      ,_shcCpusetMems :: Text
                                      ,_shcCpuQuota :: Int
                                      ,_shcBlkioWeight :: Int
                                      ,_shcOomKillDisable :: Bool
                                      ,_shcPrivileged :: Bool
                                      ,_shcPortBindings :: Object
                                      ,_shcLinks :: Maybe Object
                                      ,_shcPublishAllPorts :: Bool
                                      ,_shcDns :: Maybe Object
                                      ,_shcDnsSearch :: Maybe Object
                                      ,_shcExtraHosts :: Maybe Object
                                      ,_shcVolumesFrom :: Maybe Object
                                      ,_shcDevices :: Array
                                      ,_shcNetworkMode :: Text
                                      ,_shcIpcMode :: Text
                                      ,_shcPidMode :: Text
                                      ,_shcUTSMode :: Text
                                      ,_shcCapAdd :: Maybe Object
                                      ,_shcCapDrop :: Maybe Object
                                      ,_shcRestartPolicy :: StartRestartPolicy
                                      ,_shcSecurityOpt :: Maybe Object
                                      ,_shcReadonlyRootfs :: Bool
                                      ,_shcUlimits :: Maybe Object
                                      ,_shcLogConfig :: StartLogConfig
                                      ,_shcCgroupParent :: Text
                                      } deriving (Eq, Show, Typeable, Data, Generic)

data StartConfig = StartConfig {_scHostname :: Text
                               ,_scDomainname :: Text
                               ,_scUser :: Text
                               ,_scAttachStdin :: Bool
                               ,_scAttachStdout :: Bool
                               ,_scAttachStderr :: Bool
                               ,_scPortSpecs :: Maybe Object
                               ,_scExposedPorts :: Object
                               ,_scTty :: Bool
                               ,_scOpenStdin :: Bool
                               ,_scStdinOnce :: Bool
                               ,_scEnv :: Array
                               ,_scCmd :: Maybe Text
                               ,_scImage :: Text
                               ,_scVolumes :: Maybe Array
                               ,_scVolumeDriver :: Text
                               ,_scEntrypoint :: Array
                               ,_scNetworkDisabled :: Bool
                               ,_scMacAddress :: Text
                               ,_scOnBuild :: Maybe Array
                               ,_scLabels :: Object
                               } deriving (Eq, Show, Typeable, Data, Generic)

data StartNetworkSettings = StartNetworkSettings {_snsBridge :: Text
                                                 ,_snsEndpointID :: Text
                                                 ,_snsGateway :: Text
                                                 ,_snsGlobalIPv6Address :: Text
                                                 ,_snsGlobalIPv6PrefixLen :: Int
                                                 ,_snsHairpinMode :: Bool
                                                 ,_snsIPAddress :: Text
                                                 ,_snsIPPrefixLen :: Int
                                                 ,_snsIPv6Gateway :: Text
                                                 ,_snsLinkLocalIPv6Address :: Text
                                                 ,_snsLinkLocalIPv6PrefixLen :: Int
                                                 ,_snsMacAddress :: Text
                                                 ,_snsNetworkID :: Text
                                                 ,_snsPortMapping :: Maybe Object
                                                 ,_snsPorts :: Maybe Object
                                                 ,_snsSandboxKey :: Text
                                                 ,_snsSecondaryIPAddresses :: Maybe Object
                                                 ,_snsSecondaryIPv6Addresses :: Maybe Object
                                                 } deriving (Eq, Show, Typeable, Data, Generic)

data StartResponse = StartResponse {_srId :: Text
                                   ,_srCreated :: Text
                                   ,_srPath :: Text
                                   ,_srArgs :: Array
                                   ,_srState :: StartState
                                   ,_srImage :: Text
                                   ,_srNetworkSettings :: StartNetworkSettings
                                   ,_srResolvConfPath :: Text
                                   ,_srHostnamePath :: Text                                                          
                                   ,_srHostsPath :: Text                                                          
                                   ,_srLogPath :: Text
                                   ,_srName :: Text
                                   ,_srRestartCount:: Int
                                   ,_srDriver :: Text
                                   ,_srExecDriver :: Text
                                   ,_srMountLabel :: Text
                                   ,_srProcessLabel :: Text
                                   ,_srVolumes :: Maybe Object
                                   ,_srVolumesRW :: Maybe Object
                                   ,_srAppArmorProfile :: Text
                                   ,_srExecIDs :: Maybe Text
                                   ,_srHostConfig :: StartHostConfig
                                   ,_srConfig :: StartConfig
                                   } deriving (Eq, Show, Typeable, Data, Generic)

instance ToJSON Event where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = map toLower . (drop 2)}
instance FromJSON Event where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = map toLower . (drop 2)}

instance ToJSON StartState where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}
instance FromJSON StartState where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON StartLogConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 4}
instance FromJSON StartLogConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 4}
              
instance ToJSON StartRestartPolicy where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 4}
instance FromJSON StartRestartPolicy where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 4}
              
instance ToJSON StartHostConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 4}
instance FromJSON StartHostConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 4}
              
instance ToJSON StartConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}
instance FromJSON StartConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON StartNetworkSettings where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 4}
instance FromJSON StartNetworkSettings where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 4}

instance ToJSON StartResponse where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}
instance FromJSON StartResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}
                                           
