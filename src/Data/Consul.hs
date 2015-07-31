{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
module Data.Consul where

import GHC.Generics (Generic)
import Data.Data (Typeable, Data)
import Data.Aeson (FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Object, Array, defaultOptions, fieldLabelModifier)
import Data.Text (Text)

data RegisterNode = RegisterNode {_rnDatacenter :: Maybe Datacenter
                                 ,_rnNode :: Text
                                 ,_rnAddress :: Text
                                 ,_rnService :: Maybe Service
                                 ,_rnCheck :: Maybe Check
                                 } deriving (Eq, Show, Typeable, Data, Generic)

data DeregisterNode = DeregisterNode {_dnDatacenter :: Maybe Datacenter
                                     ,_dnNode :: Text
                                     } deriving (Eq, Show, Typeable, Data, Generic)

data Datacenter = Datacenter Text
                  deriving (Eq, Show, Typeable, Data, Generic)
                
data Service = Service {_sId :: Text
                       ,_sService :: Text
                       ,_sTags :: [Text]
                       ,_sAddress :: Maybe Text
                       ,_sPort :: Maybe Int
                       } deriving (Eq, Show, Typeable, Data, Generic)

data Check = Check {_cNode :: Text
                   ,_cCheckId :: Text
                   ,_cName :: Maybe Text
                   ,_cNotes :: Maybe Text
                   ,_cServiceId :: Maybe Text
                   ,_cStatus :: HealthCheckStatus
                   ,_cOutput :: Text
                   ,_cServiceName :: Maybe Text
                   } deriving (Eq, Show, Typeable, Data, Generic)

data HealthCheck = Script Text Text | Ttl Text | Http Text
                   deriving (Ord, Eq, Show, Typeable, Data, Generic)

data HealthCheckStatus = Critical | Passing | Unknown | Warning
                         deriving (Enum, Ord, Eq, Show, Typeable, Data, Generic)

instance ToJSON RegisterNode where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}
instance FromJSON RegisterNode where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON DeregisterNode where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}
instance FromJSON DeregisterNode where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON Datacenter where
  toJSON = genericToJSON defaultOptions
instance FromJSON Datacenter where
  parseJSON = genericParseJSON defaultOptions
              
instance ToJSON Service where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON Service where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON Check where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}
instance FromJSON Check where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON HealthCheck where
  toJSON = genericToJSON defaultOptions
instance FromJSON HealthCheck where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON HealthCheckStatus where
  toJSON = genericToJSON defaultOptions
instance FromJSON HealthCheckStatus where
  parseJSON = genericParseJSON defaultOptions

data ConsulStartState = ConsulStartState {_cssRunning :: Bool
                                         ,_cssPaused :: Bool
                                         ,_cssRestarting :: Bool                                                       
                                         ,_cssOOMKilled :: Bool
                                         ,_cssDead :: Bool
                                         ,_cssPid :: Int
                                         ,_cssExitCode :: Int
                                         ,_cssError :: Text
                                         ,_cssStartedAt :: Text
                                         ,_cssFinishedAT :: Text
                                         } deriving (Eq, Show, Typeable, Data, Generic)
                      
data ConsulStartNetworkSettings = ConsulStartNetworkSettings {_csnsGateway :: Text
                                                             ,_csnsIPAddress :: Text
                                                             ,_csnsIPPrefixLen :: Int
                                                             ,_csnsMacAddress :: Text
                                                             ,_csnsNetworkID :: Text
                                                             ,_csnsPortMapping :: Maybe Text
                                                             ,_csnsPorts :: Object
                                                             ,_csnsName :: Text
                                                             } deriving (Eq, Show, Typeable, Data, Generic)

data ConsulStartConfig = ConsulStartConfig {_cscHostname :: Text
                                           ,_cscDomain :: Text
                                           ,_cscExposedPorts :: Object
                                           ,_cscEnv :: Array
                                           ,_cscImage :: Text
                                           } deriving (Eq, Show, Typeable, Data, Generic)
                                                        
data ConsulStartResponse = ConsulStartResponse {_csrId :: Text
                                               ,_csrName :: Text
                                               ,_csrState :: ConsulStartState
                                               ,_csrNetworkSettings :: ConsulStartNetworkSettings
                                               ,_csrConfig :: ConsulStartConfig
                                               } deriving (Eq, Show, Typeable, Data, Generic)
                                  
instance ToJSON ConsulStartState where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}
instance FromJSON ConsulStartState where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON ConsulStartNetworkSettings where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 4}
instance FromJSON ConsulStartNetworkSettings where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 4}

instance ToJSON ConsulStartConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}
instance FromJSON ConsulStartConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON ConsulStartResponse where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}
instance FromJSON ConsulStartResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}
              
