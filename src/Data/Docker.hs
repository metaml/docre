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
                             ,_ssFinishedAT :: Text
                             } deriving (Eq, Show, Typeable, Data, Generic)
                      
data StartNetworkSettings = StartNetworkSettings {_snsBridge :: Text
                                                 ,_snsEndpointID :: Text
                                                 ,_snsGateway :: Text
                                                 ,_snsGlobalIPv6Address :: Text
                                                 ,_snsGlobalIpv6PrefixLen :: Int
                                                 ,_snnHairpinMode :: Bool
                                                 ,_snsIPAddress :: Text
                                                 ,_snsIPPrefixLen :: Int
                                                 ,_snsLinklIPv6Address :: Text
                                                 , _snsLinklIpv6PrefixLen :: Int
                                                 ,_snsMacAddress :: Text
                                                 ,_snsNetworkID :: Text
                                                 ,_snsPortMapping :: Maybe Text                                                                                                        ,_snsPorts :: Object
                                                 } deriving (Eq, Show, Typeable, Data, Generic)
                          
data StartConfig = StartConfig {_scHostname :: Text
                               ,_scDomain :: Text
                               ,_scExposedPorts :: Object
                               ,_scEnv :: Array
                               ,_scImage :: Text
                               } deriving (Eq, Show, Typeable, Data, Generic)
                                                        
data StartResponse = StartResponse {_srId :: Text
                                   ,_srCreated :: Text
                                   ,_srPath :: Text
                                   ,_srImage :: Text
                                   ,_srResolveConfPath :: Text
                                   ,_srHostnamePath :: Text                                                          
                                   ,_srHostsPath :: Text                                                          
                                   ,_srLogPath :: Text
                                   ,_srName :: Text
                                   ,_srRestartCount:: Int
                                   ,_srDrive :: Int                                                      
                                   ,_srExecDrive :: Int
                                   ,_srMountLabel :: Int
                                   ,_srProcessLabel :: Int
                                   ,_srVolumes :: Object
                                   ,_srVolumesRW :: Object
                                   ,_srAppArmorProfiled :: Text
                                   ,_srExecIds :: Maybe Text
                                   ,_srwNetworkDisabled :: Bool
                                   ,_srwMacAddress :: Text
                                   ,_srOnBuild :: Maybe Text
                                   ,_srLebels :: Object
                                   ,_srState :: StartState
                                   ,_srNetworkSettings :: StartNetworkSettings
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

instance ToJSON StartNetworkSettings where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 4}
instance FromJSON StartNetworkSettings where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 4}

instance ToJSON StartConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}
instance FromJSON StartConfig where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON StartResponse where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 3}
instance FromJSON StartResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 3}
                                           
