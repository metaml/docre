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
                      
data StartNetworkSettings = StartNetworkSettings {_csnsGateway :: Text
                                                 ,_csnsIPAddress :: Text
                                                 ,_csnsIPPrefixLen :: Int
                                                 ,_csnsMacAddress :: Text
                                                 ,_csnsNetworkID :: Text
                                                 ,_csnsPortMapping :: Maybe Text
                                                 ,_csnsPorts :: Object
                                                 ,_csnsName :: Text
                                                 } deriving (Eq, Show, Typeable, Data, Generic)
                          
data StartConfig = StartConfig {_cscHostname :: Text
                               ,_cscDomain :: Text
                               ,_cscExposedPorts :: Object
                               ,_cscEnv :: Array
                               ,_cscImage :: Text
                               } deriving (Eq, Show, Typeable, Data, Generic)
                                                        
data StartResponse = StartResponse {_csrId :: Text
                                   ,_csrName :: Text
                                   ,_csrState :: StartState
                                   ,_csrNetworkSettings :: StartNetworkSettings
                                   ,_csrConfig :: StartConfig
                                   } deriving (Eq, Show, Typeable, Data, Generic)

instance ToJSON Event where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = map toLower . (drop 1)}
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
                                           
