{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
module Data.Docker (Event) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Data (Typeable, Data)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
    
data Event = Event { eventStatus :: Text
                   , eventId :: Text
                   , eventFrom ::Text
                   , eventTime :: Int
                   } deriving (Eq, Show, Typeable, Data, Generic)

instance FromJSON Event
instance ToJSON Event
