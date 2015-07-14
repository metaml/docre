{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
module Data.Docker (Event(eventStatus, eventId, eventFrom, eventTime)) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Data (Typeable, Data)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy (Text)
    
data Event = Event { eventStatus :: Text
                   , eventId :: Text
                   , eventFrom ::Text
                   , eventTime :: Int
                   } deriving (Eq, Show, Typeable, Data, Generic)

instance FromJSON Event
instance ToJSON Event
