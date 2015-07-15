{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
module Data.Docker (Event(eventStatus, eventId, eventFrom, eventTime)) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Control.Monad (mzero)
import Data.Data (Typeable, Data)
import Data.Aeson ((.:), (.=), object, FromJSON(..), ToJSON(..), Value(..))
import Data.Text.Lazy (Text)
    
data Event = Event { eventStatus :: Text
                   , eventId :: Text
                   , eventFrom ::Text
                   , eventTime :: Int
                   } deriving (Eq, Show, Typeable, Data, Generic)

instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "status"
                               <*> v .: "id"
                               <*> v .: "from"
                               <*> v .: "time"
  parseJSON _ = mzero

instance ToJSON Event where
  toJSON (Event eventStatus eventId eventFrom eventTime) = object [ "status" .= eventStatus
                                                                  , "id" .= eventId
                                                                  , "from" .= eventFrom
                                                                  , "time" .= eventTime
                                                                  ]
