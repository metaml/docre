{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
module Data.Docker (Event(eventStatus, eventId, eventFrom, eventTime)) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Control.Monad (mzero)
import Data.Data (Typeable, Data)
import Data.Aeson ((.:), (.=), object, FromJSON(..), ToJSON(..), Value(..))
import Data.Text (Text)
    
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
  toJSON (Event eStatus eId eFrom eTime) = object [ "status" .= eStatus
                                                  , "id" .= eId
                                                  , "from" .= eFrom
                                                  , "time" .= eTime
                                                  ]
