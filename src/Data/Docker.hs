{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Docker (Event) where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Data.Data (Typeable, Data)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
    
data Event = Event { status :: Text
                   , id :: Text
                   , from ::Text
                   , time :: Int
                   } deriving (Eq, Show, Typeable, Data, Generic)


instance FromJSON Event
instance ToJSON Event
