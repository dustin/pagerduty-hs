{-|
Module      : Network.API.PagerDuty.EventV1
Description : PagerDuty Event V1 interface.
Copyright   : (c) Dustin Sallings, 2021
License     : BSD3
Maintainer  : dustin@spy.net
Stability   : experimental

PagerDuty Event V1 interface.
-}
{-# LANGUAGE TemplateHaskell #-}

module Network.API.PagerDuty.EventV1 (
  -- * Triggering an Event
  TriggerEvent(..), TriggerEvent',
  Context(..),
  -- * Updating an Event
  UpdateEvent(..), UpdateEvent', UpdateType(..),
  -- * Delivering Events to PagerDuty
  deliver, Response(..),
  -- * Lenses and Prisms
  _Link, _Image,
  teServiceKey, teIncidentKey, teDescription, teDetails, teClient, teClientURL, teContexts,
  _Acknowledge, _Resolve,
  updateType, updateServiceKey, updateIncidentKey, updateDescription, updateDetails
  ) where

import           Control.Lens           (makeLenses, makePrisms, view)
import           Control.Monad.Catch    (MonadCatch (..), SomeException (..), catch)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..), encode, object, (.:), (.=))
import           Data.Aeson.Types       (Pair, typeMismatch)
import           Data.Char              (toLower)
import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text, pack)
import           Network.Wreq           (asJSON, post, responseBody)
import           Network.Wreq.Types     (Postable)

class (ToJSON j) => EventRequest j

-- | Context that may be added when creating an event.
data Context = Link Text (Maybe Text) -- ^ Link to a URL with an optional link description.
             | Image Text (Maybe Text) (Maybe Text) -- ^ Image URL, optional link ref, and optional alt text.
  deriving (Show, Eq)

makePrisms ''Context

optj :: ToJSON v => [(Text, Maybe v)] -> [Pair]
optj = mapMaybe (fmap (uncurry (.=)) . sequenceA)

instance ToJSON Context where
  toJSON (Link u t)      = object (["type" .= ("link" :: Text), "href" .= u] <> optj [("text", t)])
  toJSON (Image s mu mt) = object (["type" .= ("image" :: Text), "src" .= s] <> optj [("href", mu), ("alt", mt)])

-- | Request object to create an event.  Any value that may be
-- serialized to JSON maybe attached as details.
--
-- This may be delivered with the 'deliver' function.
data TriggerEvent a = TriggerEvent {
  _teServiceKey    :: Text
  , _teIncidentKey :: Maybe Text
  , _teDescription :: Text
  , _teDetails     :: Maybe a
  , _teClient      :: Text
  , _teClientURL   :: Text
  , _teContexts    :: [Context]
  }
  deriving (Show, Eq)

makeLenses ''TriggerEvent

instance ToJSON a => EventRequest (TriggerEvent a)

-- | A 'TriggerEvent' type that doesn't have details.
type TriggerEvent' = TriggerEvent ()

instance ToJSON a => ToJSON (TriggerEvent a) where
  toJSON TriggerEvent{..} = object ([
                                       "service_key" .= _teServiceKey
                                    , "event_type" .= ("trigger"::Text)
                                    , "description" .= _teDescription
                                    , "client" .= _teClient
                                    , "client_url" .= _teClientURL
                                    ] <> optj [("details", _teDetails)]
                                     <> optj [("incident_key", _teIncidentKey)]
                                     <> opta "contexts" _teContexts
                                   )
    where opta _ [] = []
          opta k vs = [k .= vs]

-- | An event update will either acknowledge or resolve an incident.
data UpdateType = Acknowledge | Resolve deriving (Show, Eq, Bounded, Enum)

makePrisms ''UpdateType

-- | UpdateEvent is the message for both acknowledging and resolving
-- incidents.  This may be delivered using the 'deliver' function.
data UpdateEvent a = UpdateEvent {
  _updateType          :: UpdateType
  , _updateServiceKey  :: Text
  , _updateIncidentKey :: Text
  , _updateDescription :: Text
  , _updateDetails     :: Maybe a
  }
  deriving (Show, Eq)

makeLenses ''UpdateEvent

instance ToJSON a => EventRequest (UpdateEvent a)

-- | A 'UpdateEvent' type that doesn't have details.
type UpdateEvent' = UpdateEvent ()

instance ToJSON a => ToJSON (UpdateEvent a) where
  toJSON UpdateEvent{..} = object ([
                                   "service_key" .= _updateServiceKey
                                , "event_type" .= (map toLower . show) _updateType
                                , "incident_key" .= _updateIncidentKey
                                , "description" .= _updateDescription
                                ] <> optj [("details", _updateDetails)])

-- | Response to a delivered message.
data Response = Failure Text Text -- ^ Failure status and message
              | Success Text      -- ^ Success and incident key for further updates
  deriving (Show, Eq)

instance FromJSON Response where
  parseJSON (Object v) = subparse =<< v .: "status"
      where
        subparse "success" = Success <$> v .: "incident_key"
        subparse e         = Failure e <$> v .: "message"
  parseJSON invalid = typeMismatch "Response" invalid

jpost :: (MonadIO m, Postable a, FromJSON r) => String -> a -> m r
jpost u v = view responseBody <$> liftIO (post u v >>= asJSON)

-- | Deliver a 'TriggerEvent' or 'UpdateEvent'.
deliver :: (EventRequest r, MonadCatch m, MonadIO m) => r -> m Response
deliver r = send r `catch` failed
  where
    send = jpost "https://events.pagerduty.com/generic/2010-04-15/create_event.json" . encode
    failed (SomeException e) = pure $ Failure "http exception" (pack (show e))
