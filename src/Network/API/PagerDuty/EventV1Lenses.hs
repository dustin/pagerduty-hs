{-|
Module      : Network.API.PagerDuty.EventV1Lenses
Description : Lenses and prisms for the PagerDuty Event V1 interface.
Copyright   : (c) Dustin Sallings, 2021
License     : BSD3
Maintainer  : dustin@spy.net
Stability   : experimental

Lenses and prisms for the PagerDuty Event V1 interface.
-}
{-# LANGUAGE TemplateHaskell #-}

module Network.API.PagerDuty.EventV1Lenses (
  _Link, _Image,
  teServiceKey, teIncidentKey, teDescription, teDetails, teClient, teClientURL, teContexts,
  _Acknowledge, _Resolve,
  updateType, updateServiceKey, updateIncidentKey, updateDescription, updateDetails
  ) where

import           Control.Lens                  (makeLenses, makePrisms)
import           Network.API.PagerDuty.EventV1

makePrisms ''Context

makeLenses ''TriggerEvent

makePrisms ''UpdateType

makeLenses ''UpdateEvent
