{-# LANGUAGE DataKinds #-}

module BlockchainsQueryApi.Prelude
    ( module Exports
    , Error(..)
    , URI
    ) where

import Protolude as Exports
import Network.URI (URI)

data Error = NodeError Text | InvalidInput Text | RPCError Text | NotFound Text deriving (Eq, Show)
instance Exception Error