{-# LANGUAGE DataKinds #-}

module BlockchainsQueryApi.Prelude
    ( module Exports
    , Error(..)
    , URI
    , mapLeft
    ) where

import Protolude as Exports
import Network.URI (URI)
import Data.Either.Combinators (mapLeft)

data Error = NodeError Text | InvalidInput Text | RPCError Text | NotFound Text deriving (Eq, Show)
instance Exception Error