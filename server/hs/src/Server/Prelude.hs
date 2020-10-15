{-# LANGUAGE NoImplicitPrelude #-}
module Server.Prelude
( module Control.Category
, (<=<)
, (>=>)
, flip
, IO
, IsString
, ReaderT
, ByteString
, ask
, coerce
, Coercible
, MonadIO(liftIO)
, (<$>)
, ($)
, Monad(..)
, Applicative(..)
, Functor(..)
, guard
, Eq(..)
, Either(..)
, (||)
, Text
, Proxy(..)
, Char
, Show(show)
, Exception
, MonadThrow(..)
, MonadCatch(..)
, Ord(..)
, otherwise
, Num(..)
, forkIO
, forM_
, Semigroup(..)
, Monoid(..)
, void
, putStrLn
, threadDelay
, forever
) where

import Control.Concurrent
import Data.Foldable (forM_)
import Prelude hiding
  ( id, (.)
  )
import Control.Exception (Exception)
import Data.Text (Text)
import Control.Monad (guard, void, (<=<), (>=>), forever)
import Control.Category
import Data.ByteString (ByteString)
import Data.String (IsString)
import Control.Monad.Reader (ReaderT, ask)
import Data.Coerce (coerce, Coercible)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Proxy(Proxy(..))
import Control.Monad.Catch
