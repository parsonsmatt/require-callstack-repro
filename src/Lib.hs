{-# language RankNTypes, MultiParamTypeClasses, DataKinds, ConstraintKinds, ImplicitParams, UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-methods #-}

module Lib where

import GHC.Stack (HasCallStack)
import GHC.Classes (IP(..))
import GHC.TypeLits (TypeError, ErrorMessage(..))

foo :: RequireCallStack => IO ()
foo = do
    errorRequireCallStack "asdf"

    let bar = errorRequireCallStack "qwer"

    bar

type RequireCallStack = (HasCallStack, RequireCallStackImpl)

type RequireCallStackImpl = ?provideCallStack :: ProvideCallStack

data ProvideCallStack = ProvideCallStack

errorRequireCallStack :: RequireCallStack => String -> x
errorRequireCallStack = error

instance TypeError ('Text "Add RequireCallStack to your function context or use provideCallStack") => IP "provideCallStack" ProvideCallStack

provideCallStack :: (RequireCallStackImpl => r) -> r
provideCallStack r = r
  where
    ?provideCallStack = ProvideCallStack

