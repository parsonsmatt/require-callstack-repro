{-# language RankNTypes, TypeFamilies, KindSignatures, MultiParamTypeClasses, DataKinds, ConstraintKinds, ImplicitParams, UndecidableInstances, FlexibleContexts, FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-methods #-}

module Lib where

import Data.Kind
import GHC.Stack (HasCallStack)
import GHC.Classes (IP(..))
import GHC.TypeLits (TypeError, ErrorMessage(..))

-- The Problem:
--
-- GHC complains about `bar` with the `TypeError` in the `instance IP
-- "provideCallStack" ProvideCallStack`.
--
-- Removing the type error allows the program to compile without error.
--
-- Adding a type signature to `bar` that specifies `RequireCallStack` also
-- fixes the issue.
someFunc :: RequireCallStack => IO ()
someFunc = do
    errorRequireCallStack "asdf"

    let bar = errorRequireCallStack "qwer"

    bar

alsoWeird :: IO ()
alsoWeird = provideCallStack $ do
    -- `RequireCallStack` should be a satisfied constraint here, as
    -- evidenced by this building:
    someFunc

    -- But we get an error in this let binding.
    let bar = errorRequireCallStack "qwer"

    bar

errorRequireCallStack :: RequireCallStack => String -> x
errorRequireCallStack = error

instance TypeError ('Text "Add RequireCallStack to your function context or use provideCallStack") => IP "provideCallStack" ProvideCallStack

type RequireCallStack = (HasCallStack, RequireCallStackImpl)

type RequireCallStackImpl = ?provideCallStack :: ProvideCallStack

data ProvideCallStack = ProvideCallStack

provideCallStack :: (RequireCallStackImpl => r) -> r
provideCallStack r = r
  where
    ?provideCallStack = ProvideCallStack
