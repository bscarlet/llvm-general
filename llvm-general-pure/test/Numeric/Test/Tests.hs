module Numeric.Test.Tests where

import Test.Framework

import qualified Numeric.Test.SoftFloat as SoftFloat

tests = testGroup "Numeric" [
    SoftFloat.tests
  ]
