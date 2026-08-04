{-# Language BlockArguments #-}
{-# Language LambdaCase #-}
module Main(main) where

import Numeric.MathFunctions.Comparison (within)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@=?), assertBool, assertEqual, assertFailure, testCase)

import LibBF


main :: IO ()
main =
  defaultMain $
  testGroup "LibBF tests"
    [ testGroup "bfToString"
        [ testCase "NaN" $
            "NaN" @=? bfToString 16 (showFree Nothing) bfNaN
        ]
    , testGroup "bfFromString"
        [ testCase "Underflow" $
            let (_, status) =
                  bfFromString 10 (expBits 3 <> precBits 2 <> rnd ToZero) "0.001" in
            True @=? statusUnderflow status
        , testCase "Overflow" $
            let (_, status) =
                  bfFromString 10 (expBits 3 <> precBits 2 <> rnd ToZero) "1.0e200" in
            True @=? statusOverflow status
        ]
    , testGroup "bfAdd"
        [ dblTestCase "+" (+) (bfAdd (float64 NearEven)) 1 2
        ]
    , testGroup "bfDiv"
        [ dblTestCase "/" (/) (bfDiv (float64 NearEven)) 1 0
        ]
    , testGroup "bfIsSubnormal (float32 NearEven)"
        (map (\bf -> bfSubnormalTestCase bf False)
             [bfPosZero, bfFromInt 1, bfFromInt 0, bfNaN, bfNegInf, bfPosInf])
    , testGroup "IEEE 754 compare"
      [ testGroup "Comparisons with NaN should always return False"
        [ testCase "NaN > 0" $ False @=? bfNaN > bfPosZero
        , testCase "0 > NaN" $ False @=? bfPosZero > bfNaN
        , testCase "NaN >= 0" $ False @=? bfNaN >= bfPosZero
        , testCase "0 >= NaN" $ False @=? bfPosZero >= bfNaN
        , testCase "NaN < 0" $ False @=? bfNaN < bfPosZero
        , testCase "0 < NaN" $ False @=? bfPosZero < bfNaN
        , testCase "NaN <= 0" $ False @=? bfNaN <= bfPosZero
        , testCase "0 <= NaN" $ False @=? bfPosZero <= bfNaN
        ]
      ]
    , testGroup "Transcendental functions"
      [ -- LibBF's implementation of atan2 is ever-so-slightly difference from
        -- macOS libc's implementation, so check that their results are within
        -- 1 ULP rather than checking for IEEE equality.
        dblWithin1UlpTestCase "atan2 1 2" atan2 (bfAtan2 (float64 NearEven)) 1 2
      , dblWithin1UlpTestCase "atan2 2 1" atan2 (bfAtan2 (float64 NearEven)) 2 1
      , checkPredicateTestCase "sin" (bfSin (float256 NearEven)) (bfIsZero) (bfFromDouble 0)
      , checkPredicateTestCase "exp" (bfExp (float256 NearEven)) (== (bfFromDouble 1)) (bfFromDouble 0)
      ]
    ]

statusUnderflow :: Status -> Bool
statusUnderflow Underflow = True
statusUnderflow _         = False

statusOverflow :: Status -> Bool
statusOverflow Overflow = True
statusOverflow _        = False

-- Check that a binary operation over BigFloats returns the same result as the
-- corresponding operation over doubles.
dblTestCase ::
  String ->
  (Double -> Double -> Double) ->
  (BigFloat -> BigFloat -> (BigFloat, Status)) ->
  Double -> Double -> TestTree
dblTestCase = dblCompareTestCase (@=?)

-- Check that a binary operation over BigFloats returns approximately the same
-- result as the corresponding operation over doubles. Here, "approximately"
-- means "within 1 unit in the last place (ULP)". This is a very crude way to
-- check if two values are approximately equal, so use this with caution.
dblWithin1UlpTestCase ::
  String ->
  (Double -> Double -> Double) ->
  (BigFloat -> BigFloat -> (BigFloat, Status)) ->
  Double -> Double -> TestTree
dblWithin1UlpTestCase = dblCompareTestCase $ \expected actual ->
  assertBool "Values not within 1 ULP" $ within 1 expected actual

-- Construct a test case that compares the result of a binary BigFloat
-- operation against the same result as the corresponding operation over
-- doubles.
dblCompareTestCase ::
  -- How to compare the expected result against the actual result.
  (Double -> Double -> IO ()) ->
  String ->
  (Double -> Double -> Double) ->
  (BigFloat -> BigFloat -> (BigFloat, Status)) ->
  Double -> Double -> TestTree
dblCompareTestCase resCmp op opD opBF x y =
  testCase (unwords [show x, op, show y]) $
  case z1 of
    Left err -> assertFailure ("status: " ++ err)
    Right actual -> resCmp expected actual
  where
  expected = opD x y
  z1 = case opBF (bfFromDouble x) (bfFromDouble y) of
        (res,_) ->
          case bfToDouble NearEven res of
            (res1,Ok) -> Right res1
            (_, s)    -> Left ("result: " ++ show s)

-- Check that calling bfIsSubnormal on a BigFloat value returns the expected
-- result.
bfSubnormalTestCase :: BigFloat -> Bool -> TestTree
bfSubnormalTestCase bf expected =
  testCase (show bf) $
  expected @=? bfIsSubnormal (float32 NearEven) bf

checkPredicateTestCase ::
  Show a =>
  String ->
  (a -> (BigFloat, Status)) ->
  (BigFloat -> Bool) ->
  a ->
  TestTree
checkPredicateTestCase opName opBF predicate input =
  testCase opName $ do
    assertEqual
      ("Status '" ++ show bfStatus ++ "' not OK for " ++ describeOp)
      bfStatus
      Ok
    assertBool
      ("Test predicate failed on result " ++ show bfRes ++ " on " ++ describeOp)
      (predicate bfRes)
  where
    (bfRes, bfStatus) = opBF input
    describeOp = opName ++ "(" ++ show input ++ ")"
