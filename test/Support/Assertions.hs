module Support.Assertions
  ( assertEqualPretty
  , assertStringEqual
  , assertLeft
  , assertRight
  ) where

import Control.Monad (unless)
import Test.Tasty.HUnit (Assertion, assertFailure)

assertEqualPretty :: (Eq a, Show a) => String -> a -> a -> Assertion
assertEqualPretty label expected actual =
    unless (expected == actual) $
        assertFailure $
            unlines
                [ label
                , ""
                , "Expected:"
                , indent (show expected)
                , ""
                , "Actual:"
                , indent (show actual)
                ]

assertStringEqual :: String -> String -> String -> Assertion
assertStringEqual label expected actual =
    unless (expected == actual) $
        assertFailure $
            unlines
                [ label
                , ""
                , "Expected:"
                , fence expected
                , ""
                , "Actual:"
                , fence actual
                ]

assertLeft :: (Show a) => String -> Either e a -> Assertion
assertLeft label actual =
    case actual of
        Left _ ->
            pure ()
        Right value ->
            assertFailure $
                unlines
                    [ label
                    , ""
                    , "Expected:"
                    , "  Left _"
                    , ""
                    , "Actual:"
                    , indent (show value)
                    ]

assertRight :: (Show e) => String -> Either e a -> Assertion
assertRight label actual =
    case actual of
        Right _ ->
            pure ()
        Left err ->
            assertFailure $
                unlines
                    [ label
                    , ""
                    , "Expected:"
                    , "  Right _"
                    , ""
                    , "Actual:"
                    , indent (show err)
                    ]

indent :: String -> String
indent =
    unlines . fmap ("  " <>) . lines

fence :: String -> String
fence s =
    unlines
        [ "```"
        , s
        , "```"
        ]