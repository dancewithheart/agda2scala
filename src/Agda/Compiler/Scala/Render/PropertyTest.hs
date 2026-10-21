module Agda.Compiler.Scala.Render.PropertyTest
  ( printPropertySuite
  ) where

import Data.List (intercalate)

import Agda.Compiler.Scala.IR.ScalaExpr
  ( ScalaName
  , ScalaProperty(..)
  , ScalaTerm
  , SeVar(..)
  )

printPropertySuite
  :: (ScalaTerm -> String)
  -> [ScalaName]
  -> [ScalaProperty]
  -> String
printPropertySuite render moduleParts properties =
  unlines $
    [ "package " <> intercalate "." (init moduleParts)
    , ""
    , "import zio.test._"
    , "import " <> moduleObject <> "._"
    , ""
    , "object " <> specName <> " extends ZIOSpecDefault {"
    , "  def spec = suite(" <> show moduleObject <> ")("
    ]
    <> intercalateProperties (map printProperty properties)
    <> [ "  )"
       , "}"
       ]
  where
    moduleObject = last moduleParts
    specName = moduleObject <> "PropertySpec"

    intercalateProperties [] = []
    intercalateProperties [x] = x
    intercalateProperties (x:xs) =
      init x <> [last x <> ","] <> intercalateProperties xs

    printProperty p =
      [ "    test(" <> show (spName p) <> ") {"
      , "      check(Gen.long(0L, 1000000L)) { " <> arg <> " =>"
      , "        assertTrue(("
          <> render (spLeft p)
          <> ") == ("
          <> render (spRight p)
          <> "))"
      , "      }"
      , "    }"
      ]
      where
        SeVar arg _ = spArg p
