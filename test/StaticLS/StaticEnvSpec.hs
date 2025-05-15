{-# LANGUAGE OverloadedRecordDot #-}

module StaticLS.StaticEnvSpec (spec) where

import Data.Path (AbsPath)
import Data.Path qualified as Path
import StaticLS.StaticEnv (allSrcDirs, immutableSrcDirs, initStaticEnv, mutableSrcDirs)
import StaticLS.StaticEnv.Options (defaultStaticEnvOptions, optionImmutableSrcDirs, optionSrcDirs)
import Test.Hspec

spec :: Spec
spec = describe "initStaticEnv merge" $ do
  let check name mutables immutabless = it name $ do
        root <- Path.filePathToAbs "/proj"

        -- set immutable and mutable
        let opts =
              defaultStaticEnvOptions
                { optionSrcDirs = mutables
                , optionImmutableSrcDirs = immutabless
                }

        -- make env
        env <- initStaticEnv root opts

        -- make absolute paths
        let absList xs = map ((root Path.</>) . Path.filePathToRel) xs

        -- ensure options correlate to env list
        env.mutableSrcDirs `shouldBe` absList mutables
        env.immutableSrcDirs `shouldBe` absList immutabless
        env.allSrcDirs `shouldBe` absList (mutables ++ immutabless)

  check "only mutable dirs" ["a", "b"] []
  check "only immutable dirs" [] ["a", "b"]
  check "mutable abd immutable dirs" ["x"] ["y", "z"]
  check "both empty" [] []
