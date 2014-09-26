module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map
import Data.List
import Data.String.Utils
import Data.Ord
import System.FilePath

type Path = String
type PathSegment = String
data PathMapping = PathMapping PathSegment PathSegment
data SubstitutionRule = SubstitutionRule [PathMapping]

something :: PathMapping -> Bool
something ("path/segment", "path/segment")

buildRule :: [(PathSegment, PathSegment)] -> SubstitutionRule
buildRule pathSegmentTuples =
  SubstitutionRule $ map buildPathMapping pathSegmentTuples
  where
    buildPathMapping :: (PathSegment, PathSegment) -> PathMapping
    buildPathMapping (from, to) = PathMapping from to

buildRules configuration = rules ++ reverseRules
  where
    rules = map buildRule configuration
    reverseRules = map reverseRule rules

alternativeTo :: [SubstitutionRule] -> Path -> Maybe Path
alternativeTo rules path =
  case matchingRules of
    [] -> Nothing
    (rule:xs) -> Just $ applyRule rule path
  where
    matchingRules = filter (flip matchesSubstitutionRule path) rules

    applyRule :: SubstitutionRule -> Path -> Path
    applyRule (SubstitutionRule pathMappings) path = foldr applyPathMapping path pathMappings

    applyPathMapping :: PathMapping -> Path -> Path
    applyPathMapping (PathMapping findStr replaceStr) = replace findStr replaceStr

matchesPathSegment :: PathSegment -> Path -> Bool
matchesPathSegment = isInfixOf

matchesPathMapping :: PathMapping -> Path -> Bool
matchesPathMapping (PathMapping from to) = matchesPathSegment from

matchesSubstitutionRule :: SubstitutionRule -> Path -> Bool
matchesSubstitutionRule (SubstitutionRule pathMappings) path = all isMatchingPathMapping pathMappings
  where
    isMatchingPathMapping = flip matchesPathMapping path

reverseRule :: SubstitutionRule -> SubstitutionRule
reverseRule (SubstitutionRule pathMappings) = SubstitutionRule $ map reversePathMapping pathMappings

reversePathMapping :: PathMapping -> PathMapping
reversePathMapping (PathMapping from to) = PathMapping to from




{- TESTS -}
main = defaultMain unitTests

unitTests = testGroup "Unit tests"
  [
    mappings
  ]

mappings = testGroup "Mappings"
  [
    fromFileToSpec
  , fromSpecToFile
  ]

configuration :: [[(PathSegment, PathSegment)]]
configuration = [
    [("app", "spec/zephyr"), (".rb", "_spec.rb")]
  , [("app/assets/javascripts", "spec/javascripts"), (".js", "_spec.js")]
  ]

fromFileToSpec = testGroup "From File to Spec"
  [
    testCase "From rb to spec" $ alternativeTo (buildRules configuration) "app/models/user.rb" @?= Just "spec/zephyr/models/user_spec.rb"
  , testCase "From js to spec" $ alternativeTo (buildRules configuration) "app/assets/javascripts/initializer.js" @?= Just "spec/javascripts/initializer_spec.js"
  , testCase "No mapping found" $ alternativeTo (buildRules configuration) "config/locales/phrase.en.yml" @?= Nothing
  ]

fromSpecToFile = testGroup "From Spec to File"
  [
    testCase "From spec to rb" $ alternativeTo (buildRules configuration) "spec/zephyr/models/user_spec.rb" @?= Just "app/models/user.rb"
  , testCase "From spec to js" $ alternativeTo (buildRules configuration) "spec/javascripts/initializer_spec.js" @?= Just "app/assets/javascripts/initializer.js"
  ]

