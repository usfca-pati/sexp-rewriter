import Data.Map.Strict ()
import qualified Data.Map.Strict as M
import Test.Hspec
import Syntax
import Rewriting
import Parser
import Data.Either (isLeft)

unifySpec = describe "unify" $ do
  it "should just check equality when there are no holes" $ do
    unify M.empty (Atom "a") (Atom "b") `shouldBe` Nothing
    unify M.empty (Atom "a") (Atom "a") `shouldBe` Just M.empty
  it "should update the map for top-level variables" $ do
    unify M.empty (Atom "a") (Hole (HoleVar 1)) `shouldBe`
      Just (M.fromList [(HoleVar 1, Atom "a")])
    unify (M.fromList [(HoleVar 1, Atom "a")]) (Atom "a") (Hole (HoleVar 1)) `shouldBe`
      Just (M.fromList [(HoleVar 1, Atom "a")])
    unify M.empty (App [Atom "a", Atom "b"]) (Hole (HoleVar 1)) `shouldBe`
      Just (M.fromList [(HoleVar 1, (App [Atom "a", Atom "b"]))])
  it "should fail if a variable is assigned to 2 values" $ do
    unify (M.fromList [(HoleVar 1, Atom "a")]) (Atom "b") (Hole (HoleVar 1)) `shouldBe`
      Nothing
  it "should work recursively" $ do
    unify M.empty (App [Atom "a", Atom "b"]) (App [Atom "a", Hole (HoleVar 1)]) `shouldBe`
      Just (M.fromList [(HoleVar 1, Atom "b")])
    unify (M.fromList [(HoleVar 1, Atom "a")]) (App [Atom "a", Atom "b"]) (App [Atom "a", Hole (HoleVar 1)]) `shouldBe` Nothing
    unify M.empty (App [Atom "a", Atom "b"]) (App [Hole (HoleVar 1), Hole (HoleVar 1)]) `shouldBe` Nothing

parseS = parseSExp ""
parseR = parseRule ""

-- | A helper function to test parser failures
shouldFail s = s `shouldSatisfy` isLeft

parseSpec = describe "parseSexp" $ do
  it "should parse atoms" $ do
    parseS "x" `shouldBe` Right (Atom "x")
    parseS "4#<->" `shouldBe` Right (Atom "4#<->")
    parseS "x$y" `shouldBe` Right (Atom "x$y")
  it "should not accept $" $ do
    shouldFail $ parseS "$"
    shouldFail $ parseS "$x"
    shouldFail $ parseS "$1"

a ~> b = Rule a b

parseRuleSpec = describe "parseRule" $ do
  it "should parse atoms" $ do
    parseR "x -> x" `shouldBe` Right (Atom "x" ~> Atom "x")
    parseR "4#<-> -> x" `shouldBe` Right (Atom "4#<->" ~> Atom "x")
    shouldFail $ parseS "x$y -> x$y"
  it "should parse holes" $ do
    parseR "$1 -> $1" `shouldBe` Right (Hole (HoleVar 1) ~> Hole (HoleVar 1))
    parseR "$-1 -> $-1" `shouldBe` Right (Hole (HoleVar $ -1) ~> Hole (HoleVar $ -1))
  it "should not accept other things that begin with $" $ do
    shouldFail $ parseR "$"
    shouldFail $ parseR "$x"

main :: IO ()
main = hspec $ do
  unifySpec
  parseSpec
  parseRuleSpec
