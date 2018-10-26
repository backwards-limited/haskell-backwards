import Test.Hspec
import Data
import Lib

gwc = gridWithCoords grid

testFindWord word =
  let
    (Just result) = findWord gwc word
    string = map cell2char result
  in
    string `shouldBe` word

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "should concatenate every line with a new line" $ do
      formatGrid (gridWithCoords ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"

  describe "findWord" $ do
    it "should find words that exists on the Grid" $ do
      testFindWord "HASKELL"
      testFindWord "PERL"

    it "should not find words that do not exist on the Grid" $ do
      findWord gwc "BLAH" `shouldBe` Nothing

  describe "findWords" $ do
    it "should find all words that exist on the Grid" $ do
      let
        found = findWords gwc languages
        asString = map (map cell2char) found
      
      asString `shouldBe` languages

    it "should not find words that do not exist on the Grid" $ do
      findWords gwc ["FRENCH", "GERMAN"] `shouldBe` []
