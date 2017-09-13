{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
import           Network.LibP2P.PeerId

import qualified Crypto.LibP2P.Key          as Crypto
import qualified Crypto.LibP2P.Parse        as Crypto
import qualified Crypto.LibP2P.PrivKey      as Crypto
import qualified Crypto.LibP2P.PubKey       as Crypto
import qualified Crypto.LibP2P.Serialize    as Crypto
import qualified Crypto.PubKey.RSA          as RSA

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteArray             as BA
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSChar
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLChar
import qualified Data.Multihash.Base        as MHB
import qualified Data.Multihash.Digest      as MHD

import qualified Test.Tasty                 as Tasty
import qualified Test.Tasty.HUnit           as Tasty
import qualified Test.Tasty.QuickCheck      as Tasty

import qualified Test.QuickCheck            as QC
import qualified Test.QuickCheck.Arbitrary  as QC
import qualified Test.QuickCheck.Gen        as QC
import qualified Test.QuickCheck.Monadic    as QC

import           Control.Monad              (liftM2, replicateM)
import           Crypto.Random.Types        (MonadRandom (..))
import           Data.Word                  (Word8)
import           GHC.Generics               (Generic)


data RSAKeyPeerIdPair = RSAKeyPeerIdPair {
    base58PeerId        :: String,
    base64RSAPrivateKey :: String
} deriving (Generic, Show)


instance Aeson.FromJSON RSAKeyPeerIdPair
instance Aeson.ToJSON RSAKeyPeerIdPair


-- Adds the ability for quickcheck generators to supply random bytes
instance MonadRandom QC.Gen where
  getRandomBytes n = do
    words <- replicateM n $ (QC.chooseAny :: QC.Gen Word8)
    return $ BA.pack words


instance QC.Arbitrary MHB.BaseEncoding where
  arbitrary = QC.arbitraryBoundedEnum


instance QC.Arbitrary RSA.PublicKey where
  arbitrary = do
    (pk, sk) <- RSA.generate 128 65537
    return pk


instance QC.Arbitrary RSA.PrivateKey where
  arbitrary = do
    (pk, sk) <- RSA.generate 128 65537
    return sk


prop_encodeEquals :: MHB.BaseEncoding -> RSA.PublicKey -> Bool
prop_encodeEquals b pk = pid == pid2
  where
    pid = rsaToPeerId pk
    Right pid2 = fromBase b $ toBase b pid


prop_matchPublicKey :: RSA.PublicKey -> Bool
prop_matchPublicKey pk = matchesRSAPublicKey pk pid
  where
    pid = rsaToPeerId pk


prop_matchPrivateKey :: RSA.PrivateKey -> Bool
prop_matchPrivateKey sk = matchesRSAPrivateKey sk pid
  where
    pid = rsaToPeerId $ Crypto.toPublic sk


quickCheckTests :: Tasty.TestTree
quickCheckTests =
  Tasty.testGroup "QuickCheck Tests"
  $ [ Tasty.testProperty "Test that a peerId is the same after \
                      \encoding and decoding"
        $ prop_encodeEquals

    , Tasty.testProperty "Tests that generated PeerId matches PublicKey"
        $ prop_matchPublicKey

    , Tasty.testProperty "Tests that generated PeerId matches PrivateKey"
        $ prop_matchPrivateKey ]


correctRSAEncoding :: RSAKeyPeerIdPair -> Tasty.TestTree
correctRSAEncoding kp
  = Tasty.testCase "Test that PeerIds from RSA match the sample key encodings"
    $ Tasty.assertEqual "" pid pid2
    where
      peerId = base58PeerId kp
      skString = base64RSAPrivateKey kp
      Right skBytes = MHB.decode MHB.Base64 $ BSLChar.pack skString
      Right (Crypto.RSAPriv sk) = Crypto.parseKey $ BSL.toStrict skBytes
      Right pid = fromB58 $ BSChar.pack peerId
      pid2 = rsaToPeerId $ Crypto.toPublic sk


unitRSATests :: [RSAKeyPeerIdPair] -> Tasty.TestTree
unitRSATests kp =
  Tasty.testGroup "Unit Tests" testKeyPairs

  where tests = [correctRSAEncoding]
        testKeyPairs = liftM2 ($) tests kp


main :: IO ()
main = do
  file <- BS.readFile "test/RSATestCases.json"
  let Just rsakp = (Aeson.decodeStrict file :: Maybe [RSAKeyPeerIdPair])
  let runTests = Tasty.testGroup "All Tests"
                   $ [ quickCheckTests
                     , unitRSATests rsakp ]
  Tasty.defaultMain runTests

