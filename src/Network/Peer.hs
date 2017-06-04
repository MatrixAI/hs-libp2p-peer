{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Peer
  (
    PeerId,
    toPeerId,
    prettyPrint
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TEncoding
import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Char8 as BSStrictChar
import qualified Data.ByteString.Base58 as BSBase58
import qualified Data.Multihash.Digest as MHD

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.String (IsString, fromString)

import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.Secp256k1 as Secp256k1

newtype PeerId = PeerId BSStrict.ByteString
  deriving (Show, Eq, Generic, Typeable, Data)

instance IsString PeerId where
  fromString s = either error id $ toPeerId $ BSStrictChar.pack s

toPeerId :: BSStrict.ByteString -> Either String PeerId
toPeerId bs = fmap (PeerId . MHD.digest) $ MHD.decode bs

-- wait textual representation is base58 encoded
-- but it's also based on ...
-- but it is also trimmed to 6 characters (that should be dependent on who's pretty printing and their requirements)
-- so that's weird

prettyPrint :: PeerId -> T.Text
prettyPrint peerId = T.concat
  [
    "<peer.ID ",
    T.take maxChars $ filter $ TEncoding.decodeUtf8 $ base58 peerId,
    ">"
  ]
  where
    maxChars = 6
    filter t = maybe t id $ T.stripPrefix "Qm" t
    base58 (PeerId bs) = BSBase58.encodeBase58 BSBase58.bitcoinAlphabet bs

-- these functions will need to be put into a separate module with the necessary interface fulfilled
-- this could also be reimplemented as typeclasses

data Key = PubKey PubKey | PrivKey PrivKey
  deriving (Eq)

data PubKey = PubRSA RSA.PublicKey
           | PubEd25519 Ed25519.PublicKey
           | PubSecp256k1 Secp256k1.PubKey
            deriving (Eq)

data PrivKey = PrivRSA RSA.PrivateKey
            | PrivEd25519 Ed25519.SecretKey
            | PrivSecp256k1 Secp256k1.SecKey
            deriving (Eq)

toPublic :: PrivKey -> PubKey
toPublic (PrivRSA privKey) = PubRSA $ RSA.private_pub privKey
toPublic (PrivEd25519 privKey) = PubEd25519 $ Ed25519.toPublic privKey
toPublic (PrivSecp256k1 privKey) = PubSecp256k1 $ Secp256k1.derivePubKey privKey

matchesPrivKey :: PeerId -> PrivKey -> Bool
matchesPrivKey peerId privKey = matchesPubKey peerId $ toPublic privKey

matchesPubKey :: PeerId -> PubKey -> Bool
matchesPubKey peerId pubKey = undefined



{-

go lang has a key interface:

type Key interface {
  Bytes() ([]byte, error)
  Equals(Key) bool
}

type PrivKey interface {
  Key
  Sign([]byte)([]byte, error)
  GetPublic() PubKey
}

type PubKey interface {
  Key
  Verify(data []byte, sig []byte) (bool, error)
}

We need a general key type that supports being acquired as bytes

The other thing is their way of deriving the ID from a public key relies on a sum function exposed by their multihash module, which the haskell version doesn't have.

So how about it? Cryptonite can supply the underlying crypto types, however there needs to be some sort of type abstraction that can handle above interfaces.

Various ID construction functions that take in strings, or bytes... etc, the main idea is that they can be cast into a multihash, and if they cannot, then they are not a valid ID.

IDFromBytes

IDFromString

we can collapse this into a single smart constructor that can handle these types of operations

the smart constructor checks whether the input type is a valid multihash

the decode

-}

