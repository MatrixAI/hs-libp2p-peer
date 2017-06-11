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
-- this should also be reimplemented as typeclasses


-- pubkey to bytestring
-- then multihash sum function (bytes, SHA2_256, -1)
-- the hash is the peerid
keyToPeerId :: PubKey -> PeerId
keyToPeerId = undefined

matchesPrivKey :: PeerId -> PrivKey -> Bool
matchesPrivKey peerId privKey = matchesPubKey peerId $ toPublic privKey

matchesPubKey :: PeerId -> PubKey -> Bool
matchesPubKey peerId pubKey = undefined

-- sum takes bytes, a code, a length
-- and returns a multihash
-- sum obtains the cryptographic sum of a given buffer
-- the length parameter indicates the length of the resulting digest
-- and a negative value uses the default length values for the selected hash function
-- first we check length to see if it is valid, sounds like you can just give it a maybe int to represent whether you want a custom length
-- oh wait, if it is -1, and the hash doesn't have a default length then you are screwed, and return an error
-- ok... but then we need to consume the either type into a bool
-- but it's a separate kind of error
-- then we switch on the different hash codes that was entered
-- this is all part of some sum function in multihash which doesn't exist here? that's weird
-- the code parameter is equivalent is to: HashAlgorithm which contains SHA1, SHA256, SHA512, SHA3, BLAKE2B, BLAKE2S, there's a function called fromCode that takes an int and returns the hash algo
-- the go impl is passing the actual code for the algo
-- 


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

-- a peer id is a sha256 multihash of a public key
-- a public key is a base64 encoded string of a protobuf containing a RSA DER buffer
-- one way to represent a peer id, is the id itself and the ability to carry around the private key and the public key, that's how the javascript does it
