{-|
Module      : Network.LibP2P.PeerId
Description : Multihash public key identities for LibP2P
License     : Apache-2.0
Maintainer  : quoc.ho@matrix.ai
Stability   : experimental

Provides a PeerId type, which is a SHA256 multihash of an underlying public key.
This is used as an identifier through the LibP2P system.
At the moment, we exclusively use RSA public keys, pending changes to 
the reference libp2p-crypto implementation to use a wrapped key type (multikey).
-}
module Network.LibP2P.PeerId where

import qualified Crypto.LibP2P.Serialize as Crypto

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Char8   as BSChar
import qualified Data.Multihash.Base     as MHB
import qualified Data.Multihash.Digest   as MHD

newtype PeerId = PeerId { unPeerId :: MHD.MultihashDigest } 
  deriving (Show, Eq)

prettyPrint :: PeerId -> T.Text
prettyPrint peerId = T.concat
  [
    "<peer.ID ",
    T.take maxChars 
    $ filter 
    $ TE.decodeUtf8 
    $ BSL.toStrict
    $ MHB.encode MHB.Base58
    $ BSL.fromStrict
    $ MHD.digest 
    $ unPeerId peerId,
    ">"
  ]
  where
    maxChars = 6
    filter t = maybe t id $ T.stripPrefix "Qm" t

-- Create a Peer ID from raw bytes representing the key's multihash
-- The multihash must have a SHA256 encoded digest, or we return an error
fromBytes :: BS.ByteString -> Either String PeerId
fromBytes bs = do
  mhd <- MHD.decode bs
  case MHD.algorithm mhd of
       MHD.SHA256 -> Right (PeerId mhd)
       _          -> Left "PeerId Multihashes need to be SHA256 encoded"

-- Create a Peer ID from a base encoded bytestring representing a multihash
fromBase :: MHB.BaseEncoding -> BS.ByteString -> Either String PeerId
fromBase base bs = unBase bs >>= fromBytes
  where
    unBase bs = 
      fmap BSL.toStrict 
      $ MHB.decode base 
      $ BSL.fromStrict bs

-- Convenience methods for fromBase
fromHex :: BS.ByteString -> Either String PeerId
fromHex = fromBase MHB.Base16

fromB58 :: BS.ByteString -> Either String PeerId
fromB58 = fromBase MHB.Base58

fromB64 :: BS.ByteString -> Either String PeerId
fromB64 = fromBase MHB.Base64

-- Return the byte representation of the Peer ID
-- (i.e. the SHA256 multihash of an RSA key)
toBytes :: PeerId -> BS.ByteString
toBytes pid = unPeerId pid

-- Return a base encoded byte representation of the Peer ID multhash digest
toBase :: MHB.BaseEncoding -> PeerId -> BS.ByteString
toBase base pid = MHB.encode base $ unPeerId pid

-- Convenience methods for toBase
toHex :: PeerId -> BS.ByteString
toHex = toBase MHB.Base16

toB58 :: PeerId -> BS.ByteString
toB58 = toBase MHB.Base58

toB64 :: PeerId -> BS.ByteString
toB64 = toBase MHB.Base64

-- Convert an RSA public key to a PeerID 
rsaToPeerId :: RSA.PublicKey -> PeerId
rsaToPeerId pk = PeerId mhd
  where
    Right mhd = MHD.decode digest
    digest = 
      BSL.toStrict 
      $ MHD.encode MHD.SHA256 
      $ Crypto.serialize pk
