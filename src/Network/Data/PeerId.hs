{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Data.PeerId where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Lazy.Encoding as TLazyEncoding
import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Char8 as BSStrictChar
import qualified Data.Multihash.Base as MHB
import qualified Data.Multihash.Digest as MHD

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.String (IsString, fromString)

--newtype PeerId = PeerId BSStrict.ByteString
data PeerId = PeerId
  {
    --multihash of the peer id
    peerHash :: MHD.MultihashDigest
    --secretKey :: PrivKey a b
    --publicKey :: PubKey a b
  }
  deriving (Show, Eq)

instance IsString PeerId where
  fromString s = either error id $ fromBytes $ BSStrictChar.pack s

-- Create a Peer ID from raw bytes representing the key's multihash
fromBytes :: BSStrict.ByteString -> Either String PeerId
fromBytes bs = fmap PeerId $ MHD.decode bs

-- Create a Peer ID from a multihash base encoded string
fromBaseEncodedString :: MHB.BaseEncoding -> T.Text -> Either String PeerId
fromBaseEncodedString base s = do
  rawhash <- MHB.decode base $ TLazyEncoding.encodeUtf8 $TLazy.fromStrict s
  mhd <- fromBytes $ BSLazy.toStrict $ rawhash 
  return mhd

-- Convenience methods for fromBaseEncodedString
fromHexString :: T.Text -> Either String PeerId
fromHexString = fromBaseEncodedString MHB.Base16

-- Convenience methods for fromBaseEncodedString
fromB58String :: T.Text -> Either String PeerId
fromB58String = fromBaseEncodedString MHB.Base58

-- Convenience methods for fromBaseEncodedString
fromB64String :: T.Text -> Either String PeerId
fromB64String = fromBaseEncodedString MHB.Base64


-- prettyPrint :: PeerId -> T.Text
-- prettyPrint peerId = T.concat
--   [
--     "<peer.ID ",
--     T.take maxChars $ filter $ TEncoding.decodeUtf8 $ peerId,
--     ">"
--   ]
--   where
--     maxChars = 6
--     filter t = maybe t id $ T.stripPrefix "Qm" t
--     base58 (PeerId bs) = MHB.encode MHB.Base58 bs

-- TODO: Until the issues with haskell-libp2p-crypto are resolved,
-- leave key resolution out of representations of a PeerId,
-- and only get PeerIds from fromBytes

-- keyToPeerId :: (PubKey a) => a -> PeerId
-- keyToPeerId pubkey = PeerId . MHD.digest $ MHD.encode SHA256 $ toBytes pubkey

-- matchesPrivKey :: (PrivKey a b) => PeerId -> b -> Bool
-- matchesPrivKey peerId privKey = matchesPubKey peerId $ toPublic privKey

-- matchesPubKey :: (PrivKey a b) => PeerId -> a -> Bool
-- matchesPubKey peerId pubKey = peerId == keyToPeerId pubKey 
