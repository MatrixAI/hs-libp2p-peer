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

-- a peer id is a base58 encoded, sha256 multihash of a public key
-- a public key is a base64 encoded string of a protobuf containing a RSA DER buffer
-- one way to represent a peer id, is the id itself and the ability to carry around the private key and the public key, that's how the javascript does it
