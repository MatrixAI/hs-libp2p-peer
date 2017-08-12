
# TODO:
Various ID construction functions that take in strings, or bytes... etc, the main idea is that they can be cast into a multihash, and if they cannot, then they are not a valid ID.

IDFromBytes

IDFromString

we can collapse this into a single smart constructor that can handle these types of operations

the smart constructor checks whether the input type is a valid multihash

the decode

-- a peer id is a base58 encoded, sha256 multihash of a public key
-- a public key is a base64 encoded string of a protobuf containing a RSA DER buffer
-- one way to represent a peer id, is the id itself and the ability to carry around the private key and the public key, that's how the javascript does it
