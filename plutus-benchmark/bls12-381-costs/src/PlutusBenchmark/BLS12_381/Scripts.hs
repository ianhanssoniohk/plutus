-- editorconfig-checker-disable-file
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

{- | Approximations of the sort of computations involving BLS12-381 primitives
 that one might wish to perform on the chain.  Real on-chain code will have
 extra overhead, but these examples help to give us an idea of the sort of
 computation that can feasibly be carried out within the validation budget
 limits. -}
module PlutusBenchmark.BLS12_381.Scripts ( checkGroth16Verify_Haskell
                                         , listOfSizedByteStrings
                                         , mkGroth16VerifyScript
                                         , mkHashAndAddG1Script
                                         , mkHashAndAddG2Script
                                         , mkPairingScript
                                         , mkUncompressAndAddG1Script
                                         , mkUncompressAndAddG2Script
                                         , mkVerifyBlsSimplePolicy
                                         , checkVerifyBlsSimpleScript
                                         , mkVrfBlsPolicy
                                         , checkVrfBlsScript
                                         , mkG1VerifyPolicy
                                         , checkG1VerifyScript
                                         , mkG2VerifyPolicy
                                         , checkG2VerifyScript
                                         , mkAggregateSingleKeyG1Policy
                                         , checkAggregateSingleKeyG1Script
                                         , mkAggregateMultiKeyG2Policy
                                         , checkAggregateMultiKeyG2Script
                                         , mkSchnorrG1VerifyPolicy
                                         , checkSchnorrG1VerifyScript
                                         , mkSchnorrG2VerifyPolicy
                                         , checkSchnorrG2VerifyScript
                                        )
where
import PlutusCore (DefaultFun, DefaultUni)
import PlutusLedgerApi.V1.Bytes qualified as P (bytes, fromHex)
import PlutusTx qualified as Tx
import UntypedPlutusCore qualified as UPLC

import PlutusTx.Prelude as Tx hiding (sort, (<>))

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Semigroup ((<>))
import Data.Word (Word8)
import Hedgehog.Internal.Gen qualified as G
import Hedgehog.Internal.Range qualified as R
import System.IO.Unsafe (unsafePerformIO)

import Prelude (fromIntegral)

-- Create a list containing n bytestrings of length l.  This could be better.
{-# NOINLINE listOfSizedByteStrings #-}
listOfSizedByteStrings :: Integer -> Integer -> [ByteString]
listOfSizedByteStrings n l = unsafePerformIO . G.sample $
                             G.list (R.singleton $ fromIntegral n)
                                  (G.bytes (R.singleton $ fromIntegral l))

-- G1 generator
{-# INLINABLE g1Generator #-}
g1Generator :: BuiltinBLS12_381_G1_Element
g1Generator = Tx.bls12_381_G1_uncompress $ toBuiltin $ BS.pack
  [151, 241, 211, 167, 49, 151, 215, 148, 38, 149, 99, 140, 79, 169, 172, 15, 195, 104, 140, 79, 151, 116, 185,
   5, 161,78, 58, 63, 23, 27, 172, 88, 108, 85, 232, 63, 249, 122, 26, 239, 251, 58, 240, 10, 219, 34, 198, 187]

-- G1 generator
{-# INLINABLE g2Generator #-}
g2Generator :: BuiltinBLS12_381_G2_Element
g2Generator = Tx.bls12_381_G2_uncompress $ toBuiltin $ BS.pack
  [147, 224, 43, 96, 82, 113, 159, 96, 125, 172, 211, 160, 136, 39, 79, 101, 89, 107, 208, 208, 153, 32, 182,
   26, 181, 218, 97, 187, 220, 127, 80, 73, 51, 76, 241, 18, 19, 148, 93, 87, 229, 172, 125, 5, 93, 4, 43, 126,
   2, 74, 162, 178, 240, 143, 10, 145, 38, 8, 5, 39, 45, 197, 16, 81, 198, 228, 122, 212, 250, 64, 59, 2, 180,
   81, 11, 100, 122, 227, 209, 119, 11, 172, 3, 38, 168, 5, 187, 239, 212, 128, 86, 200, 193, 33, 189, 184]

-- | Treat string of hexidecimal bytes literally, without encoding. Useful for hashes.
bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = P.bytes . fromEither . P.fromHex
  where
    fromEither (Left _)   = traceError "bytesFromHex failed"
    fromEither (Right bs) = bs

blsSigBls12381G2XmdSha256SswuRoNul :: BuiltinByteString
blsSigBls12381G2XmdSha256SswuRoNul = toBuiltin $ C8.pack "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_"

{-# INLINABLE byteString16Null #-}
byteString16Null :: BuiltinByteString
byteString16Null = toBuiltin $ bytesFromHex "00000000000000000000000000000000"

---------------- Examples ----------------

-- Hash some bytestrings onto G1 and add them all together

{-# INLINABLE hashAndAddG1 #-}
hashAndAddG1 :: [BuiltinByteString] -> BuiltinBLS12_381_G1_Element
hashAndAddG1 [] = error ()
hashAndAddG1 (p:ps) =
    go ps (Tx.bls12_381_G1_hashToGroup p emptyByteString)
    where go [] !acc     = acc
          go (q:qs) !acc = go qs $ Tx.bls12_381_G1_add (Tx.bls12_381_G1_hashToGroup q emptyByteString) acc

mkHashAndAddG1Script :: [ByteString] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkHashAndAddG1Script l =
    let points = map toBuiltin l
    in Tx.getPlcNoAnn $ $$(Tx.compile [|| hashAndAddG1 ||]) `Tx.unsafeApplyCode` Tx.liftCodeDef points

-- Hash some bytestrings onto G2 and add them all together
{-# INLINABLE hashAndAddG2 #-}
hashAndAddG2 :: [BuiltinByteString] -> BuiltinBLS12_381_G2_Element
hashAndAddG2 [] = error ()
hashAndAddG2 (p:ps) =
    go ps (Tx.bls12_381_G2_hashToGroup p emptyByteString)
    where go [] !acc     = acc
          go (q:qs) !acc = go qs $ Tx.bls12_381_G2_add (Tx.bls12_381_G2_hashToGroup q emptyByteString) acc

mkHashAndAddG2Script :: [ByteString] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkHashAndAddG2Script l =
    let points = map toBuiltin l
    in Tx.getPlcNoAnn $ $$(Tx.compile [|| hashAndAddG2 ||]) `Tx.unsafeApplyCode` Tx.liftCodeDef points

-- Uncompress a list of compressed G1 points and add them all together
{-# INLINABLE uncompressAndAddG1 #-}
uncompressAndAddG1 :: [BuiltinByteString] -> BuiltinBLS12_381_G1_Element
uncompressAndAddG1 [] = error ()
uncompressAndAddG1 (p:ps) =
    go ps (Tx.bls12_381_G1_uncompress p)
    where go [] acc     = acc
          go (q:qs) acc = go qs $ Tx.bls12_381_G1_add (Tx.bls12_381_G1_uncompress q) acc

mkUncompressAndAddG1Script :: [ByteString] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkUncompressAndAddG1Script l =
    let ramdomPoint bs = Tx.bls12_381_G1_hashToGroup bs emptyByteString
        points = map (Tx.bls12_381_G1_compress . ramdomPoint . toBuiltin) l
    in Tx.getPlcNoAnn $ $$(Tx.compile [|| uncompressAndAddG1 ||]) `Tx.unsafeApplyCode` Tx.liftCodeDef points

-- Uncompress a list of compressed G1 points and add them all together
{-# INLINABLE uncompressAndAddG2 #-}
uncompressAndAddG2 :: [BuiltinByteString] -> BuiltinBLS12_381_G2_Element
uncompressAndAddG2 [] = error ()
uncompressAndAddG2 (p:ps) =
    go ps (Tx.bls12_381_G2_uncompress p)
    where go [] acc     = acc
          go (q:qs) acc = go qs $ Tx.bls12_381_G2_add (Tx.bls12_381_G2_uncompress q) acc

mkUncompressAndAddG2Script :: [ByteString] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkUncompressAndAddG2Script l =
    let ramdomPoint bs = Tx.bls12_381_G2_hashToGroup bs emptyByteString
        points = map (Tx.bls12_381_G2_compress . ramdomPoint . toBuiltin) l
    in Tx.getPlcNoAnn $ $$(Tx.compile [|| uncompressAndAddG2 ||]) `Tx.unsafeApplyCode` Tx.liftCodeDef points

-- Pairing operations

-- Take two points p1 and p2 in G1 and two points q1 and q2 in G2, apply the
-- Miller loop to (p1,q1) and (p2,q2), and then call finalVerify on the results.
{-# INLINABLE runPairingFunctions #-}
runPairingFunctions
    :: Tx.BuiltinBLS12_381_G1_Element
    -> Tx.BuiltinBLS12_381_G2_Element
    -> Tx.BuiltinBLS12_381_G1_Element
    -> Tx.BuiltinBLS12_381_G2_Element
    -> Bool
runPairingFunctions p1 q1 p2 q2 =
    let r1 = Tx.bls12_381_millerLoop p1 q1
        r2 = Tx.bls12_381_millerLoop p2 q2
    in Tx.bls12_381_finalVerify r1 r2

mkPairingScript
    :: BuiltinBLS12_381_G1_Element
    -> BuiltinBLS12_381_G2_Element
    -> BuiltinBLS12_381_G1_Element
    -> BuiltinBLS12_381_G2_Element
    -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkPairingScript p1 q1 p2 q2 =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| runPairingFunctions ||])
          `Tx.unsafeApplyCode` Tx.liftCodeDef p1
          `Tx.unsafeApplyCode` Tx.liftCodeDef q1
          `Tx.unsafeApplyCode` Tx.liftCodeDef p2
          `Tx.unsafeApplyCode` Tx.liftCodeDef q2


---------------- Groth16 verification ----------------

{- | An example of the on-chain computation required for verification of a Groth16
 proof.  The data here is derived from
 https://github.com/achimcc/groth16-example/blob/main/src/lib.rs -}

-- Wrappers for compressed group elements for slightly better type safety
newtype CompressedG1Element = CompressedG1Element { g1 :: BuiltinByteString }
    deriving newtype (Tx.Lift DefaultUni)

mkG1Element :: [Word8] -> CompressedG1Element
mkG1Element = CompressedG1Element . toBuiltin . BS.pack

newtype CompressedG2Element = CompressedG2Element { g2 :: BuiltinByteString }
    deriving newtype (Tx.Lift DefaultUni)

mkG2Element :: [Word8] -> CompressedG2Element
mkG2Element = CompressedG2Element . toBuiltin . BS.pack

scalar :: Integer
scalar = 0x1884d0cbcc5947434e46d19b3e904e18a8ee8d0d39ce9d315f3b00e338c8f618

-- Lots of group elements for input to the computation

alpha :: CompressedG1Element
alpha = mkG1Element [ 0xb7, 0x1d, 0xb1, 0xfa, 0x5f, 0x41, 0x36, 0x2e
                    , 0x93, 0x02, 0x5b, 0x35, 0x56, 0xd7, 0x6e, 0xad
                    , 0x12, 0x25, 0xcf, 0x59, 0x0d, 0x1c, 0xdb, 0x9e
                    , 0x38, 0x2a, 0x1f, 0xeb, 0xb7, 0x96, 0x3d, 0xcd
                    , 0x24, 0xa5, 0x1e, 0x18, 0xdf, 0x04, 0xab, 0x22
                    , 0x1b, 0xec, 0xaf, 0x29, 0x16, 0x9f, 0xaf, 0x25 ]

beta :: CompressedG2Element
beta = mkG2Element [ 0xb3, 0xa2, 0x6b, 0x0b, 0x47, 0x12, 0xe7, 0x8d
                   , 0x5d, 0x71, 0x78, 0x6d, 0x96, 0x13, 0x2a, 0x7c
                   , 0x58, 0x50, 0x23, 0xa3, 0x66, 0x32, 0xca, 0xda
                   , 0x44, 0x17, 0x1a, 0xc3, 0xf4, 0x5d, 0xb5, 0x24
                   , 0xc3, 0xf6, 0x57, 0x0c, 0x8a, 0x3f, 0x7d, 0xec
                   , 0x35, 0xae, 0x1a, 0xc3, 0x30, 0x9b, 0x05, 0xdd
                   , 0x0b, 0x30, 0x6d, 0xb4, 0xf7, 0x4f, 0xd9, 0xec
                   , 0x42, 0x1c, 0xa7, 0x0c, 0x54, 0x42, 0x5d, 0x92
                   , 0x2e, 0xac, 0x4c, 0x40, 0x3b, 0x00, 0xdb, 0x91
                   , 0x6f, 0xde, 0xdf, 0x06, 0x5b, 0xdc, 0xe0, 0x0e
                   , 0xce, 0x17, 0xb9, 0x7a, 0x4e, 0x97, 0x17, 0x3e
                   , 0x4d, 0x59, 0x89, 0x81, 0x8e, 0xdf, 0xaa, 0x4c ]

gamma :: CompressedG2Element
gamma = mkG2Element [ 0xb5, 0xac, 0xb8, 0x00, 0xcd, 0x49, 0xed, 0x8c
                    , 0xbd, 0xdb, 0xf4, 0x91, 0xa1, 0xfc, 0xf8, 0xab
                    , 0xfc, 0x93, 0xf0, 0x9d, 0x38, 0xbb, 0xb2, 0xec
                    , 0xb6, 0xb0, 0x8e, 0x23, 0xa4, 0x64, 0x2c, 0xe5
                    , 0x9c, 0x9b, 0x03, 0x86, 0x53, 0x9a, 0xc3, 0xce
                    , 0xcd, 0xfb, 0x66, 0xa9, 0xf0, 0x27, 0xfc, 0x21
                    , 0x0f, 0x25, 0x95, 0x10, 0x75, 0x64, 0x44, 0xbc
                    , 0x5e, 0xef, 0x65, 0x4f, 0x4d, 0x06, 0x12, 0xb5
                    , 0xd6, 0x37, 0x5f, 0x95, 0x26, 0xb1, 0xb9, 0x66
                    , 0xce, 0x53, 0xb8, 0xf1, 0x25, 0x94, 0xe1, 0xb3
                    , 0x99, 0xd0, 0x82, 0x31, 0xcf, 0xe6, 0xc2, 0x69
                    , 0xa4, 0x4a, 0xa8, 0xd5, 0x87, 0xf2, 0x36, 0x9d ]

delta :: CompressedG2Element
delta = mkG2Element [ 0xb3, 0xaa, 0x79, 0x7b, 0xaf, 0xa3, 0x9a, 0x48
                    , 0xf6, 0xf8, 0x7c, 0x24, 0x83, 0xc8, 0x94, 0xc2
                    , 0x81, 0xc8, 0x07, 0x82, 0x1c, 0x47, 0x30, 0x1f
                    , 0xfb, 0x75, 0x5a, 0xcf, 0xcf, 0xd2, 0x2c, 0x23
                    , 0x23, 0xce, 0xdf, 0x63, 0x49, 0xc7, 0xfe, 0xdd
                    , 0x32, 0x00, 0xa4, 0xae, 0x55, 0x86, 0x31, 0xe5
                    , 0x01, 0xd2, 0x99, 0xeb, 0x93, 0x13, 0x5c, 0x07
                    , 0xcf, 0x69, 0x4c, 0xa1, 0x18, 0xd1, 0xb3, 0x86
                    , 0x49, 0x05, 0x29, 0xc6, 0x0f, 0x57, 0x93, 0x5c
                    , 0xef, 0xa8, 0x9f, 0xca, 0xfa, 0x13, 0xa8, 0x3f
                    , 0x84, 0x20, 0x7b, 0x76, 0xfe, 0x07, 0x8d, 0xc8
                    , 0x59, 0xd4, 0x02, 0x74, 0x3d, 0x46, 0x8c, 0x15 ]

gamma_abc_1 :: CompressedG1Element
gamma_abc_1 = mkG1Element [ 0xb7, 0xf6, 0xd0, 0x6d, 0xd3, 0xe5, 0x24, 0x6e
                          , 0xf6, 0xb5, 0x1b, 0x07, 0x5c, 0x30, 0xb6, 0x8f
                          , 0xd4, 0x90, 0xfb, 0xf8, 0x5e, 0x02, 0x05, 0xf7
                          , 0x9f, 0xa0, 0x4d, 0x81, 0x13, 0x31, 0x92, 0x13
                          , 0x94, 0x63, 0xb5, 0xe8, 0xef, 0xb2, 0x2c, 0x39
                          , 0xef, 0x3d, 0xd1, 0xc5, 0x09, 0x20, 0x15, 0xb8 ]

gamma_abc_2 :: CompressedG1Element
gamma_abc_2 = mkG1Element [ 0xa2, 0xe6, 0x37, 0xdb, 0xff, 0x52, 0xa1, 0xe4
                          , 0xa8, 0xc5, 0xd9, 0x85, 0xb3, 0x41, 0x1f, 0xc5
                          , 0xfd, 0x44, 0xaf, 0x60, 0x7e, 0x42, 0x92, 0x3e
                          , 0xab, 0xb4, 0x7a, 0xd8, 0x76, 0xe1, 0xf0, 0x2b
                          , 0x5b, 0xe0, 0x34, 0xad, 0xaf, 0x73, 0x95, 0x2a
                          , 0xe8, 0xaf, 0xfe, 0xe5, 0xf5, 0x18, 0x41, 0xde ]

a :: CompressedG1Element
a = mkG1Element [ 0xa0, 0x5b, 0xe5, 0x0f, 0xab, 0x57, 0x95, 0xbb
                , 0x87, 0x84, 0x39, 0x3a, 0x50, 0x45, 0xf9, 0x87
                , 0x47, 0x17, 0x3a, 0xd2, 0x87, 0xf5, 0x5e, 0x21
                , 0x34, 0x71, 0xbd, 0x55, 0x97, 0x45, 0x55, 0x14
                , 0x52, 0x45, 0x3c, 0x4c, 0x3a, 0x39, 0xe7, 0xc8
                , 0x83, 0x10, 0x84, 0x9f, 0x3c, 0x7a, 0x1f, 0xc3 ]

b :: CompressedG2Element
b = mkG2Element [ 0xad, 0x63, 0x48, 0xb6, 0xb7, 0xb3, 0x4c, 0x86
                , 0xbf, 0x37, 0xa7, 0x48, 0xcd, 0x2d, 0x82, 0xa2
                , 0x50, 0xdf, 0xc6, 0x48, 0x46, 0x75, 0x66, 0x88
                , 0x25, 0xa1, 0x6f, 0x7d, 0xa6, 0xa0, 0x4d, 0x34
                , 0x24, 0x11, 0x3e, 0x32, 0x5c, 0xe7, 0x34, 0xec
                , 0x44, 0x95, 0x60, 0x82, 0xc0, 0xa0, 0x6e, 0x5f
                , 0x18, 0x68, 0xe1, 0xf1, 0xa6, 0xe5, 0x59, 0xb9
                , 0xfe, 0x81, 0xf1, 0xa9, 0x01, 0xf8, 0xa6, 0x34
                , 0x1b, 0x30, 0x1c, 0x45, 0xb2, 0x5d, 0x30, 0x80
                , 0xfb, 0xc5, 0x03, 0x93, 0x53, 0xd8, 0xf7, 0x1b
                , 0x55, 0x0b, 0x27, 0x4e, 0xc4, 0xc0, 0x7c, 0x70
                , 0xcd, 0x11, 0x53, 0x56, 0x2c, 0x31, 0x4c, 0x97 ]

c :: CompressedG1Element
c = mkG1Element [ 0xb5, 0x69, 0xcc, 0x49, 0x1b, 0x4d, 0xf0, 0x35
                , 0xcb, 0xf4, 0x9e, 0x95, 0x1f, 0xd4, 0xfe, 0x30
                , 0xaa, 0x82, 0x36, 0xb0, 0xe2, 0xaf, 0x68, 0xf4
                , 0xc1, 0x59, 0x2c, 0xd4, 0x0d, 0xeb, 0xeb, 0x71
                , 0x8a, 0xf3, 0x36, 0x39, 0xdb, 0x6b, 0xc1, 0xe2
                , 0xda, 0x9d, 0x98, 0xe5, 0x53, 0xe5, 0xea, 0xed ]

{-# INLINABLE groth16Verify #-}
groth16Verify
    :: BuiltinByteString  -- G1
    -> BuiltinByteString  -- G2
    -> BuiltinByteString  -- G2
    -> BuiltinByteString  -- G2
    -> BuiltinByteString  -- G1
    -> BuiltinByteString  -- G1
    -> BuiltinByteString  -- G1
    -> BuiltinByteString  -- G2
    -> BuiltinByteString  -- G1
    -> Integer
    -> Bool
groth16Verify (Tx.bls12_381_G1_uncompress -> alpha')
              (Tx.bls12_381_G2_uncompress -> beta')
              (Tx.bls12_381_G2_uncompress -> gamma')
              (Tx.bls12_381_G2_uncompress -> delta')
              (Tx.bls12_381_G1_uncompress -> abc1')
              (Tx.bls12_381_G1_uncompress -> abc2')
              (Tx.bls12_381_G1_uncompress -> a')
              (Tx.bls12_381_G2_uncompress -> b')
              (Tx.bls12_381_G1_uncompress -> c')
              s =
                  let l1 = Tx.bls12_381_millerLoop a' b'
                      l2 = Tx.bls12_381_millerLoop alpha' beta'
                      l3 = Tx.bls12_381_millerLoop c' delta'
                      p  = Tx.bls12_381_G1_add  abc1' (Tx.bls12_381_G1_scalarMul s abc2')
                      l4 = Tx.bls12_381_millerLoop p gamma'
                      y  = Tx.bls12_381_mulMlResult l2 (Tx.bls12_381_mulMlResult l3 l4)
                  in Tx.bls12_381_finalVerify l1 y

{- | Make a UPLC script applying groth16Verify to the inputs.  Passing the
 newtype inputs increases the size and CPU cost slightly, so we unwrap them
 first.  This should return `True`. -}
mkGroth16VerifyScript :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkGroth16VerifyScript =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| groth16Verify ||])
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g1 alpha)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g2 beta)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g2 gamma)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g2 delta)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g1 gamma_abc_1)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g1 gamma_abc_2)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g1 a)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g2 b)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g1 c)
           `Tx.unsafeApplyCode` Tx.liftCodeDef scalar

-- | Check that the Haskell version returns the correct result.
checkGroth16Verify_Haskell :: Bool
checkGroth16Verify_Haskell =
    groth16Verify (g1 alpha) (g2 beta) (g2 gamma) (g2 delta)
                      (g1 gamma_abc_1) (g1 gamma_abc_2) (g1 a) (g2 b) (g1 c) scalar

---------------- Simple Sign and Verify ----------------

{-# INLINABLE simpleVerifyPrivKey #-}
simpleVerifyPrivKey :: Integer
simpleVerifyPrivKey = 50166937291276222007610100461546392414157570314060957244808461481762532157524

{-# INLINABLE simpleVerifyMessage #-}
simpleVerifyMessage :: BuiltinByteString
simpleVerifyMessage  = "I am a message"

{-# INLINABLE verifyBlsSimpleScript #-}
verifyBlsSimpleScript :: Integer -> BuiltinByteString -> BuiltinBLS12_381_G1_Element -> Bool
verifyBlsSimpleScript privKey message g1Gen = do
  let
    -- calculate public key
    pubKey = Tx.bls12_381_G1_scalarMul privKey g1Gen

    -- Hash this msg to the G2
    msgToG2 = Tx.bls12_381_G2_hashToGroup message Tx.emptyByteString

    -- Create signature artifact in G2 with private key
    sigma = Tx.bls12_381_G2_scalarMul privKey msgToG2

  -- verify the msg with signature sigma with the check e(g1,sigma)=e(pub,msgToG2)
  Tx.bls12_381_finalVerify (Tx.bls12_381_millerLoop g1Gen sigma) (Tx.bls12_381_millerLoop pubKey msgToG2)
  --False

checkVerifyBlsSimpleScript :: Bool
checkVerifyBlsSimpleScript = verifyBlsSimpleScript simpleVerifyPrivKey simpleVerifyMessage g1Generator

mkVerifyBlsSimplePolicy :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkVerifyBlsSimplePolicy =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| verifyBlsSimpleScript ||])
      `Tx.unsafeApplyCode` Tx.liftCodeDef simpleVerifyPrivKey
      `Tx.unsafeApplyCode` Tx.liftCodeDef simpleVerifyMessage
      `Tx.unsafeApplyCode` Tx.liftCodeDef g1Generator

---------------- VRF ----------------

{- A basic VRF using G2 (note G1 is cheaper)
   For reference see https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-vrf-15#name-elliptic-curve-vrf-ecvrf)
   and more readable and mathematical in nature see https://eprint.iacr.org/2017/099.pdf.
-}

{-# INLINABLE vrfPrivKey #-}
vrfPrivKey :: Integer
vrfPrivKey = 50166937291276222007610100461546392414157570314060957244808461481762532157524 :: Integer

{-# INLINABLE vrfMessage #-}
vrfMessage :: BuiltinByteString
vrfMessage  = "I am a message" :: BuiltinByteString

{-# INLINABLE vrfBlsScript #-}
vrfBlsScript :: Integer -> BuiltinByteString -> BuiltinBLS12_381_G2_Element -> Bool
vrfBlsScript privKey message g2Gen = do
  let
    -- calculate public key
    pub = Tx.bls12_381_G2_scalarMul privKey g2Gen

    -- hash this msg to G2
    h = Tx.bls12_381_G2_hashToGroup message emptyByteString

    -- define first element of the proof of correct VRF
    gamma = Tx.bls12_381_G2_scalarMul privKey h

    -- for this signed hash with preimage alpha, define a ephemeral interger (for each signature take a new one)
    -- Random 32 byte int
    k = 108204667002115086588596846168569722098834602153875763359385781912495445631691 :: Integer

    -- define second element of the proof of correct VRF
    -- the paper notes that this can actually be truncated to 128 bits without loss of the 128 bits security.
    -- truncating this will allow for smaller proof sizes.
    c = sha2_256 . mconcat $ Tx.bls12_381_G2_compress <$>
        [g2Gen, h, pub, gamma, Tx.bls12_381_G2_scalarMul k g2Gen, Tx.bls12_381_G2_scalarMul k h]

    -- define the third and last element of a proof of correct VRF
    s = (k - (os2ip c) * privKey) `modulo` 52435875175126190479447740508185965837690552500527637822603658699938581184513

    -- Define the cofactor of G2
    f = 305502333931268344200999753193121504214466019254188142667664032982267604182971884026507427359259977847832272839041692990889188039904403802465579155252111 :: Integer

    -- create our VRF hash output
    beta = sha2_256 . Tx.bls12_381_G2_compress $ Tx.bls12_381_G2_scalarMul f gamma

    -- The proof of that the VRF hash of input alpha under our priv key is beta
    pi = (gamma, c, s)

    -- To verify a VRF hash given an
    --        input alpha
    --        output beta
    --        proof pi (gamma, c, s)
    --        pubkey pub
    -- do the following calculation
    u  = Tx.bls12_381_G2_add (Tx.bls12_381_G2_scalarMul (os2ip c) pub) (Tx.bls12_381_G2_scalarMul s g2Gen)
    h' = Tx.bls12_381_G2_hashToGroup message emptyByteString
    v  = Tx.bls12_381_G2_add (Tx.bls12_381_G2_scalarMul (os2ip c) gamma) (Tx.bls12_381_G2_scalarMul s h')

  -- and check
  c == (sha2_256 . mconcat $ Tx.bls12_381_G2_compress <$> [g2Gen,h',pub,gamma,u,v])

  where
    os2ip :: BuiltinByteString -> Integer
    os2ip bs
      | bs == emptyByteString = error ()
      | otherwise             = go bs
      where
        len xs = lengthOfByteString xs - 1
        intAtLastByte xs = indexByteString xs $ len xs
        stripLastByte xs = takeByteString (len xs) xs
        go xs
          | xs == emptyByteString = 0
          | otherwise             = intAtLastByte xs + 256 * go (stripLastByte xs)

checkVrfBlsScript :: Bool
checkVrfBlsScript = vrfBlsScript vrfPrivKey vrfMessage g2Generator

mkVrfBlsPolicy :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkVrfBlsPolicy =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| vrfBlsScript ||])
      `Tx.unsafeApplyCode` Tx.liftCodeDef vrfPrivKey
      `Tx.unsafeApplyCode` Tx.liftCodeDef vrfMessage
      `Tx.unsafeApplyCode` Tx.liftCodeDef g2Generator

---------------- Verify over G1 ----------------

{- BLS signature with the public key over G1. This function returns a message `msg`, a public
   key `pk`, and a signature `sig`. Verification of these test vectors should proceed as follows:
    * pk_deser = G1Decompress(pk)
    * sig_deser = G2Decompress(sig)
    * hashed_msg = G2HashToCurve(msg, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_")
    * Check that pairing(pk_deser, hashed_msg) = pairing(G1Generator, sig_deser)
-}

{-# INLINABLE g1VerifyMessage #-}
g1VerifyMessage :: BuiltinByteString
g1VerifyMessage  = toBuiltin $ bytesFromHex "3e00ef2f895f40d67f5bb8e81f09a5a12c840ec3ce9a7f3b181be188ef711a1e"

{-# INLINABLE g1VerifyPubKey #-}
g1VerifyPubKey :: BuiltinByteString
g1VerifyPubKey = toBuiltin $ bytesFromHex ("aa04a34d4db073e41505ebb84eee16c0094fde9fa22ec974" <>
                                           "adb36e5b3df5b2608639f091bff99b5f090b3608c3990173")

{-# INLINABLE g1VerifySignature #-}
g1VerifySignature :: BuiltinByteString
g1VerifySignature = toBuiltin $ bytesFromHex
                   ("808ccec5435a63ae01e10d81be2707ab55cd0dfc235dfdf9f70ad32799e42510d67c9f61d98a6578a96a76cf6f4c105d" <>
                   "09262ec1d86b06515360b290e7d52d347e48438de2ea2233f3c72a0c2221ed2da5e115367bca7a2712165032340e0b29")

{-# INLINABLE g1VerifyScript #-}
g1VerifyScript ::
     BuiltinByteString
  -> BuiltinByteString
  -> BuiltinByteString
  -> BuiltinByteString
  -> BuiltinBLS12_381_G1_Element
  -> Bool
g1VerifyScript message pubKey signature dst g1Gen = do
  let
    pkDeser = Tx.bls12_381_G1_uncompress pubKey
    sigDeser = Tx.bls12_381_G2_uncompress signature
    hashedMsg = Tx.bls12_381_G2_hashToGroup message dst

  Tx.bls12_381_finalVerify (Tx.bls12_381_millerLoop pkDeser hashedMsg) (Tx.bls12_381_millerLoop g1Gen sigDeser)

checkG1VerifyScript :: Bool
checkG1VerifyScript = g1VerifyScript g1VerifyMessage g1VerifyPubKey g1VerifySignature blsSigBls12381G2XmdSha256SswuRoNul g1Generator

mkG1VerifyPolicy :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkG1VerifyPolicy =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| g1VerifyScript ||])
      `Tx.unsafeApplyCode` Tx.liftCodeDef g1VerifyMessage
      `Tx.unsafeApplyCode` Tx.liftCodeDef g1VerifyPubKey
      `Tx.unsafeApplyCode` Tx.liftCodeDef g1VerifySignature
      `Tx.unsafeApplyCode` Tx.liftCodeDef blsSigBls12381G2XmdSha256SswuRoNul
      `Tx.unsafeApplyCode` Tx.liftCodeDef g1Generator

---------------- Verify over G2 ----------------

{- BLS signature with the public key over G2. This function returns a message `msg`, a public
   key `pk`, and a signature `sig`. Verification of these test vectors should proceed as follows:
    * pk_deser = G2Decompress(pk)
    * sig_deser = G1Decompress(sig)
    * hashed_msg = G1HashToCurve(msg, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_")
    * Check that pairing(hashed_msg, pk_deser) = pairing(sig_deser, G2Generator)
-}

{-# INLINABLE g2VerifyMessage #-}
g2VerifyMessage :: BuiltinByteString
g2VerifyMessage  = toBuiltin $ bytesFromHex "5032ec38bbc5da98ee0c6f568b872a65a08abf251deb21bb4b56e5d8821e68aa"

{-# INLINABLE g2VerifyPubKey #-}
g2VerifyPubKey :: BuiltinByteString
g2VerifyPubKey = toBuiltin $ bytesFromHex
                  ("b4953c4ba10c4d4196f90169e76faf154c260ed73fc77bb65dc3be31e0cec614a7287cda94195343676c2c57494f0e65" <>
                  "1527e6504c98408e599a4eb96f7c5a8cfb85d2fdc772f28504580084ef559b9b623bc84ce30562ed320f6b7f65245ad4")

{-# INLINABLE g2VerifySignature #-}
g2VerifySignature :: BuiltinByteString
g2VerifySignature  = toBuiltin $ bytesFromHex ("a9d4de7b0b2805fe52bccb86415ef7b8ffecb313c3c25404" <>
                                              "4dfc1bdc531d3eae999d87717822a052692140774bd7245c")

{-# INLINABLE g2VerifyScript #-}
g2VerifyScript ::
     BuiltinByteString
  -> BuiltinByteString
  -> BuiltinByteString
  -> BuiltinByteString
  -> BuiltinBLS12_381_G2_Element
  -> Bool
g2VerifyScript message pubKey signature dst g2Gen = do
  let
    pkDeser = Tx.bls12_381_G2_uncompress pubKey
    sigDeser = Tx.bls12_381_G1_uncompress signature
    hashedMsg = Tx.bls12_381_G1_hashToGroup message dst

  Tx.bls12_381_finalVerify (Tx.bls12_381_millerLoop hashedMsg pkDeser) (Tx.bls12_381_millerLoop sigDeser g2Gen)

checkG2VerifyScript :: Bool
checkG2VerifyScript =
  g2VerifyScript g2VerifyMessage g2VerifyPubKey g2VerifySignature blsSigBls12381G2XmdSha256SswuRoNul g2Generator

mkG2VerifyPolicy :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkG2VerifyPolicy =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| g2VerifyScript ||])
      `Tx.unsafeApplyCode` Tx.liftCodeDef g2VerifyMessage
      `Tx.unsafeApplyCode` Tx.liftCodeDef g2VerifyPubKey
      `Tx.unsafeApplyCode` Tx.liftCodeDef g2VerifySignature
      `Tx.unsafeApplyCode` Tx.liftCodeDef blsSigBls12381G2XmdSha256SswuRoNul
      `Tx.unsafeApplyCode` Tx.liftCodeDef g2Generator

---------------- Aggregate signature with single key and different messages over G1 ----------------

{- Aggregate BLS signature with the same key and different messages, with public key over G1. This
   function returns a list of 10 messages {`msg_1`, ..., `msg_10`}, a public key `pk`, and an
   aggregate signature `aggr_sig`. To verify the correctness of the test vectors, check the
   following:
    * hashed_msg_i = G2HashToCurve(msg_i, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_") for i in [1, 10]
    * pk_deser = G1Decompress(pk)
    * aggr_sig_deser = G2Decompress(aggr_sig)
    * aggr_msg = sum_{i\in[1,10]} hashed_msg_i
    * Check that pairing(pk_deser, aggr_msg) = pairing(G1Generator, aggr_sig_deser)
-}

{-# INLINABLE aggregateSingleKeyG1Messages #-}
aggregateSingleKeyG1Messages :: [BuiltinByteString]
aggregateSingleKeyG1Messages  = [
        toBuiltin $ bytesFromHex "2ba037cdb63cb5a7277dc5d6dc549e4e28a15c70670f0e97787c170485829264"
      , toBuiltin $ bytesFromHex "ecbf14bddeb68410f423e8849e0ce35c10d20a802bbc3d9a6ca01c386279bf01"
      , toBuiltin $ bytesFromHex "e8f75f478cb0d159db767341602fa02d3e01c3d9aacf9b686eccf1bb5ff4c8fd"
      , toBuiltin $ bytesFromHex "21473e89d50f51f9a1ced2390c72ee7e37f15728e61d1fb2c8c839495e489052"
      , toBuiltin $ bytesFromHex "8c146d00fe2e1caec31b159fc42dcd7e06865c6fa5267c6ca9c5284e651e175a"
      , toBuiltin $ bytesFromHex "362f469b6e722347de959f76533315542ffa440d37cde8862da3b3331e53b60d"
      , toBuiltin $ bytesFromHex "73baeb620e63a2e646ea148974350aa337491e5f5fc087cb429173d1eeb74f5a"
      , toBuiltin $ bytesFromHex "73acc6c3d72b59b8bf5ab58cdcf76aa001689aac938a75b1bb25d77b5382898c"
      , toBuiltin $ bytesFromHex "4e73ba04bae3a083c8a2109f15b8c4680ae4ba1c70df5b513425349a77e95d3b"
      , toBuiltin $ bytesFromHex "565825a0227d45068e61eb90aa1a4dc414c0976911a52d46b39f40c5849e5abe"
      ]

{-# INLINABLE aggregateSingleKeyG1PubKey #-}
aggregateSingleKeyG1PubKey :: BuiltinByteString
aggregateSingleKeyG1PubKey = toBuiltin $ bytesFromHex ("97c919babda8d928d771d107a69adfd85a75cee2cedc4afa" <>
                                                      "4c0a7e902f38b340ea21a701a46df825210dd6942632b46c")

{-# INLINABLE aggregateSingleKeyG1Signature #-}
aggregateSingleKeyG1Signature :: BuiltinByteString
aggregateSingleKeyG1Signature = toBuiltin $ bytesFromHex
                                 ("b425291f423235b022cdd038e1a3cbdcc73b5a4470251634" <>
                                  "abb874c7585a3a05b8ea54ceb93286edb0e9184bf9a852a1" <>
                                  "138c6dd860e4b756c63dff65c433a6c5aa06834f00ac5a1a" <>
                                  "1acf6bedc44bd4354f9d36d4f20f66318f39116428fabb88")

{-# INLINABLE aggregateSingleKeyG1Script #-}
aggregateSingleKeyG1Script ::
     [BuiltinByteString]
  -> BuiltinByteString
  -> BuiltinByteString
  -> BuiltinByteString
  -> BuiltinBLS12_381_G1_Element
  -> Bool
aggregateSingleKeyG1Script messages pubKey aggregateSignature dst g1Gen = do
  let
    hashedMsgs = Tx.map (\x -> Tx.bls12_381_G2_hashToGroup x dst) messages
    pkDeser = Tx.bls12_381_G1_uncompress pubKey
    aggrSigDeser = Tx.bls12_381_G2_uncompress aggregateSignature
    aggrMsg = foldl1 Tx.bls12_381_G2_add hashedMsgs

  Tx.bls12_381_finalVerify (Tx.bls12_381_millerLoop pkDeser aggrMsg) (Tx.bls12_381_millerLoop g1Gen aggrSigDeser)
    where
      -- PlutusTx.Foldable has no foldl1
      foldl1 :: (a -> a -> a) -> [a] -> a
      foldl1 _ []     = traceError "foldr1: empty list"
      foldl1 _ [_]    = traceError "foldr1: only one element in list"
      foldl1 f (x:xs) = Tx.foldl f x xs

checkAggregateSingleKeyG1Script :: Bool
checkAggregateSingleKeyG1Script = aggregateSingleKeyG1Script aggregateSingleKeyG1Messages aggregateSingleKeyG1PubKey
                                    aggregateSingleKeyG1Signature blsSigBls12381G2XmdSha256SswuRoNul g1Generator

mkAggregateSingleKeyG1Policy :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkAggregateSingleKeyG1Policy =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| aggregateSingleKeyG1Script ||])
      `Tx.unsafeApplyCode` Tx.liftCodeDef aggregateSingleKeyG1Messages
      `Tx.unsafeApplyCode` Tx.liftCodeDef aggregateSingleKeyG1PubKey
      `Tx.unsafeApplyCode` Tx.liftCodeDef aggregateSingleKeyG1Signature
      `Tx.unsafeApplyCode` Tx.liftCodeDef blsSigBls12381G2XmdSha256SswuRoNul
      `Tx.unsafeApplyCode` Tx.liftCodeDef g1Generator

---------------- Aggregate signature with multiple keys and single message over G1 ----------------

{- Aggregate BLS signature with different keys and same message, with public key over G2. This
   function returns a message `msg`, ten public keys `{pk_1,...,pk_10}`, and an
   aggregate signature `aggr_sig`. To verify the correctness of the test vectors, check the
   following:
    * hashed_msg = G1HashToCurve(msg, "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_")
    * pk_deser_i = G2Decompress(pk_i) for i in [1, 10]
    * ds_scalar = SHA256(pk_1 || .. || pk_10)[..16] (where [..16] represent the first 16 bytes)
    * aggr_sig_deser = G1Decompress(aggr_sig)
    * aggr_pk = sum_{i\in[1,10]} ds_scalar^i * pk_deser_i
    * Check that pairing(hashed_msg, aggr_pk) = pairing(aggr_sig_deser, G2Generator)
-}

{-# INLINABLE aggregateMultiKeyG2Message #-}
aggregateMultiKeyG2Message :: BuiltinByteString
aggregateMultiKeyG2Message  = toBuiltin $ bytesFromHex
                                "e345b7f2c017b16bb335c696bc0cc302f3db897fa25365a2ead1f149d87a97e8"

{-# INLINABLE aggregateMultiKeyG2PubKeys #-}
aggregateMultiKeyG2PubKeys :: [BuiltinByteString]
aggregateMultiKeyG2PubKeys = [
    toBuiltin $ bytesFromHex
      ("83718f20d08471565b3a6ca6ea82c1928e8730f87e2afe460b74842f2880facd8e63b8abcdcd7350fe5813a08aa0efed" <>
       "13216b10de1c56dc059c3a8910bd97ae133046ae031d2a53a44e460ab71ebda94bab64ed7478cf1a91b6d3981e32fc95")
  , toBuiltin $ bytesFromHex
      ("814f825911bd066855333b74a3cc564d512503ee29ea1ec3bd57a3c07fa5768ad27ea1ddd8047f43fbc9a4ebda897c14" <>
       "06415fefbb8838b8782aa747e2fde7b1813d0f89fad06c8971041c9427abf848503e34e3ca033ba85d50b72ffac4be4a")
  , toBuiltin $ bytesFromHex
      ("9974c70513ed5538a8e55f5ce1a0267282b9e8431e25ae566950b2d0793a44a0a3c52110f4d83d694a5296615ee68573" <>
       "098c14d255783a9b1a169d2be1baefbef914a4f830a9099f720063914cc919064d2244582bb9f302eac39c8b195cf3d2")
  , toBuiltin $ bytesFromHex
      ("894a3a01d38169a38bea13097cf904dd3ff9dceefb51e8b539725a237ae55a361758be1cdf0e21a7b8db3599adaf2305" <>
       "050f1d8450b924a4b910ff536fc2f7960cd3251c2a457b975d46f7c0f74493cc9b5e8d2fed2e489363e641cc79933d1e")
  , toBuiltin $ bytesFromHex
      ("9646da0149ed140e33a99e1ffc5fe9c97c2368ca273544024993cdcb7aa04c0be936e6d4427747e62c4caea4fe1f69e5" <>
       "162fad222e0487f5556524c9d3db74921e1c0f5893f0e26c759e3873e8fd6637e6051f70ef9a3363cf284e8eee67bcf3")
  , toBuiltin $ bytesFromHex
      ("b75743fb2f8321ac56cee19aacd7e141a3592b7230992ea84d8800d45ad71924a477f61cf9d4a2783df59dac21cd17e7" <>
       "0e4ce5d526cbe73edc4a10b78fa56a2ef34d2009f2756d2d50188031e026a6a1dadcd5e753f5e7f7276048277d3819f1")
  , toBuiltin $ bytesFromHex
      ("873c1e7d525265afa8c037d33874261a90daaa2c6ed5e46ed043ec48a28b7111d0de65800aa72448c1fdb1026ba076bd" <>
       "04193bd2d04e0de63e7a008b8417420eb4920767a1d32f6330ed25bdb4dc7726d989d6cf192db6b32728bb388195ba27")
  , toBuiltin $ bytesFromHex
      ("b993f867f9f1f84c3c5c3e5b80013055da7705491c36a80e1201a6a503d7364000c50bc27e03477646874a3074cc4e39" <>
      "0febfea78a2b4d0e40c57d6deaf9fae430a19fcce0c03f43ff8f7e788de0c7b8ce1b69b69d1d026175c8f2730777866d")
  , toBuiltin $ bytesFromHex
      ("99836a204576636f34a4663cfa7e02a05cb2d4fd1b582427d199ac3ddac6f087968d2290198aa15e04f6e7e0d070b7dd" <>
      "03607db9c2e4b17709853c30b2f6490261599408fbbc17371de74d0a2a76ff10cd8c9b55461c444bbebc82547bb40c9f")
  , toBuiltin $ bytesFromHex
      ("96f8d678f40dd83b2060e14372d0bc43a423fecac44f082afd89cb481b855885ac83fb366516dc74023cc41a0c606be2" <>
      "067ba826ea612f84c9f0e895d02bc04d6c34e201ff8c26cc22cb4c426c53f503d8948eafceb12e2f4b6ad49b4e051690")
  ]

{-# INLINABLE aggregateMultiKeyG2Signature #-}
aggregateMultiKeyG2Signature :: BuiltinByteString
aggregateMultiKeyG2Signature = toBuiltin $ bytesFromHex ("b24d876661d0d1190c796bf7eaa7e02b807ff603093b1733" <>
                                                        "6289d4de0477f6c17afb487275cb9de44325016edfeda042")

{-# INLINABLE aggregateMultiKeyG2Script #-}
aggregateMultiKeyG2Script ::
     BuiltinByteString
  -> [BuiltinByteString]
  -> BuiltinByteString
  -> BuiltinByteString
  -> BuiltinByteString
  -> BuiltinBLS12_381_G2_Element
  -> Bool
aggregateMultiKeyG2Script message pubKeys aggregateSignature bs16Null dst g2Gen = do
  let
    hashedMsg = Tx.bls12_381_G1_hashToGroup message dst
    pksDeser = Tx.map Tx.bls12_381_G2_uncompress pubKeys
    -- scalar calcuates to (142819114285630344964654001480828217341 :: Integer)
    dsScalar = byteStringToInteger (Tx.sliceByteString 0 16
               (Tx.sha2_256 (foldl1 Tx.appendByteString pubKeys)) `Tx.appendByteString` bs16Null)
    aggrSigDeser = Tx.bls12_381_G1_uncompress aggregateSignature
    aggrPk = calcAggregatedPubkeys dsScalar pksDeser

  Tx.bls12_381_finalVerify (Tx.bls12_381_millerLoop hashedMsg aggrPk) (Tx.bls12_381_millerLoop aggrSigDeser g2Gen)
    where
      -- PlutusTx.Foldable has no foldl1
      foldl1 :: (a -> a -> a) -> [a] -> a
      foldl1 _ []     = traceError "foldr1: empty list"
      foldl1 _ [_]    = traceError "foldr1: only one element in list"
      foldl1 f (x:xs) = Tx.foldl f x xs

      -- a (probably inefficient) workaround for lack of ByteString to Integer interpretation
      -- to be addressed by byteStringToInteger in https://github.com/input-output-hk/plutus/pull/4733
      byteStringToInteger  :: BuiltinByteString -> Integer
      byteStringToInteger b =
        go 0
          where len = Tx.lengthOfByteString b
                go i =
                    if i >= len
                    then 0
                    else (Tx.indexByteString b i) + 256 * (go (i + 1))

      calcAggregatedPubkeys :: Integer -> [BuiltinBLS12_381_G2_Element] -> BuiltinBLS12_381_G2_Element
      calcAggregatedPubkeys dsScalar' pksDeser' =
        go 1 (calc 0)
          where
            calc i = (dsScalar' `power` (i + 1)) `Tx.bls12_381_G2_scalarMul` (pksDeser' Tx.!! i)
            go i acc
              | i >= length pksDeser' = acc
              | otherwise = go (i + 1) (acc `Tx.bls12_381_G2_add` (calc i))

      -- TODO: Is this one better?
      -- calcAggregatedPubkeys :: Integer -> [BuiltinBLS12_381_G2_Element] -> BuiltinBLS12_381_G2_Element
      -- calcAggregatedPubkeys dsScalar' pksDeser' =
      --   Tx.foldl (\acc i -> acc `Tx.bls12_381_G2_add` (calc i)) (calc 0) loop
      --   where
      --     calc i = (dsScalar' `power` (i + 1)) `Tx.bls12_381_G2_scalarMul` (pksDeser' Tx.!! i)
      --     loop = loop' [] 1
      --     loop' a i
      --       | i == (Tx.length pksDeser' Tx.- 1) = (a Tx.++ [i])
      --       | otherwise                         = loop' (a Tx.++ [i]) (i Tx.+ 1)

      power :: Integer -> Integer -> Integer
      --power _ 0 = 1 -- this line breaks script compilation
      power x n
        | n == 0 = 1
        | n > 0 = x * power x (n - 1)

checkAggregateMultiKeyG2Script :: Bool
checkAggregateMultiKeyG2Script =
  aggregateMultiKeyG2Script aggregateMultiKeyG2Message aggregateMultiKeyG2PubKeys aggregateMultiKeyG2Signature
    byteString16Null blsSigBls12381G2XmdSha256SswuRoNul g2Generator

mkAggregateMultiKeyG2Policy :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkAggregateMultiKeyG2Policy =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| aggregateMultiKeyG2Script ||])
      `Tx.unsafeApplyCode` Tx.liftCodeDef aggregateMultiKeyG2Message
      `Tx.unsafeApplyCode` Tx.liftCodeDef aggregateMultiKeyG2PubKeys
      `Tx.unsafeApplyCode` Tx.liftCodeDef aggregateMultiKeyG2Signature
      `Tx.unsafeApplyCode` Tx.liftCodeDef byteString16Null
      `Tx.unsafeApplyCode` Tx.liftCodeDef blsSigBls12381G2XmdSha256SswuRoNul
      `Tx.unsafeApplyCode` Tx.liftCodeDef g2Generator

---------------- Schnorr signature in G1 ----------------

{- Schnorr signature in G1. This function returns a message `msg`, a public key `pk` and a
   signature `(A, r)`. To verify the signature, proceed as follows:
    * c = Sha256(A || pk || msg)[..16]
    * pk_deser = G1Decompress(pk)
    * A_deser = G1Decompress(A)
    * r_deser = IntegerFromBytes(r)
    * Check that r_deser * G1Generator = A_deser + c * pk_deser
-}

{-# INLINABLE schnorrG1VerifyMessage #-}
schnorrG1VerifyMessage :: BuiltinByteString
schnorrG1VerifyMessage  = toBuiltin $ bytesFromHex "0558db9aff738e5421439601e7f30e88b74f43b80c1d172b5d371ce0dc05c912"

{-# INLINABLE schnorrG1VerifyPubKey #-}
schnorrG1VerifyPubKey :: BuiltinByteString
schnorrG1VerifyPubKey = toBuiltin $ bytesFromHex ("b91cacee903a53383c504e9e9a39e57d1eaa6403d5d38fc9" <>
                                                 "496e5007d54ca92d106d1059f09461972aa98514d07000ae")

{-# INLINABLE schnorrG1VerifySignature #-}
schnorrG1VerifySignature :: (BuiltinByteString, BuiltinByteString)
schnorrG1VerifySignature =
                 (toBuiltin $ bytesFromHex
                    "8477e8491acc1cfbcf675acf7cf6b92e027cad7dd604a0e8205703aa2cc590066c1746f89e10d492d0230e6620c29726",
                  toBuiltin $ bytesFromHex "4e908280c0100cfe53501171ffa93528b9e2bb551d1025decb4a5b416a0aee53")

{-# INLINABLE schnorrG1VerifyScript #-}
schnorrG1VerifyScript ::
     BuiltinByteString
  -> BuiltinByteString
  -> (BuiltinByteString, BuiltinByteString)
  -> BuiltinByteString
  -> BuiltinBLS12_381_G1_Element
  -> Bool
schnorrG1VerifyScript message pubKey signature bs16Null g1Gen = do
  let
    a = Tx.fst signature
    r = Tx.snd signature
    c = byteStringToInteger (Tx.sliceByteString 0 16
      (Tx.sha2_256 (a `Tx.appendByteString` pubKey `Tx.appendByteString` message)) `Tx.appendByteString` bs16Null)
    pkDeser = Tx.bls12_381_G1_uncompress pubKey
    aDeser = Tx.bls12_381_G1_uncompress a
    rDeser = byteStringToInteger r
  (rDeser `Tx.bls12_381_G1_scalarMul` g1Gen) `Tx.bls12_381_G1_equals`
    (aDeser `Tx.bls12_381_G1_add` (c `Tx.bls12_381_G1_scalarMul` pkDeser))
    where
      -- a (probably inefficient) workaround for lack of ByteString to Integer interpretation
      -- to be addressed by byteStringToInteger in https://github.com/input-output-hk/plutus/pull/4733
      byteStringToInteger  :: BuiltinByteString -> Integer
      byteStringToInteger b =
        go 0
          where len = Tx.lengthOfByteString b
                go i =
                    if i >= len
                    then 0
                    else (Tx.indexByteString b i) + 256 * (go (i + 1))

checkSchnorrG1VerifyScript :: Bool
checkSchnorrG1VerifyScript = schnorrG1VerifyScript schnorrG1VerifyMessage schnorrG1VerifyPubKey
                             schnorrG1VerifySignature byteString16Null g1Generator

mkSchnorrG1VerifyPolicy :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkSchnorrG1VerifyPolicy =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| schnorrG1VerifyScript ||])
      `Tx.unsafeApplyCode` Tx.liftCodeDef schnorrG1VerifyMessage
      `Tx.unsafeApplyCode` Tx.liftCodeDef schnorrG1VerifyPubKey
      `Tx.unsafeApplyCode` Tx.liftCodeDef schnorrG1VerifySignature
      `Tx.unsafeApplyCode` Tx.liftCodeDef byteString16Null
      `Tx.unsafeApplyCode` Tx.liftCodeDef g1Generator

---------------- Schnorr signature in G2 ----------------

{- Schnorr signature in G2. This function returns a message `msg`, a public key `pk` and a
   signature `(A, r)`.To verify the signature, proceed as follows:
    * hash = Sha256(A || pk || msg)[..16]
    * pk_deser = G2Decompress(pk)
    * A_deser = G2Decompress(A)
    * r_deser = IntegerFromBytes(r)
    * Check that r_deser * G2Generator = A_deser + c * pk_deser
-}

{-# INLINABLE schnorrG2VerifyMessage #-}
schnorrG2VerifyMessage :: BuiltinByteString
schnorrG2VerifyMessage  = toBuiltin $ bytesFromHex "2b71175d0486006a33f14bc4e1fe711a3d4a3a3549b230013240e8f80e54372f"

{-# INLINABLE schnorrG2VerifyPubKey #-}
schnorrG2VerifyPubKey :: BuiltinByteString
schnorrG2VerifyPubKey = toBuiltin $ bytesFromHex
     ("88370a4b4ddc627613b0396498fb068f1c1ff8f2aa6b946a9fc65f930d24394ddc45042e602094f6a88d49a8a037e781" <>
     "08dce014586ff5ff5744842f382e3917d180c7eb969585748d20ae8c6e07ca786e8da7ea2c7bdef5ae1becebe4da59ad")

{-# INLINABLE schnorrG2VerifySignature #-}
schnorrG2VerifySignature :: (BuiltinByteString, BuiltinByteString)
schnorrG2VerifySignature  =
                (toBuiltin $ bytesFromHex
                  ("964851eb8823492c8720bf8c515b87043f5bab648000e63cfb6fc6fcdf6709061e0035c315cd23d239866471dea907d9" <>
                  "1568b69297dc8c4360f65d0bd399c2de19781c13bbf3a82ff1fcab8ac9f688ed96d6f2ea9a8ed057e76f0347d858ae22"),
                 toBuiltin $ bytesFromHex "2c5a22cb1e2fb77586c0c6908060b38107675a6277b8a61b1d6394a162af6718")

{-# INLINABLE schnorrG2VerifyScript #-}
schnorrG2VerifyScript ::
     BuiltinByteString
  -> BuiltinByteString
  -> (BuiltinByteString, BuiltinByteString)
  -> BuiltinByteString
  -> BuiltinBLS12_381_G2_Element
  -> Bool
schnorrG2VerifyScript message pubKey signature bs16Null g2Gen = do
  let
    a = Tx.fst signature
    r = Tx.snd signature
    c = byteStringToInteger (Tx.sliceByteString 0 16
      (Tx.sha2_256 (a `Tx.appendByteString` pubKey `Tx.appendByteString` message)) `Tx.appendByteString` bs16Null)
    pkDeser = Tx.bls12_381_G2_uncompress pubKey
    aDeser = Tx.bls12_381_G2_uncompress a
    rDeser = byteStringToInteger r
  (rDeser `Tx.bls12_381_G2_scalarMul` g2Gen) `Tx.bls12_381_G2_equals`
    (aDeser `Tx.bls12_381_G2_add` (c `Tx.bls12_381_G2_scalarMul` pkDeser))
    where
      -- a (probably inefficient) workaround for lack of ByteString to Integer interpretation
      -- to be addressed by byteStringToInteger in https://github.com/input-output-hk/plutus/pull/4733
      byteStringToInteger  :: BuiltinByteString -> Integer
      byteStringToInteger b =
        go 0
          where len = Tx.lengthOfByteString b
                go i =
                    if i >= len
                    then 0
                    else (Tx.indexByteString b i) + 256 * (go (i Tx.+ 1))

checkSchnorrG2VerifyScript :: Bool
checkSchnorrG2VerifyScript = schnorrG2VerifyScript schnorrG2VerifyMessage schnorrG2VerifyPubKey
                             schnorrG2VerifySignature byteString16Null g2Generator

mkSchnorrG2VerifyPolicy :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkSchnorrG2VerifyPolicy =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| schnorrG2VerifyScript ||])
      `Tx.unsafeApplyCode` Tx.liftCodeDef schnorrG2VerifyMessage
      `Tx.unsafeApplyCode` Tx.liftCodeDef schnorrG2VerifyPubKey
      `Tx.unsafeApplyCode` Tx.liftCodeDef schnorrG2VerifySignature
      `Tx.unsafeApplyCode` Tx.liftCodeDef byteString16Null
      `Tx.unsafeApplyCode` Tx.liftCodeDef g2Generator