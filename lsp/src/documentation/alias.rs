use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    static ref TYPE_INFO: HashMap<&'static str, (&'static str, &'static str)> =
        HashMap::from([
            ("Ctx8",
                ("(List<u8, 64>, (u64, u256))",
                 "State of a SHA256 hash engine. SHA context for streams of 8-bit values.")
            ),
            ("Pubkey", ("u256", "X-only public key.")),
            ("Message", ("u256", "256-bit message (signature hash).")),
            ("Message64",
                ("[u8; 64]",
                 "512-bit message (CMR of program that computes signature hash + signature hash).")
            ),
            ("Signature", ("[u8; 64]", "Schnorr signature.")),

            ("Scalar", ("u256", "Scalar of the secp256k1 elliptic curve.")),
            ("Fe", ("u256", "Field element (coordinate) of the secp256k1 elliptic curve.")),
            ("Ge",
                ("(u256, u256)",
                 "Group element of the secp256k1 elliptic curve in affine coordinates.")
            ),
            ("Gej",
                ("((u256, u256), u256)",
                 "Group element of the secp256k1 elliptic curve in projective / Jacobian coordinates.")
            ),
            ("Point",
                ("(u1, u256)",
                 "Compressed affine point (y-parity + x coordinate).")
            ),

            ("Height", ("u32", "Height of a Bitcoin block.")),
            ("Time", ("u32", "UNIX timestamp of a Bitcoin block.")),
            ("Distance", ("u16", "Relative distance between blocks in height.")),
            ("Duration", ("u16", "Relative distance between blocks in UNIX time.")),

            ("Lock", ("u32", "Lock time of an Elements transaction.")),
            ("Outpoint",
                ("(u256, u32)",
                 "Outpoint of an Elements transaction input (transaction ID + vout).")
            ),
            ("Confidential1", ("(u1, u256)", "Pedersen commitment to a confidential value.")),
            ("ExplicitAsset", ("u256", "Explicit Elements asset ID.")),
            ("Asset1", ("Either<(u1, u256), u256>", "Elements asset (confidential or explicit).")),
            ("ExplicitAmount", ("u64", "Explicit amount of an Elements asset.")),
            ("Amount1", ("Either<(u1, u256), u64>", "Amount of an Elements asset (confidential or explicit).")),
            ("ExplicitNonce", ("u256", "Explicit 256-bit nonce.")),
            ("Nonce", ("Either<(u1, u256), u256>", "Nonce (confidential or explicit).")),
            ("TokenAmount1", ("Either<(u1, u256), u64>", "Amount of an Elements token (confidential or explicit).")),
        ]);
}

pub fn type_info(name: &str) -> Option<(&'static str, &'static str)> {
    TYPE_INFO.get(name).copied()
}
