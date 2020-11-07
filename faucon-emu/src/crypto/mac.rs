//! Utilities for Falcon MAC calculation on Heavy Secure mode entry.

use std::convert::TryInto;

use byteorder::{ByteOrder, LittleEndian};

use super::{aes, xor};

fn prepare_signature_block(address: u32) -> aes::Block {
    let mut block = [0; 16];
    LittleEndian::write_u32(&mut block[..4], address);

    block
}

/// Computes a Davies Meyer MAC over the code pages of a secret region.
pub fn calculate_davies_meyer_mac(code: &[u8], mut address: u32) -> aes::Block {
    assert_eq!(code.len() % 0x100, 0);

    let mut ciphertext = [0; 16];
    for page in code.chunks(0x100) {
        // Process all blocks in a page separately.
        for block in page.chunks(0x10) {
            let cipher_block = aes::encrypt(block.try_into().unwrap(), &ciphertext);
            xor(&mut ciphertext, &cipher_block);
        }

        // Every page additionally has one last signature block to be processed.
        let sig_block = prepare_signature_block(address);
        let cipher_block = aes::encrypt(&sig_block, &ciphertext);
        xor(&mut ciphertext, &cipher_block);

        // Advance to the next code page.
        address += 0x100;
    }

    ciphertext
}
