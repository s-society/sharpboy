module Cpu

open Mmu
open Register
open Instruction


opcode.[0x00] <- (fun () -> 

opcode.[0x01] <- (fun () -> 

opcode.[0x02] <- (fun () -> 

opcode.[0x03] <- (fun () -> 

opcode.[0x04] <- (fun () -> 

opcode.[0x05] <- (fun () -> 

opcode.[0x06] <- (fun () -> 

opcode.[0x07] <- (fun () -> 

opcode.[0x08] <- (fun () -> 

opcode.[0x09] <- (fun () -> 

opcode.[0x0A] <- (fun () -> 

opcode.[0x0B] <- (fun () -> 

opcode.[0x0C] <- (fun () -> 

opcode.[0x0D] <- (fun () -> 

opcode.[0x0E] <- (fun () -> 

opcode.[0x0F] <- (fun () -> 

opcode.[0x10] <- (fun () -> 

opcode.[0x11] <- (fun () -> 

opcode.[0x12] <- (fun () -> 

opcode.[0x13] <- (fun () -> 

opcode.[0x14] <- (fun () -> 

opcode.[0x15] <- (fun () -> 

opcode.[0x16] <- (fun () -> 

opcode.[0x17] <- (fun () -> 

opcode.[0x18] <- (fun () -> 

opcode.[0x19] <- (fun () -> 

opcode.[0x1A] <- (fun () -> 

opcode.[0x1B] <- (fun () -> 

opcode.[0x1C] <- (fun () -> 

opcode.[0x1D] <- (fun () -> 

opcode.[0x1E] <- (fun () -> 

opcode.[0x1F] <- (fun () -> 

opcode.[0x20] <- (fun () -> 

opcode.[0x21] <- (fun () -> 

opcode.[0x22] <- (fun () -> 

opcode.[0x23] <- (fun () -> 

opcode.[0x24] <- (fun () -> 

opcode.[0x25] <- (fun () -> 

opcode.[0x26] <- (fun () -> 

opcode.[0x27] <- (fun () -> 

opcode.[0x28] <- (fun () -> 

opcode.[0x29] <- (fun () -> 

opcode.[0x2A] <- (fun () -> 

opcode.[0x2B] <- (fun () -> 

opcode.[0x2C] <- (fun () -> 

opcode.[0x2D] <- (fun () -> 

opcode.[0x2E] <- (fun () -> 

opcode.[0x2F] <- (fun () -> 

opcode.[0x30] <- (fun () -> 

opcode.[0x31] <- (fun () -> 

opcode.[0x32] <- (fun () -> 

opcode.[0x33] <- (fun () -> 

opcode.[0x34] <- (fun () -> 

opcode.[0x35] <- (fun () -> 

opcode.[0x36] <- (fun () -> 

opcode.[0x37] <- (fun () -> 

opcode.[0x38] <- (fun () -> 

opcode.[0x39] <- (fun () -> 

opcode.[0x3A] <- (fun () -> 

opcode.[0x3B] <- (fun () -> 

opcode.[0x3C] <- (fun () -> 

opcode.[0x3D] <- (fun () -> 

opcode.[0x3E] <- (fun () -> 

opcode.[0x3F] <- (fun () -> 

opcode.[0x40] <- (fun () -> 

opcode.[0x41] <- (fun () -> 

opcode.[0x42] <- (fun () -> 

opcode.[0x43] <- (fun () -> 

opcode.[0x44] <- (fun () -> 

opcode.[0x45] <- (fun () -> 

opcode.[0x46] <- (fun () -> 

opcode.[0x47] <- (fun () -> 

opcode.[0x48] <- (fun () -> 

opcode.[0x49] <- (fun () -> 

opcode.[0x4A] <- (fun () -> 

opcode.[0x4B] <- (fun () -> 

opcode.[0x4C] <- (fun () -> 

opcode.[0x4D] <- (fun () -> 

opcode.[0x4E] <- (fun () -> 

opcode.[0x4F] <- (fun () -> 

opcode.[0x50] <- (fun () -> 

opcode.[0x51] <- (fun () -> 

opcode.[0x52] <- (fun () -> 

opcode.[0x53] <- (fun () -> 

opcode.[0x54] <- (fun () -> 

opcode.[0x55] <- (fun () -> 

opcode.[0x56] <- (fun () -> 

opcode.[0x57] <- (fun () -> 

opcode.[0x58] <- (fun () -> 

opcode.[0x59] <- (fun () -> 

opcode.[0x5A] <- (fun () -> 

opcode.[0x5B] <- (fun () -> 

opcode.[0x5C] <- (fun () -> 

opcode.[0x5D] <- (fun () -> 

opcode.[0x5E] <- (fun () -> 

opcode.[0x5F] <- (fun () -> 

opcode.[0x60] <- (fun () -> 

opcode.[0x61] <- (fun () -> 

opcode.[0x62] <- (fun () -> 

opcode.[0x63] <- (fun () -> 

opcode.[0x64] <- (fun () -> 

opcode.[0x65] <- (fun () -> 

opcode.[0x66] <- (fun () -> 

opcode.[0x67] <- (fun () -> 

opcode.[0x68] <- (fun () -> 

opcode.[0x69] <- (fun () -> 

opcode.[0x6A] <- (fun () -> 

opcode.[0x6B] <- (fun () -> 

opcode.[0x6C] <- (fun () -> 

opcode.[0x6D] <- (fun () -> 

opcode.[0x6E] <- (fun () -> 

opcode.[0x6F] <- (fun () -> 

opcode.[0x70] <- (fun () -> 

opcode.[0x71] <- (fun () -> 

opcode.[0x72] <- (fun () -> 

opcode.[0x73] <- (fun () -> 

opcode.[0x74] <- (fun () -> 

opcode.[0x75] <- (fun () -> 

opcode.[0x76] <- (fun () -> 

opcode.[0x77] <- (fun () -> 

opcode.[0x78] <- (fun () -> 

opcode.[0x79] <- (fun () -> 

opcode.[0x7A] <- (fun () -> 

opcode.[0x7B] <- (fun () -> 

opcode.[0x7C] <- (fun () -> 

opcode.[0x7D] <- (fun () -> 

opcode.[0x7E] <- (fun () -> 

opcode.[0x7F] <- (fun () -> 

opcode.[0x80] <- (fun () -> 

opcode.[0x81] <- (fun () -> 

opcode.[0x82] <- (fun () -> 

opcode.[0x83] <- (fun () -> 

opcode.[0x84] <- (fun () -> 

opcode.[0x85] <- (fun () -> 

opcode.[0x86] <- (fun () -> 

opcode.[0x87] <- (fun () -> 

opcode.[0x88] <- (fun () -> 

opcode.[0x89] <- (fun () -> 

opcode.[0x8A] <- (fun () -> 

opcode.[0x8B] <- (fun () -> 

opcode.[0x8C] <- (fun () -> 

opcode.[0x8D] <- (fun () -> 

opcode.[0x8E] <- (fun () -> 

opcode.[0x8F] <- (fun () -> 

opcode.[0x90] <- (fun () -> 

opcode.[0x91] <- (fun () -> 

opcode.[0x92] <- (fun () -> 

opcode.[0x93] <- (fun () -> 

opcode.[0x94] <- (fun () -> 

opcode.[0x95] <- (fun () -> 

opcode.[0x96] <- (fun () -> 

opcode.[0x97] <- (fun () -> 

opcode.[0x98] <- (fun () -> 

opcode.[0x99] <- (fun () -> 

opcode.[0x9A] <- (fun () -> 

opcode.[0x9B] <- (fun () -> 

opcode.[0x9C] <- (fun () -> 

opcode.[0x9D] <- (fun () -> 

opcode.[0x9E] <- (fun () -> 

opcode.[0x9F] <- (fun () -> 

opcode.[0xA0] <- (fun () -> 

opcode.[0xA1] <- (fun () -> 

opcode.[0xA2] <- (fun () -> 

opcode.[0xA3] <- (fun () -> 

opcode.[0xA4] <- (fun () -> 

opcode.[0xA5] <- (fun () -> 

opcode.[0xA6] <- (fun () -> 

opcode.[0xA7] <- (fun () -> 

opcode.[0xA8] <- (fun () -> 

opcode.[0xA9] <- (fun () -> 

opcode.[0xAA] <- (fun () -> 

opcode.[0xAB] <- (fun () -> 

opcode.[0xAC] <- (fun () -> 

opcode.[0xAD] <- (fun () -> 

opcode.[0xAE] <- (fun () -> 

opcode.[0xAF] <- (fun () -> 

opcode.[0xB0] <- (fun () -> 

opcode.[0xB1] <- (fun () -> 

opcode.[0xB2] <- (fun () -> 

opcode.[0xB3] <- (fun () -> 

opcode.[0xB4] <- (fun () -> 

opcode.[0xB5] <- (fun () -> 

opcode.[0xB6] <- (fun () -> 

opcode.[0xB7] <- (fun () -> 

opcode.[0xB8] <- (fun () -> 

opcode.[0xB9] <- (fun () -> 

opcode.[0xBA] <- (fun () -> 

opcode.[0xBB] <- (fun () -> 

opcode.[0xBC] <- (fun () -> 

opcode.[0xBD] <- (fun () -> 

opcode.[0xBE] <- (fun () -> 

opcode.[0xBF] <- (fun () -> 

opcode.[0xC0] <- (fun () -> 

opcode.[0xC1] <- (fun () -> 

opcode.[0xC2] <- (fun () -> 

opcode.[0xC3] <- (fun () -> 

opcode.[0xC4] <- (fun () -> 

opcode.[0xC5] <- (fun () -> 

opcode.[0xC6] <- (fun () -> 

opcode.[0xC7] <- (fun () -> 

opcode.[0xC8] <- (fun () -> 

opcode.[0xC9] <- (fun () -> 

opcode.[0xCA] <- (fun () -> 

opcode.[0xCB] <- (fun () -> 

opcode.[0xCC] <- (fun () -> 

opcode.[0xCD] <- (fun () -> 

opcode.[0xCE] <- (fun () -> 

opcode.[0xCF] <- (fun () -> 

opcode.[0xD0] <- (fun () -> 

opcode.[0xD1] <- (fun () -> 

opcode.[0xD2] <- (fun () -> 

opcode.[0xD3] <- (fun () -> 

opcode.[0xD4] <- (fun () -> 

opcode.[0xD5] <- (fun () -> 

opcode.[0xD6] <- (fun () -> 

opcode.[0xD7] <- (fun () -> 

opcode.[0xD8] <- (fun () -> 

opcode.[0xD9] <- (fun () -> 

opcode.[0xDA] <- (fun () -> 

opcode.[0xDB] <- (fun () -> 

opcode.[0xDC] <- (fun () -> 

opcode.[0xDD] <- (fun () -> 

opcode.[0xDE] <- (fun () -> 

opcode.[0xDF] <- (fun () -> 

opcode.[0xE0] <- (fun () -> 

opcode.[0xE1] <- (fun () -> 

opcode.[0xE2] <- (fun () -> 

opcode.[0xE3] <- (fun () -> 

opcode.[0xE4] <- (fun () -> 

opcode.[0xE5] <- (fun () -> 

opcode.[0xE6] <- (fun () -> 

opcode.[0xE7] <- (fun () -> 

opcode.[0xE8] <- (fun () -> 

opcode.[0xE9] <- (fun () -> 

opcode.[0xEA] <- (fun () -> 

opcode.[0xEB] <- (fun () -> 

opcode.[0xEC] <- (fun () -> 

opcode.[0xED] <- (fun () -> 

opcode.[0xEE] <- (fun () -> 

opcode.[0xEF] <- (fun () -> 

opcode.[0xF0] <- (fun () -> 

opcode.[0xF1] <- (fun () -> 

opcode.[0xF2] <- (fun () -> 

opcode.[0xF3] <- (fun () -> 

opcode.[0xF4] <- (fun () -> 

opcode.[0xF5] <- (fun () -> 

opcode.[0xF6] <- (fun () -> 

opcode.[0xF7] <- (fun () -> 

opcode.[0xF8] <- (fun () -> 

opcode.[0xF9] <- (fun () -> 

opcode.[0xFA] <- (fun () -> 

opcode.[0xFB] <- (fun () -> 

opcode.[0xFC] <- (fun () -> 

opcode.[0xFD] <- (fun () -> 

opcode.[0xFE] <- (fun () -> 

opcode.[0xFF] <- (fun () -> 


CBopcode.[0x00] <- (fun () -> 

CBopcode.[0x01] <- (fun () -> 

CBopcode.[0x02] <- (fun () -> 

CBopcode.[0x03] <- (fun () -> 

CBopcode.[0x04] <- (fun () -> 

CBopcode.[0x05] <- (fun () -> 

CBopcode.[0x06] <- (fun () -> 

CBopcode.[0x07] <- (fun () -> 

CBopcode.[0x08] <- (fun () -> 

CBopcode.[0x09] <- (fun () -> 

CBopcode.[0x0A] <- (fun () -> 

CBopcode.[0x0B] <- (fun () -> 

CBopcode.[0x0C] <- (fun () -> 

CBopcode.[0x0D] <- (fun () -> 

CBopcode.[0x0E] <- (fun () -> 

CBopcode.[0x0F] <- (fun () -> 

CBopcode.[0x10] <- (fun () -> 

CBopcode.[0x11] <- (fun () -> 

CBopcode.[0x12] <- (fun () -> 

CBopcode.[0x13] <- (fun () -> 

CBopcode.[0x14] <- (fun () -> 

CBopcode.[0x15] <- (fun () -> 

CBopcode.[0x16] <- (fun () -> 

CBopcode.[0x17] <- (fun () -> 

CBopcode.[0x18] <- (fun () -> 

CBopcode.[0x19] <- (fun () -> 

CBopcode.[0x1A] <- (fun () -> 

CBopcode.[0x1B] <- (fun () -> 

CBopcode.[0x1C] <- (fun () -> 

CBopcode.[0x1D] <- (fun () -> 

CBopcode.[0x1E] <- (fun () -> 

CBopcode.[0x1F] <- (fun () -> 

CBopcode.[0x20] <- (fun () -> 

CBopcode.[0x21] <- (fun () -> 

CBopcode.[0x22] <- (fun () -> 

CBopcode.[0x23] <- (fun () -> 

CBopcode.[0x24] <- (fun () -> 

CBopcode.[0x25] <- (fun () -> 

CBopcode.[0x26] <- (fun () -> 

CBopcode.[0x27] <- (fun () -> 

CBopcode.[0x28] <- (fun () -> 

CBopcode.[0x29] <- (fun () -> 

CBopcode.[0x2A] <- (fun () -> 

CBopcode.[0x2B] <- (fun () -> 

CBopcode.[0x2C] <- (fun () -> 

CBopcode.[0x2D] <- (fun () -> 

CBopcode.[0x2E] <- (fun () -> 

CBopcode.[0x2F] <- (fun () -> 

CBopcode.[0x30] <- (fun () -> 

CBopcode.[0x31] <- (fun () -> 

CBopcode.[0x32] <- (fun () -> 

CBopcode.[0x33] <- (fun () -> 

CBopcode.[0x34] <- (fun () -> 

CBopcode.[0x35] <- (fun () -> 

CBopcode.[0x36] <- (fun () -> 

CBopcode.[0x37] <- (fun () -> 

CBopcode.[0x38] <- (fun () -> 

CBopcode.[0x39] <- (fun () -> 

CBopcode.[0x3A] <- (fun () -> 

CBopcode.[0x3B] <- (fun () -> 

CBopcode.[0x3C] <- (fun () -> 

CBopcode.[0x3D] <- (fun () -> 

CBopcode.[0x3E] <- (fun () -> 

CBopcode.[0x3F] <- (fun () -> 

CBopcode.[0x40] <- (fun () -> 

CBopcode.[0x41] <- (fun () -> 

CBopcode.[0x42] <- (fun () -> 

CBopcode.[0x43] <- (fun () -> 

CBopcode.[0x44] <- (fun () -> 

CBopcode.[0x45] <- (fun () -> 

CBopcode.[0x46] <- (fun () -> 

CBopcode.[0x47] <- (fun () -> 

CBopcode.[0x48] <- (fun () -> 

CBopcode.[0x49] <- (fun () -> 

CBopcode.[0x4A] <- (fun () -> 

CBopcode.[0x4B] <- (fun () -> 

CBopcode.[0x4C] <- (fun () -> 

CBopcode.[0x4D] <- (fun () -> 

CBopcode.[0x4E] <- (fun () -> 

CBopcode.[0x4F] <- (fun () -> 

CBopcode.[0x50] <- (fun () -> 

CBopcode.[0x51] <- (fun () -> 

CBopcode.[0x52] <- (fun () -> 

CBopcode.[0x53] <- (fun () -> 

CBopcode.[0x54] <- (fun () -> 

CBopcode.[0x55] <- (fun () -> 

CBopcode.[0x56] <- (fun () -> 

CBopcode.[0x57] <- (fun () -> 

CBopcode.[0x58] <- (fun () -> 

CBopcode.[0x59] <- (fun () -> 

CBopcode.[0x5A] <- (fun () -> 

CBopcode.[0x5B] <- (fun () -> 

CBopcode.[0x5C] <- (fun () -> 

CBopcode.[0x5D] <- (fun () -> 

CBopcode.[0x5E] <- (fun () -> 

CBopcode.[0x5F] <- (fun () -> 

CBopcode.[0x60] <- (fun () -> 

CBopcode.[0x61] <- (fun () -> 

CBopcode.[0x62] <- (fun () -> 

CBopcode.[0x63] <- (fun () -> 

CBopcode.[0x64] <- (fun () -> 

CBopcode.[0x65] <- (fun () -> 

CBopcode.[0x66] <- (fun () -> 

CBopcode.[0x67] <- (fun () -> 

CBopcode.[0x68] <- (fun () -> 

CBopcode.[0x69] <- (fun () -> 

CBopcode.[0x6A] <- (fun () -> 

CBopcode.[0x6B] <- (fun () -> 

CBopcode.[0x6C] <- (fun () -> 

CBopcode.[0x6D] <- (fun () -> 

CBopcode.[0x6E] <- (fun () -> 

CBopcode.[0x6F] <- (fun () -> 

CBopcode.[0x70] <- (fun () -> 

CBopcode.[0x71] <- (fun () -> 

CBopcode.[0x72] <- (fun () -> 

CBopcode.[0x73] <- (fun () -> 

CBopcode.[0x74] <- (fun () -> 

CBopcode.[0x75] <- (fun () -> 

CBopcode.[0x76] <- (fun () -> 

CBopcode.[0x77] <- (fun () -> 

CBopcode.[0x78] <- (fun () -> 

CBopcode.[0x79] <- (fun () -> 

CBopcode.[0x7A] <- (fun () -> 

CBopcode.[0x7B] <- (fun () -> 

CBopcode.[0x7C] <- (fun () -> 

CBopcode.[0x7D] <- (fun () -> 

CBopcode.[0x7E] <- (fun () -> 

CBopcode.[0x7F] <- (fun () -> 

CBopcode.[0x80] <- (fun () -> 

CBopcode.[0x81] <- (fun () -> 

CBopcode.[0x82] <- (fun () -> 

CBopcode.[0x83] <- (fun () -> 

CBopcode.[0x84] <- (fun () -> 

CBopcode.[0x85] <- (fun () -> 

CBopcode.[0x86] <- (fun () -> 

CBopcode.[0x87] <- (fun () -> 

CBopcode.[0x88] <- (fun () -> 

CBopcode.[0x89] <- (fun () -> 

CBopcode.[0x8A] <- (fun () -> 

CBopcode.[0x8B] <- (fun () -> 

CBopcode.[0x8C] <- (fun () -> 

CBopcode.[0x8D] <- (fun () -> 

CBopcode.[0x8E] <- (fun () -> 

CBopcode.[0x8F] <- (fun () -> 

CBopcode.[0x90] <- (fun () -> 

CBopcode.[0x91] <- (fun () -> 

CBopcode.[0x92] <- (fun () -> 

CBopcode.[0x93] <- (fun () -> 

CBopcode.[0x94] <- (fun () -> 

CBopcode.[0x95] <- (fun () -> 

CBopcode.[0x96] <- (fun () -> 

CBopcode.[0x97] <- (fun () -> 

CBopcode.[0x98] <- (fun () -> 

CBopcode.[0x99] <- (fun () -> 

CBopcode.[0x9A] <- (fun () -> 

CBopcode.[0x9B] <- (fun () -> 

CBopcode.[0x9C] <- (fun () -> 

CBopcode.[0x9D] <- (fun () -> 

CBopcode.[0x9E] <- (fun () -> 

CBopcode.[0x9F] <- (fun () -> 

CBopcode.[0xA0] <- (fun () -> 

CBopcode.[0xA1] <- (fun () -> 

CBopcode.[0xA2] <- (fun () -> 

CBopcode.[0xA3] <- (fun () -> 

CBopcode.[0xA4] <- (fun () -> 

CBopcode.[0xA5] <- (fun () -> 

CBopcode.[0xA6] <- (fun () -> 

CBopcode.[0xA7] <- (fun () -> 

CBopcode.[0xA8] <- (fun () -> 

CBopcode.[0xA9] <- (fun () -> 

CBopcode.[0xAA] <- (fun () -> 

CBopcode.[0xAB] <- (fun () -> 

CBopcode.[0xAC] <- (fun () -> 

CBopcode.[0xAD] <- (fun () -> 

CBopcode.[0xAE] <- (fun () -> 

CBopcode.[0xAF] <- (fun () -> 

CBopcode.[0xB0] <- (fun () -> 

CBopcode.[0xB1] <- (fun () -> 

CBopcode.[0xB2] <- (fun () -> 

CBopcode.[0xB3] <- (fun () -> 

CBopcode.[0xB4] <- (fun () -> 

CBopcode.[0xB5] <- (fun () -> 

CBopcode.[0xB6] <- (fun () -> 

CBopcode.[0xB7] <- (fun () -> 

CBopcode.[0xB8] <- (fun () -> 

CBopcode.[0xB9] <- (fun () -> 

CBopcode.[0xBA] <- (fun () -> 

CBopcode.[0xBB] <- (fun () -> 

CBopcode.[0xBC] <- (fun () -> 

CBopcode.[0xBD] <- (fun () -> 

CBopcode.[0xBE] <- (fun () -> 

CBopcode.[0xBF] <- (fun () -> 

CBopcode.[0xC0] <- (fun () -> 

CBopcode.[0xC1] <- (fun () -> 

CBopcode.[0xC2] <- (fun () -> 

CBopcode.[0xC3] <- (fun () -> 

CBopcode.[0xC4] <- (fun () -> 

CBopcode.[0xC5] <- (fun () -> 

CBopcode.[0xC6] <- (fun () -> 

CBopcode.[0xC7] <- (fun () -> 

CBopcode.[0xC8] <- (fun () -> 

CBopcode.[0xC9] <- (fun () -> 

CBopcode.[0xCA] <- (fun () -> 

CBopcode.[0xCB] <- (fun () -> 

CBopcode.[0xCC] <- (fun () -> 

CBopcode.[0xCD] <- (fun () -> 

CBopcode.[0xCE] <- (fun () -> 

CBopcode.[0xCF] <- (fun () -> 

CBopcode.[0xD0] <- (fun () -> 

CBopcode.[0xD1] <- (fun () -> 

CBopcode.[0xD2] <- (fun () -> 

CBopcode.[0xD3] <- (fun () -> 

CBopcode.[0xD4] <- (fun () -> 

CBopcode.[0xD5] <- (fun () -> 

CBopcode.[0xD6] <- (fun () -> 

CBopcode.[0xD7] <- (fun () -> 

CBopcode.[0xD8] <- (fun () -> 

CBopcode.[0xD9] <- (fun () -> 

CBopcode.[0xDA] <- (fun () -> 

CBopcode.[0xDB] <- (fun () -> 

CBopcode.[0xDC] <- (fun () -> 

CBopcode.[0xDD] <- (fun () -> 

CBopcode.[0xDE] <- (fun () -> 

CBopcode.[0xDF] <- (fun () -> 

CBopcode.[0xE0] <- (fun () -> 

CBopcode.[0xE1] <- (fun () -> 

CBopcode.[0xE2] <- (fun () -> 

CBopcode.[0xE3] <- (fun () -> 

CBopcode.[0xE4] <- (fun () -> 

CBopcode.[0xE5] <- (fun () -> 

CBopcode.[0xE6] <- (fun () -> 

CBopcode.[0xE7] <- (fun () -> 

CBopcode.[0xE8] <- (fun () -> 

CBopcode.[0xE9] <- (fun () -> 

CBopcode.[0xEA] <- (fun () -> 

CBopcode.[0xEB] <- (fun () -> 

CBopcode.[0xEC] <- (fun () -> 

CBopcode.[0xED] <- (fun () -> 

CBopcode.[0xEE] <- (fun () -> 

CBopcode.[0xEF] <- (fun () -> 

CBopcode.[0xF0] <- (fun () -> 

CBopcode.[0xF1] <- (fun () -> 

CBopcode.[0xF2] <- (fun () -> 

CBopcode.[0xF3] <- (fun () -> 

CBopcode.[0xF4] <- (fun () -> 

CBopcode.[0xF5] <- (fun () -> 

CBopcode.[0xF6] <- (fun () -> 

CBopcode.[0xF7] <- (fun () -> 

CBopcode.[0xF8] <- (fun () -> 

CBopcode.[0xF9] <- (fun () -> 

CBopcode.[0xFA] <- (fun () -> 

CBopcode.[0xFB] <- (fun () -> 

CBopcode.[0xFC] <- (fun () -> 

CBopcode.[0xFD] <- (fun () -> 

CBopcode.[0xFE] <- (fun () -> 

CBopcode.[0xFF] <- (fun () -> 
