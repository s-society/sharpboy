﻿module Mmu

let mutable A, B, C, D, E, F, H, L, SP, PC = 0x01uy,0uy,0x13uy,0uy,0xD8uy,0xB0uy,0x01uy,0x4duy,0xFFFEus,0x0100us 
let mutable ZF,NF,HF,CF = true, false, true, true 
