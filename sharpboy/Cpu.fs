﻿module Cpu

open Memory
open Register


let bit (b:int, reg:byte) = 
    ZF <- ((reg &&& (1uy <<< b)) = 0uy)
    NF <- false
    HF <- true

let bitHL (b:int) = bit(b, readAddress_2(H,L))

let adcA (n:byte) = 
     Memory.temp <- (if CF then 1uy else 0uy) 
     NF <- false 
     HF <- ((A &&& 0x0Fuy) + (n &&& 0x0Fuy) + Memory.temp) > 0x0Fuy 
     CF <- (int A + int n + int Memory.temp) > 0xFF 
     A <- A + n + Memory.temp 
     ZF <- A = 0uy

let addA (n:byte) = 
     NF <- false 
     HF <- ((A &&& 0x0Fuy) + (n &&& 0x0Fuy)) > 0x0Fuy 
     CF <- (A + n) < A 
     A <- A + n 
     ZF <- A = 0uy

let andA (n:byte) =
     A <- A &&& n 
     ZF <- (A = 0uy) 
     NF <- false
     HF <- true
     CF <- false

let dec (reg:byte byref) =
     reg <- reg - 1uy
     ZF <- (reg = 0uy) 
     NF <- true
     HF <- (reg = 0x0Fuy)

let orA (n:byte) =
     A <- A ||| n 
     ZF <- (A = 0uy) 
     NF <- false
     HF <- false
     CF <- false 

let xorA (n:byte) =
     A <- A ^^^ n 
     ZF <- (A = 0uy) 
     NF <- false
     HF <- false
     CF <- false

let subA (n:byte) =
     NF <- true 
     HF <- (A &&& 0x0Fuy) < (n &&& 0x0Fuy) 
     CF <- A < n 
     A <- A - n 
     ZF <- A = 0uy

let sbcA (n:byte) =
     temp <- (if CF then 1uy else 0uy) 
     NF <- true 
     HF <- (A &&& 0x0Fuy) < ((n &&& 0x0Fuy)+temp) 
     CF <- A < (n + temp)
     A <- A - n - temp 
     ZF <- A = 0uy

let cp (n:byte) =
     ZF <- (A = n) 
     NF <- true
     HF <- (A &&& 0x0Fuy) < (n &&& 0x0Fuy)
     CF <- A < n
    


let decSP () = SP <- SP - 1us 

let inc (reg:byte byref) =
     reg <- reg + 1uy
     ZF <- (reg = 0uy) 
     NF <- false
     HF <- (reg = 0xF0uy)

let incSP () = SP <- SP + 1us 

let jp () = PC <- readAddress16(PC + 1us)

let jpHL () = PC <- uint16 H <<< 8 ||| uint16 L

let pop (into:uint16 byref) =
     into <- ((uint16 memory.[int (SP+1us)] <<< 8) ||| uint16 memory.[int SP]) 
     SP <- SP + 2us

let pop_2 (msb:byte byref, lsb:byte byref) =
     msb <- memory.[int (SP+1us)] 
     lsb <- memory.[int SP] 
     SP <- SP + 2us  

let popAF() =
     pop_2(&A,&F) 
     ZF <- (F &&& (1uy <<< 7)) > 1uy 
     NF <- (F &&& (1uy <<< 6)) > 1uy 
     HF <- (F &&& (1uy <<< 5)) > 1uy 
     CF <- (F &&& (1uy <<< 4)) > 1uy




let rlc (reg:byte byref) =
     CF <- (if reg &&& 0b10000000uy > 1uy then true else false) 
     reg <- (reg <<< 1) ||| (if CF then 1uy else 0uy) 
     ZF <- reg = 0uy
     NF <- false
     HF <- false
    

let rlcHL () =
     temp <- readAddress_2(H,L) 
     rlc(&temp)
     writeAddress_2(H,L, temp)

let rrc (reg:byte byref) =
     CF <- (if reg &&& 1uy >= 1uy then true else false) 
     reg <- (reg >>> 1) ||| (if CF then (1uy<<<7) else 0uy) 
     ZF <- reg = 0uy
     NF <- false
     HF <- false
    

let rrcHL () =
     temp <- readAddress_2(H,L) 
     rrc(&temp)
     writeAddress_2(H,L, temp)

let rl (reg:byte byref) =
     temp <- (if CF then 1uy else 0uy) 
     CF <- (if reg &&& 0b10000000uy > 1uy then true else false) 
     reg <- (reg <<< 1) ||| temp 
     ZF <- reg = 0uy
     NF <- false
     HF <- false
    

let rlHL() =
     temp <- readAddress_2(H,L) 
     rl(&temp)
     writeAddress_2(H,L, temp)

let rr (reg:byte byref) =
     temp <- (if CF then 1uy else 0uy) 
     CF <- (if reg &&& 1uy >= 1uy then true else false) 
     reg <- (reg >>> 1) ||| (temp<<<7) 
     ZF <- reg = 0uy
     NF <- false
     HF <- false
    

let rrHL() =
     temp <- readAddress_2(H,L) 
     rr(&temp)
     writeAddress_2(H,L, temp)

let sla (reg:byte byref) =
     CF <- (if reg &&& 0b10000000uy > 1uy then true else false) 
     reg <- reg <<< 1 
     ZF <- reg = 0uy
     NF <- false 
     HF <- false

let slaHL () =
     temp <- readAddress_2(H,L) 
     sla(&temp)
     writeAddress_2(H,L, temp)

let sra (reg:byte byref) =
     CF <- (if reg &&& 1uy >= 1uy then true else false) 
     reg <- (reg >>> 1) ||| (reg &&& 0b10000000uy)
     ZF <- reg = 0uy
     NF <- false 
     HF <- false

let sraHL () =
     temp <- readAddress_2(H,L) 
     sra(&temp)
     writeAddress_2(H,L, temp)

let swap (reg:byte byref) =
     reg <- (((reg &&& 0xF0uy) >>> 4) ||| ((reg &&& 0xFuy) <<< 4))
     ZF <- (reg = 0uy) 
     NF <- false 
     HF <- false 
     CF <- false

let swapHL () =
     temp <- readAddress_2(H, L) 
     swap(&temp) 
     writeAddress_2(H, L, temp)

let srl (reg:byte byref) =
     CF <- (if reg &&& 1uy >= 1uy then true else false) 
     reg <- reg >>> 1 
     ZF <- reg = 0uy
     NF <- false 
     HF <- false

let srlHL () =
     temp <- readAddress_2(H,L) 
     srl(&temp)
     writeAddress_2(H,L, temp)

let res (b:int, reg:byte byref) = reg <- reg &&& ~~~(1uy <<< b)

let resHL(b:int) =
     temp <- readAddress_2(H,L) 
     res(b, &temp) 
     writeAddress_2(H,L,temp)

let set (b:int, reg:byte byref) = reg <- reg ||| (1uy <<< b)

let setHL(b:int) =
     temp <- readAddress_2(H,L) 
     set(b, &temp) 
     writeAddress_2(H,L,temp)


let opcode = Array.create 0x100 (fun () -> 0uy)

let CBopcode = Array.create (0x100) (fun () -> 0uy)


opcode.[0x00] <- (fun () -> PC <- PC + 1us; 1uy) //NOP

opcode.[0x01] <- (fun () -> )

opcode.[0x02] <- (fun () -> )

opcode.[0x03] <- (fun () -> )

opcode.[0x04] <- (fun () -> )

opcode.[0x05] <- (fun () -> )

opcode.[0x06] <- (fun () -> )

opcode.[0x07] <- (fun () -> )

opcode.[0x08] <- (fun () -> )

opcode.[0x09] <- (fun () -> )

opcode.[0x0A] <- (fun () -> )

opcode.[0x0B] <- (fun () -> )

opcode.[0x0C] <- (fun () -> )

opcode.[0x0D] <- (fun () -> )

opcode.[0x0E] <- (fun () -> )

opcode.[0x0F] <- (fun () -> )

opcode.[0x10] <- (fun () -> )

opcode.[0x11] <- (fun () -> )

opcode.[0x12] <- (fun () -> )

opcode.[0x13] <- (fun () -> )

opcode.[0x14] <- (fun () -> )

opcode.[0x15] <- (fun () -> )

opcode.[0x16] <- (fun () -> )

opcode.[0x17] <- (fun () -> )

opcode.[0x18] <- (fun () -> )

opcode.[0x19] <- (fun () -> )

opcode.[0x1A] <- (fun () -> )

opcode.[0x1B] <- (fun () -> )

opcode.[0x1C] <- (fun () -> )

opcode.[0x1D] <- (fun () -> )

opcode.[0x1E] <- (fun () -> )

opcode.[0x1F] <- (fun () -> )

opcode.[0x20] <- (fun () -> )

opcode.[0x21] <- (fun () -> )

opcode.[0x22] <- (fun () -> )

opcode.[0x23] <- (fun () -> )

opcode.[0x24] <- (fun () -> )

opcode.[0x25] <- (fun () -> )

opcode.[0x26] <- (fun () -> )

opcode.[0x27] <- (fun () -> )

opcode.[0x28] <- (fun () -> )

opcode.[0x29] <- (fun () -> )

opcode.[0x2A] <- (fun () -> )

opcode.[0x2B] <- (fun () -> )

opcode.[0x2C] <- (fun () -> )

opcode.[0x2D] <- (fun () -> )

opcode.[0x2E] <- (fun () -> )

opcode.[0x2F] <- (fun () -> )

opcode.[0x30] <- (fun () -> )

opcode.[0x31] <- (fun () -> )

opcode.[0x32] <- (fun () -> )

opcode.[0x33] <- (fun () -> )

opcode.[0x34] <- (fun () -> )

opcode.[0x35] <- (fun () -> )

opcode.[0x36] <- (fun () -> )

opcode.[0x37] <- (fun () -> )

opcode.[0x38] <- (fun () -> )

opcode.[0x39] <- (fun () -> )

opcode.[0x3A] <- (fun () -> )

opcode.[0x3B] <- (fun () -> )

opcode.[0x3C] <- (fun () -> )

opcode.[0x3D] <- (fun () -> )

opcode.[0x3E] <- (fun () -> )

opcode.[0x3F] <- (fun () -> )

opcode.[0x40] <- (fun () -> )

opcode.[0x41] <- (fun () -> )

opcode.[0x42] <- (fun () -> )

opcode.[0x43] <- (fun () -> )

opcode.[0x44] <- (fun () -> )

opcode.[0x45] <- (fun () -> )

opcode.[0x46] <- (fun () -> )

opcode.[0x47] <- (fun () -> )

opcode.[0x48] <- (fun () -> )

opcode.[0x49] <- (fun () -> )

opcode.[0x4A] <- (fun () -> )

opcode.[0x4B] <- (fun () -> )

opcode.[0x4C] <- (fun () -> )

opcode.[0x4D] <- (fun () -> )

opcode.[0x4E] <- (fun () -> )

opcode.[0x4F] <- (fun () -> )

opcode.[0x50] <- (fun () -> )

opcode.[0x51] <- (fun () -> )

opcode.[0x52] <- (fun () -> )

opcode.[0x53] <- (fun () -> )

opcode.[0x54] <- (fun () -> )

opcode.[0x55] <- (fun () -> )

opcode.[0x56] <- (fun () -> )

opcode.[0x57] <- (fun () -> )

opcode.[0x58] <- (fun () -> )

opcode.[0x59] <- (fun () -> )

opcode.[0x5A] <- (fun () -> )

opcode.[0x5B] <- (fun () -> )

opcode.[0x5C] <- (fun () -> )

opcode.[0x5D] <- (fun () -> )

opcode.[0x5E] <- (fun () -> )

opcode.[0x5F] <- (fun () -> )

opcode.[0x60] <- (fun () -> )

opcode.[0x61] <- (fun () -> )

opcode.[0x62] <- (fun () -> )

opcode.[0x63] <- (fun () -> )

opcode.[0x64] <- (fun () -> )

opcode.[0x65] <- (fun () -> )

opcode.[0x66] <- (fun () -> )

opcode.[0x67] <- (fun () -> )

opcode.[0x68] <- (fun () -> )

opcode.[0x69] <- (fun () -> )

opcode.[0x6A] <- (fun () -> )

opcode.[0x6B] <- (fun () -> )

opcode.[0x6C] <- (fun () -> )

opcode.[0x6D] <- (fun () -> )

opcode.[0x6E] <- (fun () -> )

opcode.[0x6F] <- (fun () -> )

opcode.[0x70] <- (fun () -> )

opcode.[0x71] <- (fun () -> )

opcode.[0x72] <- (fun () -> )

opcode.[0x73] <- (fun () -> )

opcode.[0x74] <- (fun () -> )

opcode.[0x75] <- (fun () -> )

opcode.[0x76] <- (fun () -> )

opcode.[0x77] <- (fun () -> )

opcode.[0x78] <- (fun () -> )

opcode.[0x79] <- (fun () -> )

opcode.[0x7A] <- (fun () -> )

opcode.[0x7B] <- (fun () -> )

opcode.[0x7C] <- (fun () -> )

opcode.[0x7D] <- (fun () -> )

opcode.[0x7E] <- (fun () -> )

opcode.[0x7F] <- (fun () -> )

opcode.[0x80] <- (fun () -> addA(B) ; PC <- PC + 1us; 1uy) 

opcode.[0x81] <- (fun () -> addA(C) ; PC <- PC + 1us; 1uy) 

opcode.[0x82] <- (fun () -> addA(D) ; PC <- PC + 1us; 1uy) 

opcode.[0x83] <- (fun () -> addA(E) ; PC <- PC + 1us; 1uy) 

opcode.[0x84] <- (fun () -> addA(H) ; PC <- PC + 1us; 1uy)

opcode.[0x85] <- (fun () -> addA(L) ; PC <- PC + 1us; 1uy)

opcode.[0x86] <- (fun () -> addA(readAddress_2(H, L)); PC <- PC + 1us; 2uy)

opcode.[0x87] <- (fun () -> addA(A) ; PC <- PC + 1us; 1uy)

opcode.[0x88] <- (fun () -> adcA(B) ; PC <- PC + 1us; 1uy)

opcode.[0x89] <- (fun () -> adcA(C) ; PC <- PC + 1us; 1uy)

opcode.[0x8A] <- (fun () -> adcA(D) ; PC <- PC + 1us; 1uy)

opcode.[0x8B] <- (fun () -> adcA(E) ; PC <- PC + 1us; 1uy)

opcode.[0x8C] <- (fun () -> adcA(H) ; PC <- PC + 1us; 1uy)

opcode.[0x8D] <- (fun () -> adcA(L) ; PC <- PC + 1us; 1uy)

opcode.[0x8E] <- (fun () -> adcA(readAddress_2(H, L)); PC <- PC + 1us; 2uy)

opcode.[0x8F] <- (fun () -> adcA(A) ; PC <- PC + 1us; 1uy)

opcode.[0x90] <- (fun () -> subA(B); PC <- PC + 1us; 1uy) 

opcode.[0x91] <- (fun () -> subA(C); PC <- PC + 1us; 1uy) 

opcode.[0x92] <- (fun () -> subA(D); PC <- PC + 1us; 1uy) 

opcode.[0x93] <- (fun () -> subA(E); PC <- PC + 1us; 1uy) 

opcode.[0x94] <- (fun () -> subA(H); PC <- PC + 1us; 1uy) 

opcode.[0x95] <- (fun () -> subA(L); PC <- PC + 1us; 1uy) 

opcode.[0x96] <- (fun () -> subA(readAddress_2(H, L)); PC <- PC + 1us; 2uy)

opcode.[0x97] <- (fun () -> subA(A); PC <- PC + 1us; 1uy) 

opcode.[0x98] <- (fun () -> sbcA(B); PC <- PC + 1us; 1uy)

opcode.[0x99] <- (fun () -> sbcA(C); PC <- PC + 1us; 1uy)

opcode.[0x9A] <- (fun () -> sbcA(D); PC <- PC + 1us; 1uy)

opcode.[0x9B] <- (fun () -> sbcA(E); PC <- PC + 1us; 1uy)

opcode.[0x9C] <- (fun () -> sbcA(H); PC <- PC + 1us; 1uy)

opcode.[0x9D] <- (fun () -> sbcA(L); PC <- PC + 1us; 1uy)

opcode.[0x9E] <- (fun () -> sbcA(readAddress_2(H, L)); PC <- PC + 1us; 2uy)

opcode.[0x9F] <- (fun () -> sbcA(A); PC <- PC + 1us; 1uy)

opcode.[0xA0] <- (fun () -> andA(B); PC <- PC + 1us; 1uy) 

opcode.[0xA1] <- (fun () -> andA(C); PC <- PC + 1us; 1uy) 

opcode.[0xA2] <- (fun () -> andA(D); PC <- PC + 1us; 1uy) 

opcode.[0xA3] <- (fun () -> andA(E); PC <- PC + 1us; 1uy) 

opcode.[0xA4] <- (fun () -> andA(H); PC <- PC + 1us; 1uy) 

opcode.[0xA5] <- (fun () -> andA(L); PC <- PC + 1us; 1uy) 

opcode.[0xA6] <- (fun () -> andA(readAddress_2(H, L)); PC <- PC + 1us; 2uy

opcode.[0xA7] <- (fun () -> andA(A); PC <- PC + 1us; 1uy) 

opcode.[0xA8] <- (fun () -> xorA(B); PC <- PC + 1us; 1uy)

opcode.[0xA9] <- (fun () -> xorA(C); PC <- PC + 1us; 1uy)

opcode.[0xAA] <- (fun () -> xorA(D); PC <- PC + 1us; 1uy)

opcode.[0xAB] <- (fun () -> xorA(E); PC <- PC + 1us; 1uy)

opcode.[0xAC] <- (fun () -> xorA(H); PC <- PC + 1us; 1uy)

opcode.[0xAD] <- (fun () -> xorA(L); PC <- PC + 1us; 1uy)

opcode.[0xAE] <- (fun () -> xorA(readAddress_2(H, L)); PC <- PC + 1us; 2uy)

opcode.[0xAF] <- (fun () -> xorA(A); PC <- PC + 1us; 1uy)

opcode.[0xB0] <- (fun () -> orA(B); PC <- PC + 1us; 1uy) 

opcode.[0xB1] <- (fun () -> orA(C); PC <- PC + 1us; 1uy) 

opcode.[0xB2] <- (fun () -> orA(D); PC <- PC + 1us; 1uy) 

opcode.[0xB3] <- (fun () -> orA(E); PC <- PC + 1us; 1uy) 

opcode.[0xB4] <- (fun () -> orA(H); PC <- PC + 1us; 1uy) 

opcode.[0xB5] <- (fun () -> orA(L); PC <- PC + 1us; 1uy) 

opcode.[0xB6] <- (fun () -> orA(readAddress_2(H, L)); PC <- PC + 1us; 2uy) 

opcode.[0xB7] <- (fun () -> orA(A); PC <- PC + 1us; 1uy) 

opcode.[0xB8] <- (fun () -> cp(B); PC <- PC + 1us; 1uy)

opcode.[0xB9] <- (fun () -> cp(C); PC <- PC + 1us; 1uy)

opcode.[0xBA] <- (fun () -> cp(D); PC <- PC + 1us; 1uy)

opcode.[0xBB] <- (fun () -> cp(E); PC <- PC + 1us; 1uy)

opcode.[0xBC] <- (fun () -> cp(H); PC <- PC + 1us; 1uy)

opcode.[0xBD] <- (fun () -> cp(L); PC <- PC + 1us; 1uy)

opcode.[0xBE] <- (fun () -> cp(readAddress_2(H, L)); PC <- PC + 1us; 2uy)

opcode.[0xBF] <- (fun () -> cp(A); PC <- PC + 1us; 1uy)

opcode.[0xC0] <- (fun () -> )

opcode.[0xC1] <- (fun () -> )

opcode.[0xC2] <- (fun () -> )

opcode.[0xC3] <- (fun () -> )

opcode.[0xC4] <- (fun () -> )

opcode.[0xC5] <- (fun () -> )

opcode.[0xC6] <- (fun () -> )

opcode.[0xC7] <- (fun () -> )

opcode.[0xC8] <- (fun () -> )

opcode.[0xC9] <- (fun () -> )

opcode.[0xCA] <- (fun () -> )

opcode.[0xCB] <- (fun () -> )

opcode.[0xCC] <- (fun () -> )

opcode.[0xCD] <- (fun () -> )

opcode.[0xCE] <- (fun () -> )

opcode.[0xCF] <- (fun () -> )

opcode.[0xD0] <- (fun () -> )

opcode.[0xD1] <- (fun () -> )

opcode.[0xD2] <- (fun () -> )

opcode.[0xD3] <- (fun () -> )

opcode.[0xD4] <- (fun () -> )

opcode.[0xD5] <- (fun () -> )

opcode.[0xD6] <- (fun () -> )

opcode.[0xD7] <- (fun () -> )

opcode.[0xD8] <- (fun () -> )

opcode.[0xD9] <- (fun () -> )

opcode.[0xDA] <- (fun () -> )

opcode.[0xDB] <- (fun () -> )

opcode.[0xDC] <- (fun () -> )

opcode.[0xDD] <- (fun () -> )

opcode.[0xDE] <- (fun () -> )

opcode.[0xDF] <- (fun () -> )

opcode.[0xE0] <- (fun () -> )

opcode.[0xE1] <- (fun () -> )

opcode.[0xE2] <- (fun () -> )

opcode.[0xE3] <- (fun () -> )

opcode.[0xE4] <- (fun () -> )

opcode.[0xE5] <- (fun () -> )

opcode.[0xE6] <- (fun () -> )

opcode.[0xE7] <- (fun () -> )

opcode.[0xE8] <- (fun () -> )

opcode.[0xE9] <- (fun () -> )

opcode.[0xEA] <- (fun () -> )

opcode.[0xEB] <- (fun () -> )

opcode.[0xEC] <- (fun () -> )

opcode.[0xED] <- (fun () -> )

opcode.[0xEE] <- (fun () -> xorA(readAddress(PC+1us)); PC <- PC + 2us; 2uy) 

opcode.[0xEF] <- (fun () -> )

opcode.[0xF0] <- (fun () -> )

opcode.[0xF1] <- (fun () -> )

opcode.[0xF2] <- (fun () -> )

opcode.[0xF3] <- (fun () -> )

opcode.[0xF4] <- (fun () -> )

opcode.[0xF5] <- (fun () -> )

opcode.[0xF6] <- (fun () -> orA(readAddress(PC+1us)); PC <- PC + 2us; 2uy) 

opcode.[0xF7] <- (fun () -> )

opcode.[0xF8] <- (fun () -> )

opcode.[0xF9] <- (fun () -> )

opcode.[0xFA] <- (fun () -> )

opcode.[0xFB] <- (fun () -> )

opcode.[0xFC] <- (fun () -> )

opcode.[0xFD] <- (fun () -> )

opcode.[0xFE] <- (fun () -> )

opcode.[0xFF] <- (fun () -> )

CBopcode.[0x00] <- (fun () -> rlc(&B);    PC <- PC + 1us; 2uy)

CBopcode.[0x01] <- (fun () -> rlc(&C);    PC <- PC + 1us; 2uy)

CBopcode.[0x02] <- (fun () -> rlc(&D);    PC <- PC + 1us; 2uy)

CBopcode.[0x03] <- (fun () -> rlc(&E);    PC <- PC + 1us; 2uy)

CBopcode.[0x04] <- (fun () -> rlc(&H);    PC <- PC + 1us; 2uy)

CBopcode.[0x05] <- (fun () -> rlc(&L);    PC <- PC + 1us; 2uy)

CBopcode.[0x06] <- (fun () -> rlcHL();    PC <- PC + 1us; 4uy)

CBopcode.[0x07] <- (fun () -> rlc(&A);    PC <- PC + 1us; 2uy)

CBopcode.[0x08] <- (fun () -> rrc(&B);    PC <- PC + 1us; 2uy)

CBopcode.[0x09] <- (fun () -> rrc(&C);    PC <- PC + 1us; 2uy)

CBopcode.[0x0A] <- (fun () -> rrc(&D);    PC <- PC + 1us; 2uy)

CBopcode.[0x0B] <- (fun () -> rrc(&E);    PC <- PC + 1us; 2uy)

CBopcode.[0x0C] <- (fun () -> rrc(&H);    PC <- PC + 1us; 2uy)

CBopcode.[0x0D] <- (fun () -> rrc(&L);    PC <- PC + 1us; 2uy)

CBopcode.[0x0E] <- (fun () -> rrcHL();    PC <- PC + 1us; 4uy)

CBopcode.[0x0F] <- (fun () -> rrc(&A);    PC <- PC + 1us; 2uy)

CBopcode.[0x10] <- (fun () -> rl(&B);     PC <- PC + 1us; 2uy)

CBopcode.[0x11] <- (fun () -> rl(&C);     PC <- PC + 1us; 2uy)

CBopcode.[0x12] <- (fun () -> rl(&D);     PC <- PC + 1us; 2uy)

CBopcode.[0x13] <- (fun () -> rl(&E);     PC <- PC + 1us; 2uy)

CBopcode.[0x14] <- (fun () -> rl(&H);     PC <- PC + 1us; 2uy)

CBopcode.[0x15] <- (fun () -> rl(&L);     PC <- PC + 1us; 2uy)

CBopcode.[0x16] <- (fun () -> rlHL();     PC <- PC + 1us; 4uy)

CBopcode.[0x17] <- (fun () -> rl(&A);     PC <- PC + 1us; 2uy)

CBopcode.[0x18] <- (fun () -> rr(&B);     PC <- PC + 1us; 2uy)

CBopcode.[0x19] <- (fun () -> rr(&C);     PC <- PC + 1us; 2uy)

CBopcode.[0x1A] <- (fun () -> rr(&D);     PC <- PC + 1us; 2uy)

CBopcode.[0x1B] <- (fun () -> rr(&E);     PC <- PC + 1us; 2uy)

CBopcode.[0x1C] <- (fun () -> rr(&H);     PC <- PC + 1us; 2uy)

CBopcode.[0x1D] <- (fun () -> rr(&L);     PC <- PC + 1us; 2uy)

CBopcode.[0x1E] <- (fun () -> rrHL();     PC <- PC + 1us; 4uy)

CBopcode.[0x1F] <- (fun () -> rr(&A);     PC <- PC + 1us; 2uy)

CBopcode.[0x20] <- (fun () -> sla(&B);    PC <- PC + 1us; 2uy)

CBopcode.[0x21] <- (fun () -> sla(&C);    PC <- PC + 1us; 2uy)

CBopcode.[0x22] <- (fun () -> sla(&D);    PC <- PC + 1us; 2uy)

CBopcode.[0x23] <- (fun () -> sla(&E);    PC <- PC + 1us; 2uy)

CBopcode.[0x24] <- (fun () -> sla(&H);    PC <- PC + 1us; 2uy)

CBopcode.[0x25] <- (fun () -> sla(&L);    PC <- PC + 1us; 2uy)

CBopcode.[0x26] <- (fun () -> slaHL();    PC <- PC + 1us; 4uy)

CBopcode.[0x27] <- (fun () -> sla(&A);    PC <- PC + 1us; 2uy)

CBopcode.[0x28] <- (fun () -> sra(&B);    PC <- PC + 1us; 2uy)

CBopcode.[0x29] <- (fun () -> sra(&C);    PC <- PC + 1us; 2uy)

CBopcode.[0x2A] <- (fun () -> sra(&D);    PC <- PC + 1us; 2uy)

CBopcode.[0x2B] <- (fun () -> sra(&E);    PC <- PC + 1us; 2uy)

CBopcode.[0x2C] <- (fun () -> sra(&H);    PC <- PC + 1us; 2uy)

CBopcode.[0x2D] <- (fun () -> sra(&L);    PC <- PC + 1us; 2uy)

CBopcode.[0x2E] <- (fun () -> sraHL();    PC <- PC + 1us; 4uy)

CBopcode.[0x2F] <- (fun () -> sra(&A);    PC <- PC + 1us; 2uy)

CBopcode.[0x30] <- (fun () -> swap(&B);   PC <- PC + 1us; 2uy)

CBopcode.[0x31] <- (fun () -> swap(&C);   PC <- PC + 1us; 2uy)

CBopcode.[0x32] <- (fun () -> swap(&D);   PC <- PC + 1us; 2uy)

CBopcode.[0x33] <- (fun () -> swap(&E);   PC <- PC + 1us; 2uy)

CBopcode.[0x34] <- (fun () -> swap(&H);   PC <- PC + 1us; 2uy)

CBopcode.[0x35] <- (fun () -> swap(&L);   PC <- PC + 1us; 2uy)

CBopcode.[0x36] <- (fun () -> swapHL();   PC <- PC + 1us; 4uy)

CBopcode.[0x37] <- (fun () -> swap(&A);   PC <- PC + 1us; 2uy)

CBopcode.[0x38] <- (fun () -> srl(&B);    PC <- PC + 1us; 2uy)

CBopcode.[0x39] <- (fun () -> srl(&C);    PC <- PC + 1us; 2uy)

CBopcode.[0x3A] <- (fun () -> srl(&D);    PC <- PC + 1us; 2uy)

CBopcode.[0x3B] <- (fun () -> srl(&E);    PC <- PC + 1us; 2uy)

CBopcode.[0x3C] <- (fun () -> srl(&H);    PC <- PC + 1us; 2uy)

CBopcode.[0x3D] <- (fun () -> srl(&L);    PC <- PC + 1us; 2uy)

CBopcode.[0x3E] <- (fun () -> srlHL();    PC <- PC + 1us; 4uy)

CBopcode.[0x3F] <- (fun () -> srl(&A);    PC <- PC + 1us; 2uy)

CBopcode.[0x40] <- (fun () -> bit(0, B);  PC <- PC + 1us; 2uy)

CBopcode.[0x41] <- (fun () -> bit(0, C);  PC <- PC + 1us; 2uy)

CBopcode.[0x42] <- (fun () -> bit(0, D);  PC <- PC + 1us; 2uy)

CBopcode.[0x43] <- (fun () -> bit(0, E);  PC <- PC + 1us; 2uy)

CBopcode.[0x44] <- (fun () -> bit(0, H);  PC <- PC + 1us; 2uy)

CBopcode.[0x45] <- (fun () -> bit(0, L);  PC <- PC + 1us; 2uy)

CBopcode.[0x46] <- (fun () -> bitHL(0);   PC <- PC + 1us; 4uy)

CBopcode.[0x47] <- (fun () -> bit(0, A);  PC <- PC + 1us; 2uy)

CBopcode.[0x48] <- (fun () -> bit(1, B);  PC <- PC + 1us; 2uy)

CBopcode.[0x49] <- (fun () -> bit(1, C);  PC <- PC + 1us; 2uy)

CBopcode.[0x4A] <- (fun () -> bit(1, D);  PC <- PC + 1us; 2uy)

CBopcode.[0x4B] <- (fun () -> bit(1, E);  PC <- PC + 1us; 2uy)

CBopcode.[0x4C] <- (fun () -> bit(1, H);  PC <- PC + 1us; 2uy)

CBopcode.[0x4D] <- (fun () -> bit(1, L);  PC <- PC + 1us; 2uy)

CBopcode.[0x4E] <- (fun () -> bitHL(1);   PC <- PC + 1us; 4uy)

CBopcode.[0x4F] <- (fun () -> bit(1, A);  PC <- PC + 1us; 2uy)

CBopcode.[0x50] <- (fun () -> bit(2, B);  PC <- PC + 1us; 2uy)

CBopcode.[0x51] <- (fun () -> bit(2, C);  PC <- PC + 1us; 2uy)

CBopcode.[0x52] <- (fun () -> bit(2, D);  PC <- PC + 1us; 2uy)

CBopcode.[0x53] <- (fun () -> bit(2, E);  PC <- PC + 1us; 2uy)

CBopcode.[0x54] <- (fun () -> bit(2, H);  PC <- PC + 1us; 2uy)

CBopcode.[0x55] <- (fun () -> bit(2, L);  PC <- PC + 1us; 2uy)

CBopcode.[0x56] <- (fun () -> bitHL(2);   PC <- PC + 1us; 4uy)

CBopcode.[0x57] <- (fun () -> bit(2, A);  PC <- PC + 1us; 2uy)

CBopcode.[0x58] <- (fun () -> bit(3, B);  PC <- PC + 1us; 2uy)

CBopcode.[0x59] <- (fun () -> bit(3, C);  PC <- PC + 1us; 2uy)

CBopcode.[0x5A] <- (fun () -> bit(3, D);  PC <- PC + 1us; 2uy)

CBopcode.[0x5B] <- (fun () -> bit(3, E);  PC <- PC + 1us; 2uy)

CBopcode.[0x5C] <- (fun () -> bit(3, H);  PC <- PC + 1us; 2uy)

CBopcode.[0x5D] <- (fun () -> bit(3, L);  PC <- PC + 1us; 2uy)

CBopcode.[0x5E] <- (fun () -> bitHL(3);   PC <- PC + 1us; 4uy)

CBopcode.[0x5F] <- (fun () -> bit(3, A);  PC <- PC + 1us; 2uy)

CBopcode.[0x60] <- (fun () -> bit(4, B);  PC <- PC + 1us; 2uy)

CBopcode.[0x61] <- (fun () -> bit(4, C);  PC <- PC + 1us; 2uy)

CBopcode.[0x62] <- (fun () -> bit(4, D);  PC <- PC + 1us; 2uy)

CBopcode.[0x63] <- (fun () -> bit(4, E);  PC <- PC + 1us; 2uy)

CBopcode.[0x64] <- (fun () -> bit(4, H);  PC <- PC + 1us; 2uy)

CBopcode.[0x65] <- (fun () -> bit(4, L);  PC <- PC + 1us; 2uy)

CBopcode.[0x66] <- (fun () -> bitHL(4);   PC <- PC + 1us; 4uy)

CBopcode.[0x67] <- (fun () -> bit(4, A);  PC <- PC + 1us; 2uy)

CBopcode.[0x68] <- (fun () -> bit(5, B);  PC <- PC + 1us; 2uy)

CBopcode.[0x69] <- (fun () -> bit(5, C);  PC <- PC + 1us; 2uy)

CBopcode.[0x6A] <- (fun () -> bit(5, D);  PC <- PC + 1us; 2uy)

CBopcode.[0x6B] <- (fun () -> bit(5, E);  PC <- PC + 1us; 2uy)

CBopcode.[0x6C] <- (fun () -> bit(5, H);  PC <- PC + 1us; 2uy)

CBopcode.[0x6D] <- (fun () -> bit(5, L);  PC <- PC + 1us; 2uy)

CBopcode.[0x6E] <- (fun () -> bitHL(5);   PC <- PC + 1us; 4uy)

CBopcode.[0x6F] <- (fun () -> bit(5, A);  PC <- PC + 1us; 2uy)

CBopcode.[0x70] <- (fun () -> bit(6, B);  PC <- PC + 1us; 2uy)

CBopcode.[0x71] <- (fun () -> bit(6, C);  PC <- PC + 1us; 2uy)

CBopcode.[0x72] <- (fun () -> bit(6, D);  PC <- PC + 1us; 2uy)

CBopcode.[0x73] <- (fun () -> bit(6, E);  PC <- PC + 1us; 2uy)

CBopcode.[0x74] <- (fun () -> bit(6, H);  PC <- PC + 1us; 2uy)

CBopcode.[0x75] <- (fun () -> bit(6, L);  PC <- PC + 1us; 2uy)

CBopcode.[0x76] <- (fun () -> bitHL(6);   PC <- PC + 1us; 4uy)

CBopcode.[0x77] <- (fun () -> bit(6, A);  PC <- PC + 1us; 2uy)

CBopcode.[0x78] <- (fun () -> bit(7, B);  PC <- PC + 1us; 2uy)

CBopcode.[0x79] <- (fun () -> bit(7, C);  PC <- PC + 1us; 2uy)

CBopcode.[0x7A] <- (fun () -> bit(7, D);  PC <- PC + 1us; 2uy)

CBopcode.[0x7B] <- (fun () -> bit(7, E);  PC <- PC + 1us; 2uy)

CBopcode.[0x7C] <- (fun () -> bit(7, H);  PC <- PC + 1us; 2uy)

CBopcode.[0x7D] <- (fun () -> bit(7, L);  PC <- PC + 1us; 2uy)

CBopcode.[0x7E] <- (fun () -> bitHL(7);   PC <- PC + 1us; 4uy)

CBopcode.[0x7F] <- (fun () -> bit(7, A);  PC <- PC + 1us; 2uy)

CBopcode.[0x80] <- (fun () -> res(0, &B); PC <- PC + 1us; 2uy)

CBopcode.[0x81] <- (fun () -> res(0, &C); PC <- PC + 1us; 2uy)

CBopcode.[0x82] <- (fun () -> res(0, &D); PC <- PC + 1us; 2uy)

CBopcode.[0x83] <- (fun () -> res(0, &E); PC <- PC + 1us; 2uy)

CBopcode.[0x84] <- (fun () -> res(0, &H); PC <- PC + 1us; 2uy)

CBopcode.[0x85] <- (fun () -> res(0, &L); PC <- PC + 1us; 2uy)

CBopcode.[0x86] <- (fun () -> resHL(0);   PC <- PC + 1us; 4uy)

CBopcode.[0x87] <- (fun () -> res(0, &A); PC <- PC + 1us; 2uy)

CBopcode.[0x88] <- (fun () -> res(1, &B); PC <- PC + 1us; 2uy)

CBopcode.[0x89] <- (fun () -> res(1, &C); PC <- PC + 1us; 2uy)

CBopcode.[0x8A] <- (fun () -> res(1, &D); PC <- PC + 1us; 2uy)

CBopcode.[0x8B] <- (fun () -> res(1, &E); PC <- PC + 1us; 2uy)

CBopcode.[0x8C] <- (fun () -> res(1, &H); PC <- PC + 1us; 2uy)

CBopcode.[0x8D] <- (fun () -> res(1, &L); PC <- PC + 1us; 2uy)

CBopcode.[0x8E] <- (fun () -> resHL(1);   PC <- PC + 1us; 4uy)

CBopcode.[0x8F] <- (fun () -> res(1, &A); PC <- PC + 1us; 2uy)

CBopcode.[0x90] <- (fun () -> res(2, &B); PC <- PC + 1us; 2uy)

CBopcode.[0x91] <- (fun () -> res(2, &C); PC <- PC + 1us; 2uy)

CBopcode.[0x92] <- (fun () -> res(2, &D); PC <- PC + 1us; 2uy)

CBopcode.[0x93] <- (fun () -> res(2, &E); PC <- PC + 1us; 2uy)

CBopcode.[0x94] <- (fun () -> res(2, &H); PC <- PC + 1us; 2uy)

CBopcode.[0x95] <- (fun () -> res(2, &L); PC <- PC + 1us; 2uy)

CBopcode.[0x96] <- (fun () -> resHL(2);   PC <- PC + 1us; 4uy)

CBopcode.[0x97] <- (fun () -> res(2, &A); PC <- PC + 1us; 2uy)

CBopcode.[0x98] <- (fun () -> res(3, &B); PC <- PC + 1us; 2uy)

CBopcode.[0x99] <- (fun () -> res(3, &C); PC <- PC + 1us; 2uy)

CBopcode.[0x9A] <- (fun () -> res(3, &D); PC <- PC + 1us; 2uy)

CBopcode.[0x9B] <- (fun () -> res(3, &E); PC <- PC + 1us; 2uy)

CBopcode.[0x9C] <- (fun () -> res(3, &H); PC <- PC + 1us; 2uy)

CBopcode.[0x9D] <- (fun () -> res(3, &L); PC <- PC + 1us; 2uy)

CBopcode.[0x9E] <- (fun () -> resHL(3);   PC <- PC + 1us; 4uy)

CBopcode.[0x9F] <- (fun () -> res(3, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xA0] <- (fun () -> res(4, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xA1] <- (fun () -> res(4, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xA2] <- (fun () -> res(4, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xA3] <- (fun () -> res(4, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xA4] <- (fun () -> res(4, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xA5] <- (fun () -> res(4, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xA6] <- (fun () -> resHL(4);   PC <- PC + 1us; 4uy)

CBopcode.[0xA7] <- (fun () -> res(4, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xA8] <- (fun () -> res(5, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xA9] <- (fun () -> res(5, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xAA] <- (fun () -> res(5, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xAB] <- (fun () -> res(5, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xAC] <- (fun () -> res(5, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xAD] <- (fun () -> res(5, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xAE] <- (fun () -> resHL(5);   PC <- PC + 1us; 4uy)

CBopcode.[0xAF] <- (fun () -> res(5, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xB0] <- (fun () -> res(6, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xB1] <- (fun () -> res(6, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xB2] <- (fun () -> res(6, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xB3] <- (fun () -> res(6, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xB4] <- (fun () -> res(6, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xB5] <- (fun () -> res(6, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xB6] <- (fun () -> resHL(6);   PC <- PC + 1us; 4uy)

CBopcode.[0xB7] <- (fun () -> res(6, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xB8] <- (fun () -> res(7, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xB9] <- (fun () -> res(7, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xBA] <- (fun () -> res(7, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xBB] <- (fun () -> res(7, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xBC] <- (fun () -> res(7, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xBD] <- (fun () -> res(7, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xBE] <- (fun () -> resHL(7);   PC <- PC + 1us; 4uy)

CBopcode.[0xBF] <- (fun () -> res(7, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xC0] <- (fun () -> set(0, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xC1] <- (fun () -> set(0, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xC2] <- (fun () -> set(0, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xC3] <- (fun () -> set(0, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xC4] <- (fun () -> set(0, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xC5] <- (fun () -> set(0, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xC6] <- (fun () -> setHL(0);   PC <- PC + 1us; 4uy)

CBopcode.[0xC7] <- (fun () -> set(0, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xC8] <- (fun () -> set(1, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xC9] <- (fun () -> set(1, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xCA] <- (fun () -> set(1, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xCB] <- (fun () -> set(1, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xCC] <- (fun () -> set(1, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xCD] <- (fun () -> set(1, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xCE] <- (fun () -> setHL(1);   PC <- PC + 1us; 4uy)

CBopcode.[0xCF] <- (fun () -> set(1, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xD0] <- (fun () -> set(2, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xD1] <- (fun () -> set(2, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xD2] <- (fun () -> set(2, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xD3] <- (fun () -> set(2, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xD4] <- (fun () -> set(2, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xD5] <- (fun () -> set(2, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xD6] <- (fun () -> setHL(2);   PC <- PC + 1us; 4uy)

CBopcode.[0xD7] <- (fun () -> set(2, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xD8] <- (fun () -> set(3, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xD9] <- (fun () -> set(3, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xDA] <- (fun () -> set(3, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xDB] <- (fun () -> set(3, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xDC] <- (fun () -> set(3, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xDD] <- (fun () -> set(3, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xDE] <- (fun () -> setHL(3);   PC <- PC + 1us; 4uy)

CBopcode.[0xDF] <- (fun () -> set(3, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xE0] <- (fun () -> set(4, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xE1] <- (fun () -> set(4, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xE2] <- (fun () -> set(4, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xE3] <- (fun () -> set(4, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xE4] <- (fun () -> set(4, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xE5] <- (fun () -> set(4, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xE6] <- (fun () -> setHL(4);   PC <- PC + 1us; 4uy)

CBopcode.[0xE7] <- (fun () -> set(4, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xE8] <- (fun () -> set(5, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xE9] <- (fun () -> set(5, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xEA] <- (fun () -> set(5, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xEB] <- (fun () -> set(5, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xEC] <- (fun () -> set(5, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xED] <- (fun () -> set(5, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xEE] <- (fun () -> setHL(5);   PC <- PC + 1us; 4uy)

CBopcode.[0xEF] <- (fun () -> set(5, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xF0] <- (fun () -> set(6, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xF1] <- (fun () -> set(6, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xF2] <- (fun () -> set(6, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xF3] <- (fun () -> set(6, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xF4] <- (fun () -> set(6, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xF5] <- (fun () -> set(6, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xF6] <- (fun () -> setHL(6);   PC <- PC + 1us; 4uy)

CBopcode.[0xF7] <- (fun () -> set(6, &A); PC <- PC + 1us; 2uy)

CBopcode.[0xF8] <- (fun () -> set(7, &B); PC <- PC + 1us; 2uy)

CBopcode.[0xF9] <- (fun () -> set(7, &C); PC <- PC + 1us; 2uy)

CBopcode.[0xFA] <- (fun () -> set(7, &D); PC <- PC + 1us; 2uy)

CBopcode.[0xFB] <- (fun () -> set(7, &E); PC <- PC + 1us; 2uy)

CBopcode.[0xFC] <- (fun () -> set(7, &H); PC <- PC + 1us; 2uy)

CBopcode.[0xFD] <- (fun () -> set(7, &L); PC <- PC + 1us; 2uy)

CBopcode.[0xFE] <- (fun () -> setHL(7);   PC <- PC + 1us; 4uy)

CBopcode.[0xFF] <- (fun () -> set(7, &A); PC <- PC + 1us; 2uy)

