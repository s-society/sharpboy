module Register

let mutable A = 0x01uy
let mutable B = 0uy
let mutable C = 0x13uy
let mutable D = 0uy
let mutable E = 0xD8uy
let mutable F = 0b10110000uy
let mutable H = 0x01uy
let mutable L = 0x4Duy
let mutable SP = 0xFFFEus
let mutable PC = 0x0100us

let mutable ZF = true
let mutable NF = false
let mutable HF = true
let mutable CF = true

let flag_cons () = byte (((if ZF then 1 else 0) <<< 7) ||| ((if NF then 1 else 0) <<< 6) ||| ((if HF then 1 else 0) <<< 5) ||| ((if CF then 1 else 0) <<< 4)) ||| (F &&& 0x0Fuy) 

