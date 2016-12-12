module Instruction

open Register
open Mmu

let decSP () = SP <- SP - 1us
let incSP () = SP <- SP + 1us