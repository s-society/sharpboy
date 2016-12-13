module Instruction

open Register
open Memory

let decSP () = SP <- SP - 1us
let incSP () = SP <- SP + 1us