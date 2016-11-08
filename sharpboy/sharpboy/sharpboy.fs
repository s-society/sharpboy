open System

//Eight 8-bit registers A,B,C,D,E,F,H,L each one represented by a byte
let mutable registers = Array.create 8 0uy

//Two 16-bit registers SP which points to the stack position and PC which points to the next instruction to be executed
let mutable PC = 0us
let mutable SP = 0us


 