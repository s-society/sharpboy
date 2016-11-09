
﻿module sharpboy
           
//Necessary modules
open System
open System.Windows.Forms 

//Main RAM of the device
let main_RAM = Array.create 8192 0uy

//Video RAM of the device
let video_RAM = Array.create 8192 0uy

//Screen (160x144 = 23040 pixels) represented by an array of bytes each taking four different values %00, %01, %10, %11
let screen = Array.create 23040 0uy

//Eight 8-bit registers A,B,C,D,E,F,H,L each one represented by a byte
let mutable registers = Array.create 8 0uy

//Two 16-bit registers, SP which points to the stack position and starts at $FFFE, and PC which points to the next instruction to be executed and starts at $100
let mutable PC = 0x100us
let mutable SP = 0xFFFEus

//Double Buffer Form for the main window of the emulator
let form = new DoubleBufferForm()

