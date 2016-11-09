
﻿//Necessary modules
open System 
open System.Windows.Forms
open System.IO
open System.Drawing

[<EntryPoint>]
let main argv = 
    let mutable rom_name = ""
    let user_interface = new OpenFileDialog()
    user_interface.Title <- "Open yor GameBoy ROM"
    user_interface.Filter <- "GameBoy ROM Files|*.gb|All files|*.*"
    match user_interface.ShowDialog() with

        | DialogResult.OK -> do let rom = File.ReadAllBytes(user_interface.FileName)
                                if rom.Length = 0 || rom.Length > 32767 then
                                    ignore(MessageBox.Show("Invalid","Fatal Error",MessageBoxButtons.OK, MessageBoxIcon.Error))
                                    Environment.Exit(1)
                                else 
                                    rom_name <- user_interface.FileName
                                    rom.CopyTo(sharpboy.main_RAM,int sharpboy.PC)

        | _ -> Environment.Exit(1)


    //instruction respresents the current OpCode in hex 
    let mutable instruction = 0x0us

    (* 

    Fix Me : 

    Create Clock
    Add main_loop function with pattern matching and clock synchronisation
    Implement 8-bit sounds 

    *)

    sharpboy.form.ClientSize -< new System.Drawing.Size(160 * 16 , 144 * 16)
    sharpboy.form.Load.Add(fun e -> sharpboy.form.BackColor <- Color.Black
                           Async.Start(main_loop))
    sharpboy.form.Text <- String.Format("{0} - SharpBoy Emulator", rom_name)
    sharpboy.form.MaximizeBox <- false
    sharpboy.form.FormBorderStyle <- FormBorderStyle.FixedSingle
    Application.Run(sharpboy.form)


                 
    0 // return an integer exit code 0 meaning all went well, 1 meaning an abnormal exit

