open System
open System.Windows.Forms
open System.Threading
open System.IO
open System.Text
open System.Drawing
open Memory
open Register
open Cpu

type DoubleBufferForm() =
    inherit Form()
    do base.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.UserPaint ||| ControlStyles.DoubleBuffer, true)

[<EntryPoint>][<STAThread>]
let main argv = 

    memory.[int LCDC] <-    0x91uy ; memory.[int STAT] <- STAT_MODE_10_OAM ; memory.[int SCROLLX] <- 0x0uy
    memory.[int SCROLLY] <- 0x0uy  ; memory.[int LY] <-   0x0uy            ; memory.[int LYC] <-     0x0uy 
    memory.[int IE] <- 0x0uy  

    let openRomDialog = new OpenFileDialog()
    openRomDialog.Title <- "Select Game Boy ROM" 
    openRomDialog.Filter <- "Gameboy ROM Files|*.gb|All files|*.*"
    if openRomDialog.ShowDialog() = DialogResult.OK then
        Memory.rom <- File.ReadAllBytes(openRomDialog.FileName)
    else Environment.Exit(1)

    if rom.Length > int (snd ROM1+1us) then
        rom.[0..int (snd ROM1)].CopyTo(memory,0)
    else
        rom.CopyTo(memory,0)

    //Reading bytes to know MBC type and cartridge options
    Memory.cartridge <- LanguagePrimitives.EnumOfValue<byte, Memory.Cartridge>(memory.[0x0147])
    
    Memory.mbcType <- match cartridge with
                      | Memory.Cartridge.Mbc1 | Memory.Cartridge.Mbc1Ram | Memory.Cartridge.Mbc1RamBatt -> Memory.Mbc.Mbc1
                      | Memory.Cartridge.Mbc2 | Memory.Cartridge.Mbc2Batt -> Memory.Mbc.Mbc2
                      | Memory.Cartridge.Mbc3 | Memory.Cartridge.Mbc3Ram | Memory.Cartridge.Mbc3RamBatt | Memory.Cartridge.Mbc3TimerBatt | Memory.Cartridge.Mbc3TimerRamBatt -> Memory.Mbc.Mbc3
                      | _ -> Memory.Mbc.RomOnly

    let hasExternalRam = match cartridge with
                     | Memory.Cartridge.Mbc1Ram | Memory.Cartridge.Mbc1RamBatt | Memory.Cartridge.RomRam | Memory.Cartridge.RomRamBatt | Memory.Cartridge.Mbc3TimerRamBatt | Memory.Cartridge.Mbc3Ram | Memory.Cartridge.Mbc3RamBatt -> true
                     | _ -> false

    let form = new DoubleBufferForm()

    let SCREEN_WIDTH = 160 
    let SCREEN_HEIGHT = 144
    let SCALE = 2

    let screenBuffer = Array.create (SCREEN_WIDTH*SCREEN_HEIGHT) 0

    let brushes = [| new SolidBrush(Color.FromArgb(255, 255, 255)); new SolidBrush(Color.FromArgb(160, 160, 160)); new SolidBrush(Color.FromArgb(64, 64, 64));  new SolidBrush(Color.FromArgb(0, 0, 0)) |]

    let Draw (args:PaintEventArgs) =
        for y in [0..SCREEN_HEIGHT-1] do
            for x in [0..SCREEN_WIDTH-1] do
                args.Graphics.FillRectangle(brushes.[screenBuffer.[(y*SCREEN_WIDTH) + x]], x * SCALE, y * SCALE, SCALE, SCALE)

    let SetInterrupt (bit:int) = memory.[int IF] <- memory.[int IF] ||| (1uy <<< bit)

    let JumpToInterrupt (address:uint16, bit:int) = 
        if ((memory.[int IF] &&& (1uy <<< bit)) >= 1uy) && ((memory.[int IE] &&& (1uy <<< bit)) >= 1uy) then
            IME <- false ; lcdCycles <- lcdCycles + 32; memory.[int IF] <- memory.[int IF] &&& ~~~(1uy <<< bit); 
            push(PC) ; PC <- address ; true 
        else false

    let SetInterrupt (bit:int) = memory.[int IF] <- memory.[int IF] ||| (1uy <<< bit)
    let SetStatusInterruptIfModeBitSelected (bit:int) = if (memory.[int STAT] &&& (1uy <<< bit)) >= 1uy then SetInterrupt(LCD_STATUS_INT_BIT)
    let SetStatMode (mode:byte) = 
        if memory.[int STAT] <> ((memory.[int STAT] &&& 0xFCuy) ||| mode) then  
            memory.[int STAT] <- (memory.[int STAT] &&& 0xFCuy) ||| mode ; true
        else false 

    let incrementLY () =
        if memory.[int LYC] = memory.[int LY] then
            SetStatusInterruptIfModeBitSelected(STAT_LY_LYC_BIT) 
            memory.[int STAT] <- memory.[int STAT] ||| 0b100uy 
        else 
            memory.[int STAT] <- memory.[int STAT] &&& ~~~(0b100uy)
        memory.[int LY] <- memory.[int LY] + 1uy
        if memory.[int LY] = 154uy then 
            memory.[int LY] <- 0uy

    let Loop  =
        async {
        while true do
            if not stopped then

                cycles <- opcode.[int (Memory.readAddress(PC))]() * 4uy
                if cycles = 0uy then do
                    ignore(MessageBox.Show(String.Format("Invalid Opcode {2}{0:X2} at 0x{1:X4}", Memory.readAddress(PC), (if unhandledCBOpcode then PC-1us else PC), if unhandledCBOpcode then "CB " else String.Empty))); 
                    Environment.Exit(1)
            
                lcdCycles <- lcdCycles + int cycles

                if IME then
                    if not (JumpToInterrupt(P10_P13_INT, P10_P13_INT_BIT)) then
                        if not (JumpToInterrupt(TIMEROF_INT, TIMEROF_INT_BIT)) then
                            if not (JumpToInterrupt(LCD_STATUS_INT, LCD_STATUS_INT_BIT)) then
                                ignore(JumpToInterrupt(VBLANK_INT, VBLANK_INT_BIT))


                if memory.[int LY] < 144uy then
                    if lcdCycles >= (80+172) then
                        if SetStatMode(STAT_MODE_00_HBLANK) then SetStatusInterruptIfModeBitSelected(STAT_MODE_00_BIT)
                    else if lcdCycles >= 80 then
                        ignore(SetStatMode(STAT_MODE_11_OAM_RAM))

            
                if lcdCycles >= (80+172+204) then
                    lcdCycles <- lcdCycles - 456
                    let y = int memory.[int LY]
                    if y < 144 then
                        if y < 143 then
                            ignore(SetStatMode(STAT_MODE_10_OAM))
                            SetStatusInterruptIfModeBitSelected(STAT_MODE_10_BIT)   
                        for x in [0..SCREEN_WIDTH-1] do 
                            let tileOffset = (uint16 ((x + int (memory.[int SCROLLX]))/8) + uint16 (32*((y+int (memory.[int SCROLLY]))/8))) % 0x400us
                            let tilePixelX = uint16 ((x+int (memory.[int SCROLLX]))%8)
                            let tilePixelY = uint16 ((y+int (memory.[int SCROLLY]))%8)
                            let tileIndex = Memory.readAddress(BG_TILE_MAP_SEL + tileOffset)
                            let address = TILE_PATTERN_TABLE_SEL + uint16 (if TILE_PATTERN_TABLE_SEL = TILE_PATTERN_TABLE_1 then (0x800s + ((int16 (sbyte tileIndex)) * 16s)) else (int16 tileIndex*16s)) + (tilePixelY*2us)
                            screenBuffer.[(y*SCREEN_WIDTH) + x] <- (if Memory.readAddress(address) &&& (0b10000000uy >>> int tilePixelX) > 0uy then 1 else 0) ||| (if Memory.readAddress(address+1us) &&& (0b10000000uy >>> int tilePixelX) > 0uy then 0b10 else 0)
                    
                        for sprite in [(int (fst OAM))..4..(int (snd OAM))] do
                            let spx,spy,pattern,flipx,flipy = int memory.[sprite+1], int memory.[sprite], int memory.[sprite+2], int (if (memory.[sprite+3] &&& (1uy <<< 5)) > 0uy then 1uy else 0uy), int (if (memory.[sprite+3] &&& (1uy <<< 6)) > 0uy then 1uy else 0uy) 
                            if spx > 0 && spy > 0 && y >= (spy-16) && y < (spy-16+8) then
                                for tilePixelX in [0..7] do
                                    let ftilePixelX = if flipx = 1 then 7-tilePixelX else tilePixelX
                                    let address = TILE_PATTERN_TABLE_0 + uint16 ((pattern*16) + ((if flipy = 1 then (7-(y-(spy-16))) else y-(spy-16))*2))
                                    let color = (if Memory.readAddress(address) &&& (0b10000000uy >>> ftilePixelX) > 0uy then 1 else 0) ||| (if Memory.readAddress(address+1us) &&& (0b10000000uy >>> ftilePixelX) > 0uy then 0b10 else 0)
                                    if color > 0 then screenBuffer.[(y*SCREEN_WIDTH) + spx - 8 + tilePixelX] <- color
                                     
                    incrementLY()

                    if memory.[int LY] = 144uy then 
                        ignore(SetStatMode(STAT_MODE_01_VBLANK))
                        SetStatusInterruptIfModeBitSelected(STAT_MODE_01_BIT)   
                        SetInterrupt(VBLANK_INT_BIT)

                        if (memory.[int LCDC] &&& 0b10000000uy) > 1uy then 

                            if (memory.[int LCDC] &&& 1uy) = 1uy then
                                form.Invalidate() 

                    if memory.[int LY] = 0uy then
                        ignore(SetStatMode(STAT_MODE_10_OAM))
                        SetStatusInterruptIfModeBitSelected(STAT_MODE_10_BIT) 

                if memory.[int TAC] &&& (1uy <<< TAC_TIMER_STOP_BIT) > 1uy then 
                    timerCycles <- timerCycles + int cycles
                    if timerCycles >= timerOverflow then
                        timerCycles <- timerCycles - timerOverflow
                        memory.[int TIMA] <- memory.[int TIMA] + 1uy;
                        if memory.[int TIMA] = 0uy then
                            memory.[int TIMA] <- memory.[int TMA]
                            SetInterrupt(TIMEROF_INT_BIT)

                divCycles <- divCycles + int cycles
                if divCycles > 256 then
                    divCycles <- divCycles - 256
                    memory.[int DIV] <- memory.[int DIV] + 1uy
        }


    form.ClientSize <- new System.Drawing.Size(SCREEN_WIDTH * SCALE, SCREEN_HEIGHT * SCALE)
    form.Load.Add(fun e -> form.BackColor <- Color.Black ; Async.Start(Loop))
    form.KeyDown.Add(fun e -> match e.KeyCode with
                            | Keys.Right -> SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 &&& ~~~0b0001uy
                            | Keys.Left ->  SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 &&& ~~~0b0010uy
                            | Keys.Up ->    SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 &&& ~~~0b0100uy
                            | Keys.Down ->  SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 &&& ~~~0b1000uy
                            | Keys.W ->     SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 &&& ~~~0b0001uy
                            | Keys.X ->     SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 &&& ~~~0b0010uy
                            | Keys.Space -> SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 &&& ~~~0b0100uy
                            | Keys.Enter -> SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 &&& ~~~0b1000uy
                            | _ -> ())
                            
    form.KeyUp.Add(fun e -> stopped <- false
                            match e.KeyCode with
                            | Keys.Right -> SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 ||| 0b0001uy
                            | Keys.Left ->  SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 ||| 0b0010uy
                            | Keys.Up ->    SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 ||| 0b0100uy
                            | Keys.Down ->  SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 ||| 0b1000uy
                            | Keys.W ->     SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 ||| 0b0001uy
                            | Keys.X ->     SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 ||| 0b0010uy
                            | Keys.Space -> SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 ||| 0b0100uy
                            | Keys.Enter -> SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 ||| 0b1000uy
                            | _ -> ())
 
  
    form.Paint.Add(Draw)
    form.Text <- String.Concat("SharpBoy - ", ASCIIEncoding.ASCII.GetString(memory.[0x0134..0x0142]))
    form.MaximizeBox <- false
    form.FormBorderStyle <- FormBorderStyle.FixedSingle
    Application.Run(form)
    0
