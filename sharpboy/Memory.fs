module Memory


let memory = Array.create (0xFFFF+1) 0uy
let mutable temp = 0uy
let mutable temp16 = 0us
let mutable swap = 0
//Initializing rom to be able to define readAdress before loading the rom from dialog box
let mutable rom = [|0uy|]

// Keep track of different memory places
let ROM0 = (0us, 0x3FFFus) // ROM 0 (16 KB)
let ROM1 = (0x4000us, 0x7FFFus) // ROM 1 (16 KB)
let VRAM = (0x8000us,0x9FFFus) // Video RAM (8KB)
let IRAM = (0xC000us,0xDFFFus) // Interal RAM (8KB)
let ECHO = (0xE000us,0xFDFFus) // Echo of internal RAM
let OAM = (0xFE00us, 0xFE9Fus) // Object attribute memory
let SWRAMBANK = (0xA000us,0xBFFFus) // Switchable RAM

let mutable P14, P15 = 0xEFuy,0xDFuy //Joypad out ports

let P1,DIV,TAC,WY,WX = 0xFF00us,0xFF04us,0xFF07us,0xFF4Aus,0xFF4Bus 
let LCDC,STAT,SCROLLY,SCROLLX = 0xFF40us, 0xFF41us, 0xFF42us, 0xFF43us 
let LY,LYC,DMA = 0xFF44us, 0xFF45us, 0xFF46us
let BGP,OBP0,OBP1 = 0xFF47us, 0xFF48us, 0xFF49us
let IF,IE = 0xFF0Fus, 0xFFFFus
let TIMA,TMA = 0xFF05us,0xFF06us
let TAC_TIMER_STOP_BIT = 2

let mutable IME = false
let mutable unhandledCBOpcode = false
let mutable stopped = false
let mutable cbOpCycles = 0uy

let STAT_MODE_00_HBLANK,STAT_MODE_01_VBLANK,STAT_MODE_10_OAM,STAT_MODE_11_OAM_RAM = 0b00uy,0b01uy,0b10uy,0b11uy
let STAT_MODE_00_BIT,STAT_MODE_01_BIT,STAT_MODE_10_BIT,STAT_LY_LYC_BIT = 3,4,5,6

let TILE_PATTERN_TABLE_0 = 0x8000us
let TILE_PATTERN_TABLE_1 = 0x8800us
let BG_TILE_MAP_0 = 0x9800us
let BG_TILE_MAP_1 = 0x9C00us

let mutable TILE_PATTERN_TABLE_SEL = TILE_PATTERN_TABLE_0
let mutable BG_TILE_MAP_SEL = BG_TILE_MAP_0
let mutable WINDOW_TILE_MAP_SEL = BG_TILE_MAP_1

let mutable cycles, lcdCycles, timerCycles, timerOverflow, divCycles = 0uy, 0, 0, 0, 0

let VBLANK_INT,LCD_STATUS_INT,TIMEROF_INT,P10_P13_INT = 0x0040us,0x0048us,0x0058us,0x0060us
let VBLANK_INT_BIT,LCD_STATUS_INT_BIT,TIMEROF_INT_BIT,P10_P13_INT_BIT = 0,1,2,4

type Cartridge = | RomOnly = 0uy
                 | Mbc1 = 1uy | Mbc1Ram = 2uy | Mbc1RamBatt = 3uy
                 | Mbc2 = 5uy | Mbc2Batt = 6uy
                 | RomRam = 0x8uy | RomRamBatt = 0x9uy
                 | Mbc3TimerBatt = 0xFuy | Mbc3TimerRamBatt = 0x10uy | Mbc3 = 0x11uy
                 | Mbc3Ram = 0x12uy | Mbc3RamBatt = 0x13uy

let mutable cartridge = Cartridge.RomOnly


let mutable externalRamEnabled = false

type Mbc = | RomOnly = 0
           | Mbc1 = 1 
           | Mbc2 = 2 
           | Mbc3 = 3 

let mutable mbcType = Mbc.RomOnly

let mutable mbc1RamMode = false

//ROM banks for MBC
let mutable romBank, romBankOffset = 0,0x4000
let mutable ramBank, ramBankOffset = 0,0

//External RAM
let hasExternalRam = match cartridge with
                     | Cartridge.Mbc1Ram | Cartridge.Mbc1RamBatt | Cartridge.RomRam | Cartridge.RomRamBatt | Cartridge.Mbc3TimerRamBatt | Cartridge.Mbc3Ram | Cartridge.Mbc3RamBatt -> true
                     | _ -> false

// Default values
memory.[int LCDC] <-    0x91uy ; memory.[int STAT] <- STAT_MODE_10_OAM ; memory.[int SCROLLX] <- 0x0uy
memory.[int SCROLLY] <- 0x0uy  ; memory.[int LY] <-   0x0uy            ; memory.[int LYC] <-     0x0uy 
memory.[int IE] <- 0x0uy    

//Updated write function for handling MBC
let  writeAddress (address:uint16, data:byte) = 
    match address with
    //Writing to ROM space (ROM0&ROM1)
    | address when address <= snd ROM1 -> if mbcType <> Mbc.RomOnly || hasExternalRam then 
                                                match address with
                                                //0000 - 1FFF Enable external RAM (if XXXX1010 (0x0A))
                                                | address when address <= 0x1FFFus -> externalRamEnabled <- (data &&& 0x0Fuy) = 0x0Auy
                                                //2000 - 3FFF Switch ROM Banks (MBC1: XXXBBBBB (0x1F), MBC3: XBBBBBBB (0x7F) )
                                                | address when address >= 0x2000us && address <= 0x3FFFus -> match mbcType with
                                                                                                             | Mbc.Mbc1 -> romBank <- romBank &&& 0x60
                                                                                                                           swap <- int (data &&& 0x1Fuy) 
                                                                                                                           if swap = 0 then swap <- 1
                                                                                                                           romBank <- romBank ||| swap
                                                                                                                           romBankOffset <- romBank * 0x4000

                                                                                                             | _ -> romBank <- int (data)  
                                                                                                                    if romBank = 0 then romBank <- 1
                                                                                                                    romBankOffset <- romBank * 0x4000                                                                                                                               
                                                //4000 - 5FFF Switch ROM Bank set / RAM Bank (MBC1)
                                                | address when address >= 0x4000us && address <= 0x5FFFus -> if mbcType = Mbc.Mbc1 then
                                                                                                                 if mbc1RamMode then 
                                                                                                                    ramBank <- int (data &&& 0b11uy)
                                                                                                                    ramBankOffset <- ramBank * 0x2000
                                                                                                                 else
                                                                                                                    romBank <- romBank &&& 0x1F
                                                                                                                    romBank <- romBank ||| (((int data &&& 0b11))<<<5)
                                                                                                                    romBankOffset <- romBank * 0x4000
                                                                                                              else 
                                                                                                                 let mutable swap = int (data &&& 0x0Fuy)
                                                                                                                 if swap < 4 then 
                                                                                                                    ramBank <- swap
                                                                                                                    mbc1RamMode <- true
                                                                                                                 else if swap > 7 && swap < 0xD then
                                                                                                                    ramBank <- swap
                                                                                                                    mbc1RamMode <- false                                                                                                                                                                        //romBankOffset <- uint16 (data) * 0x4000us     
                                                //6000 - 7FFF ROM(0) or RAM(1) Mode (MBC1), 
                                                | address when address >= 0x6000us && address <= 0x7FFFus -> if mbcType = Mbc.Mbc1 then 
                                                                                                                mbc1RamMode <- if (data &&& 1uy) = 1uy then true else false
                                                                                                             else () 
               
    //Writing to SWRAMBANK
    | address when address >= (fst SWRAMBANK) && 
                   address <= (snd SWRAMBANK) ->  if (hasExternalRam && externalRamEnabled) then
                                                     rom.[int address + int ramBankOffset] <- data
                                                  if not hasExternalRam then 
                                                     memory.[int address] <- data
                                                  //if hasExternalRam && hasBattery then //TODO:
                                                     //File.WriteAllBytes("test.sav", rom.[int (fst SWRAMBANK)..(int (snd SWRAMBANK)])
                                               
    | address when address = P1 -> match ((data &&& 0b110000uy) >>> 4) with //bits 4 (P14 out port) & 5 (P15 out port)
                                   | 0b00uy -> memory.[int address] <- P14 &&& P15
                                   | 0b01uy -> memory.[int address] <- P15
                                   | 0b10uy -> memory.[int address] <- P14
                                   | 0b11uy -> memory.[int address] <- 0xFFuy 
    
    | address when address = LY -> memory.[int address] <- 0uy
    | address when address = TAC -> memory.[int address] <- data
                                    match data &&& 0b11uy with
                                   | 0b00uy -> timerOverflow <- 0x0400 //4.096 KHz
                                   | 0b01uy -> timerOverflow <- 0x0010 //262.144 Khz
                                   | 0b10uy -> timerOverflow <- 0x0040 //65.536 KHz
                                   | 0b11uy -> timerOverflow <- 0x0100 //16.384 KHz 
                                          
    | address when address = IF -> memory.[int address] <- data &&& 0x1Fuy
    | address when address = IE -> memory.[int address] <- data &&& 0x1Fuy
    | address when address = DMA -> let mutable dmaAddress = uint16 data <<< 8
                                    for oamAddress in [fst OAM..snd OAM] do
                                        memory.[int oamAddress] <- memory.[int dmaAddress]
                                        dmaAddress <- dmaAddress+1us
    | address when address = DIV -> memory.[int address] <- 0uy
    | address when address = BGP -> memory.[int address] <- data //TODO: BGP
    | address when address = OBP0 -> memory.[int address] <- data //TODO: OBP0
    | address when address = OBP1 -> memory.[int address] <- data //TODO: OBP1
    | address when address = LCDC -> if (data >>> 7) <> (memory.[int address] >>> 7) then
                                        lcdCycles <- 0
                                        memory.[int LY] <- 0uy
                                        //if memory.[int LYC] = memory.[int LY] then memory.[int STAT] <- memory.[int STAT] ||| 0b100uy else memory.[int STAT] <- memory.[int STAT] &&& ~~~(0b100uy)
                                        //memory.[int STAT] <- (memory.[int STAT] &&& 0xFCuy) ||| 0b10uy //->OAM
                                     memory.[int address] <- data
                                     BG_TILE_MAP_SEL <- if data &&& (1uy <<< 3) = 0uy then BG_TILE_MAP_0 else BG_TILE_MAP_1 
                                     TILE_PATTERN_TABLE_SEL <- if data &&& (1uy <<< 4) > 0uy then TILE_PATTERN_TABLE_0 else TILE_PATTERN_TABLE_1 
                                     WINDOW_TILE_MAP_SEL <- if data &&& (1uy <<< 6) = 0uy then BG_TILE_MAP_0 else BG_TILE_MAP_1                                       
    | address when (address >= fst ECHO && address <= snd ECHO) -> memory.[int address] <- data
                                                                   memory.[int address - 0x2000] <- data                                                    
    | address when (address >= fst IRAM && address <= snd IRAM) -> memory.[int address] <- data
                                                                   if address < 0xDE00us then memory.[int address + 0x2000] <- data
    | _ -> memory.[int address] <- data 

// Write to 16bit address obtained from 2 8bit numbers
let writeAddress_2 (first:byte, second: byte, data:byte) =
    writeAddress((uint16 first <<< 8) ||| uint16 second, data)

// Split 16 bits of data in 2 8 bit addresses
let writeAddress16 (address:uint16, data:uint16) = 
    writeAddress(address, byte (data &&& 0xFF00us >>> 8))
    writeAddress(address+1us, byte (data &&& 0xFFus))

// Write 16bits of data to 16 bit address obtained from 2 8bit numbers
let writeAddress16_2 (addressFirst:byte, addressSecond:byte, data:uint16) = 
    writeAddress16(uint16 addressFirst <<< 8 ||| uint16 addressSecond, data)

// Get data and address from 2 8bit numbers
let writeAddress16_2_2 (addressFirst:byte, addressSecond:byte, dataFirst:byte, dataSecond:byte) = 
    writeAddress16(uint16 addressFirst <<< 8 ||| uint16 addressSecond, uint16 dataFirst <<< 8 ||| uint16 dataSecond)

// Same for reading, much simpler
let readAddress (address:uint16) = 
    memory.[int address]

let readAddress_2 (msb:byte, lsb: byte) = readAddress((uint16 msb <<< 8) ||| uint16 lsb)
let readAddress16 (address:uint16) = uint16 (readAddress(address+1us)) <<< 8 ||| uint16 (readAddress(address))
let readAddress16_2 (msb:byte, lsb: byte) = readAddress16(uint16 msb <<< 8 ||| uint16 lsb)
