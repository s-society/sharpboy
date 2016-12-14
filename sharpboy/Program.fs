open System
open System.Windows.Forms
open System.Threading
open System.IO
open System.Text
open System.Drawing
open Memory
open Cpu

type DoubleBufferForm() =
    inherit Form()
    do base.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.UserPaint ||| ControlStyles.DoubleBuffer, true)

[<EntryPoint>]
let main argv = 

    let form = new DoubleBufferForm()

    let SCREEN_WIDTH = 160 
    let SCREEN_HEIGHT = 144
    let SCALE = 2

    let screenBuffer = Array.create (SCREEN_WIDTH*SCREEN_HEIGHT) 0

    let brushes = [| new SolidBrush(Color.FromArgb(223, 253, 234)); new SolidBrush(Color.FromArgb(181, 227, 198)); new SolidBrush(Color.FromArgb(162, 206, 178));  new SolidBrush(Color.FromArgb(3, 36, 15)) |]

    let Draw (args:PaintEventArgs) =
        for y in [0..SCREEN_HEIGHT-1] do
            for x in [0..SCREEN_WIDTH-1] do
                args.Graphics.FillRectangle(brushes.[screenBuffer.[(y*SCREEN_WIDTH) + x]], x * SCALE, y * SCALE, SCALE, SCALE) // problème de type

    let SetInterrupt (bit:int) = memory.[int IF] <- memory.[int IF] ||| (1uy <<< bit)

    let P10_P13_INT_BIT = 4

    form.ClientSize <- new System.Drawing.Size(SCREEN_WIDTH * SCALE, SCREEN_HEIGHT * SCALE)
    form.Load.Add(fun e -> form.BackColor <- Color.Black)//<-Async.Start(Loop) ?
    form.KeyDown.Add(fun e -> match e.KeyCode with
                            | Keys.Right -> SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 &&& ~~~0b0001uy //right
                            | Keys.Left ->  SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 &&& ~~~0b0010uy //left
                            | Keys.Up ->    SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 &&& ~~~0b0100uy //up
                            | Keys.Down ->  SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 &&& ~~~0b1000uy //down
                            | Keys.Z ->     SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 &&& ~~~0b0001uy //A
                            | Keys.X ->     SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 &&& ~~~0b0010uy //B
                            | Keys.Space -> SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 &&& ~~~0b0100uy //SELECT
                            | Keys.Enter -> SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 &&& ~~~0b1000uy //START
                            | _ -> ())
    form.KeyUp.Add(fun e -> stopped <- false
                            match e.KeyCode with
                            | Keys.Right -> SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 ||| 0b0001uy //right
                            | Keys.Left ->  SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 ||| 0b0010uy //left
                            | Keys.Up ->    SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 ||| 0b0100uy //up
                            | Keys.Down ->  SetInterrupt(P10_P13_INT_BIT) ; P14 <- P14 ||| 0b1000uy //down
                            | Keys.Z ->     SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 ||| 0b0001uy //A
                            | Keys.X ->     SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 ||| 0b0010uy //B
                            | Keys.Space -> SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 ||| 0b0100uy //SELECT
                            | Keys.Enter -> SetInterrupt(P10_P13_INT_BIT) ; P15 <- P15 ||| 0b1000uy //START
                            | _ -> ())
 
  
    form.Paint.Add(Draw)
    //form.Closed.Add(fun e -> fpsBrush.Dispose() ; Array.ForEach(brushes,(fun e -> e.Dispose()))) 
    form.Text <- "SharpBoy by S_Society"
    form.MaximizeBox <- false
    form.FormBorderStyle <- FormBorderStyle.FixedSingle
    Application.Run(form)
    0