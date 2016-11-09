---
layout: post
title: "Breaking a loop in F#"
excerpt_separator: <!--more-->
---

During [our work](https://github.com/s-society/chip-8) on the Chip 8 emulator,
we encountered some difficulties while learning the F# language:

One of these difficulties was that as a functional language, F# didn't allow the use of
a `break` keyword during `while` loops.
<!--more-->
Something like this won't work:

```  fsharp
let loop =
  for i in [0..8] do
    if i = 6 then
      printfn "%d" i
      break
```

Instead, you have to leave your imperative thinking behind and go functional, with recursive functions:

``` fsharp
let rec loop_rec i =
  if i < 9 then
    if i != 6 then
      loop_rec (i + 1)
    else
      printfn "%d" i
  ()

let loop =
  loop_rec 0
```

This was a basic exemple, but you can see how we applied it to our work in [this commit](https://github.com/s-society/chip-8/commit/a058707c59b14d5d821310e16962b628af917956).
