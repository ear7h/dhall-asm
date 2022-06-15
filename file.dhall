let Prelude = ./prelude.dhall
let Text/concat = Prelude.Text.concat
let Text/concatMapSep = Prelude.Text.concatMapSep

let arch = ./arch.dhall
let Size = arch.Size

let Section = < Text | Data | Rodata | Bss >

let fmtSection = λ(s : Section) →
  merge
    { Text   = ".text"
    , Data   = ".data"
    , Rodata = ".rodata"
    , Bss    = ".bss" }
    s

let Directive =
  < Global  : Text
  | Section : Section
  | Asciz   : Text
  | Equ     : { symbol : Text, value : Integer }
  | Label   : Text
  | Ds      : { size : Size, value : Integer }
  | Dc      : { size : Size, value : Integer }
  | Instr   : Text
  | Raw     : Text
  >

let sizeName = λ(size : Size) →
  merge
    { _1 = "b"
    , _2 = "w"
    , _4 = "l"
    , _8 = "d"
    }
    size

let fmtDirective = λ(dir : Directive) →
  merge
    { Global = λ(sym : Text) →
      ".global ${sym}"

    , Section = fmtSection
    , Asciz = λ(str : Text) →
      ".asciz \"${Text/replace "\"" "\\\"" str}\""

    , Label = λ(label : Text) →
      "${label}:"

    , Equ = λ(args : { symbol : Text, value : Integer }) →
      ".equ ${args.symbol}, ${Integer/show args.value}"

    , Ds = λ(args : { size : Size, value : Integer }) →
      ".ds.${sizeName args.size}, ${Integer/show args.value}"

    , Dc = λ(args : { size : Size, value : Integer }) →
      ".dc.${sizeName args.size}, ${Integer/show args.value}"

    , Instr = λ(raw : Text) → "\t" ++ raw
    , Raw = λ(raw : Text) → raw
    }
    dir

let writeFile = Text/concatMapSep "\n" Directive fmtDirective

in  { Section, Directive, writeFile }
