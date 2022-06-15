let file =
      ./file.dhall
        sha256:cdc0a89be943e0d28f1df5fc126dea00a836820c4e77fe36af4e28fd6359131b

let D = file.Directive

let arch =
      ./arch.dhall
        sha256:9ed55ff2d4d3a9089eb2133734dfc95e689cdc5a2a1108290be615550526f179

let Size = arch.Size

let x86_64
    : arch.Arch D
    = ./x86_64.dhall
        sha256:a9e7f635204ad10eae863578a88b427789a5c44024d1bcdae0584364db2b4c9d
        D
        D.Instr

let mkPrelude =
      λ(arch : arch.Arch D) →
        { writeFile = file.writeFile
        , Size = arch@1.Size
        , Reg = arch@1.Reg
        , Val = arch@1.Val
        , BranchTarget = arch@1.BranchTarget
        , Cond = arch@1.Cond
        , global = D.Global
        , data = D.Section file.Section.Data
        , text = D.Section file.Section.Text
        , section = D.Section
        , Section = file.Section
        , asciz = D.Asciz
        , equ = λ(symbol : Text) → λ(value : Integer) → D.Equ { symbol, value }
        , label = D.Label
        , ds = λ(size : Size) → λ(value : Integer) → D.Ds { size, value }
        , dc = λ(size : Size) → λ(value : Integer) → D.Dc { size, value }
        , ld = arch.ld
        , ldl = arch.ldl
        , st = arch.st
        , mv = arch.mv
        , add = arch.add
        , sub = arch.sub
        , mul = arch.mul
        , smul = arch.smul
        , div = arch.div
        , shr = arch.shr
        , sshr = arch.sshr
        , shl = arch.shl
        , and = arch.and
        , or = arch.or
        , xor = arch.xor
        , cmp = arch.cmp
        , b = arch.b
        , call = arch.call
        , return = arch.return
        , syscal = arch.syscall
        }

in  { arch, file, x86_64, mkPrelude }
