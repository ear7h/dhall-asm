let Prelude = ./prelude.dhall
let List/index = Prelude.List.index
let arch = ./arch.dhall
let Size = arch.Size
let Reg = arch.Reg
let Mem = arch.Mem
let Val = arch.Val
let Cond = arch.Cond
let BranchTarget = arch.BranchTarget

let error = arch.error

let fmtReg = λ(size : Size) → λ(reg : Reg) →
  let RegRow = { _1 : Text, _2 : Text, _4 : Text, _8 : Text }

  let gpRegs =
    -- ax
    -- [ { _1 = "al", _2 = "ax", _4 = "eax", _8 = "rax" }
    -- NOTE: rax is used as a scratch register to satisfy the
    -- Arch interface
    [ { _1 = "bl", _2 = "bx", _4 = "ebx", _8 = "rbx" }
    , { _1 = "cl", _2 = "cx", _4 = "ecx", _8 = "rcx" }
    , { _1 = "dl", _2 = "dx", _4 = "edx", _8 = "rdx" }
    , { _1 = "sil", _2 = "si", _4 = "esi", _8 = "rsi" }
    , { _1 = "dil", _2 = "di", _4 = "dsi", _8 = "rdi" }
    , { _1 = "r8b", _2 = "r8w", _4 = "r8d", _8 = "r8" }
    , { _1 = "r9b", _2 = "r9w", _4 = "r9d", _8 = "r9" }
    , { _1 = "r10b", _2 = "r10w", _4 = "r10d", _8 = "r10" }
    , { _1 = "r11b", _2 = "r11w", _4 = "r11d", _8 = "r11" }
    , { _1 = "r12b", _2 = "r12w", _4 = "r12d", _8 = "r12" }
    , { _1 = "r13b", _2 = "r13w", _4 = "r13d", _8 = "r13" }
    , { _1 = "r14b", _2 = "r14w", _4 = "r14d", _8 = "r14" }
    , { _1 = "r15b", _2 = "r15w", _4 = "r15d", _8 = "r15" }
    ]

  let instrPtr =
    { _1 = error "can't access instrucion pointer as byte"
    , _2 = "ip"
    , _4 = "eip"
    , _8 = "rip"
    }

  let stackPtr = { _1 = "spl", _2 = "sp", _4 = "esp", _8 = "rsp" }

  let framePtr = { _1 = "bpl", _2 = "bp", _4 = "ebp", _8 = "rbp" }
    in merge
      (merge
        { Gp = λ(n : Natural) →
          merge
            { Some = λ(row : RegRow) → row
            , None =
              let err =
                error "register ${Natural/show n} not supported"
              in  { _1 = err, _2 = err, _4 = err, _8 = err }
            }
            (List/index n RegRow gpRegs)
        , InstrPtr = instrPtr
        , StackPtr = stackPtr
        , FramePtr = framePtr
        }
        reg
      )
      size

-- shortcut for arith
let rax = λ(size : Size) →
  merge { _1 = "al", _2 = "ax", _4 = "eax", _8 = "rax" } size

-- shortcut for shifts
let rcx = λ(size : Size) →
  merge { _1 = "cl", _2 = "cx", _4 = "ecx", _8 = "rcx" } size

let fmtVal = λ(size : Size) → λ(val : Val) →
  merge { Reg = fmtReg size, Imm = Natural/show } val

let fmtMem = λ(size : Size) → λ(mem : Mem) →
  let prefix =
    merge
      { _1 = "byte", _2 = "word", _4 = "dword", _8 = "qword" }
      size
  in  "${prefix} ptr [${fmtVal size mem.base} + ${Natural/show mem.offset}]"

let condSuffix = λ(cond : Cond) →
  merge
    { Eq  = "e"
    , Neq = "ne"
    , Lt  = "l"
    , Lte = "le"
    , Gt  = "g"
    , Gte = "ge"
    }
    cond

let fmtTarget = λ(target : BranchTarget) →
  merge
    { Mem = fmtMem Size._8
    , Label = λ(s : Text) → s
    }
    target

in  λ(OutText : Type) → λ(conv : Text → OutText) →
  { pointerSize = Size._8
  , ld = λ(size : Size) → λ(reg : Reg) → λ(mem : Mem) →
    conv "mov ${fmtReg size reg}, ${fmtMem size mem} "
  , ldl = λ(reg : Reg) → λ(label : Text) →
        conv "lea ${fmtReg Size._8 reg}, ${label}"

  , st = λ(size : Size) → λ(mem : Mem) → λ(val : Val) →
        conv "mov ${fmtMem size mem}, ${fmtVal size val}"

  , mv = λ(size : Size) → λ(reg : Reg) → λ(val : Val) →
        conv "mov ${fmtReg size reg}, ${fmtVal size val}"

  , add = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${fmtReg size reg}, ${fmtVal size x}
      add ${fmtReg size reg}, ${fmtVal size y}''

  , sub = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${fmtReg size reg}, ${fmtVal size x}
      sub ${fmtReg size reg}, ${fmtVal size y}''

  , mul = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${rax size} ${fmtVal size x}
      mul ${fmtVal size y}
      mov ${fmtReg size reg}''

  , smul = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${rax size} ${fmtVal size x}
      imul ${fmtVal size y}
      mov ${fmtReg size reg}''

  , div = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${rax size} ${fmtVal size x}
      div ${fmtVal size y}
      mov ${fmtReg size reg}''

  , sdiv = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${rax size} ${fmtVal size x}
      idiv ${fmtVal size y}
      mov ${fmtReg size reg}''

  , shr = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${rax Size._8} ${rcx Size._8}
      mov ${rcx Size._8} ${fmtVal Size._8 y}
      mov ${fmtReg Size._8 reg} ${fmtVal Size._8 x}
      shr ${fmtReg size reg}
      mov ${rcx Size._8} ${rax Size._8}''

  , sshr = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${rax Size._8} ${rcx Size._8}
      mov ${rcx Size._8} ${fmtVal Size._8 y}
      mov ${fmtReg Size._8 reg} ${fmtVal Size._8 x}
      sar ${fmtReg size reg}
      mov ${rcx Size._8} ${rax Size._8}''

  , shl = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${rax Size._8} ${rcx Size._8}
      mov ${rcx Size._8} ${fmtVal Size._8 y}
      mov ${fmtReg Size._8 reg} ${fmtVal Size._8 x}
      shl ${fmtReg size reg}
      mov ${rcx Size._8} ${rax Size._8}''

  , and = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${fmtReg size reg}, ${fmtVal size x}
      and ${fmtReg size reg}, ${fmtVal size y}''

  , or = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${fmtReg size reg}, ${fmtVal size x}
      or ${fmtReg size reg}, ${fmtVal size y}''

  , xor = λ(size : Size) → λ(reg : Reg) → λ(x : Val) → λ(y : Val) →
    conv
      ''
      mov ${fmtReg size reg}, ${fmtVal size x}
      xor ${fmtReg size reg}, ${fmtVal size y}''

  , cmp = λ(size : Size) → λ(x : Val) → λ(y : Val) →
    conv "cmp ${fmtVal size x}, ${fmtVal size y}"

  , b = λ(cond : Cond) → λ(target : BranchTarget) →
    conv "j${condSuffix cond} ${fmtTarget target}"

  , call = λ(target : BranchTarget) →
    conv "call ${fmtTarget target}"

  , return = conv "return"
  , syscall = conv "syscall"
  }
