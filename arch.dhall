let Reg =
    -- general purpose registers
      < Gp : Natural | InstrPtr | StackPtr | FramePtr >

let Val = < Reg : Reg | Imm : Natural >

let Mem = { base : Val, offset : Natural }

let Size = < _1 | _2 | _4 | _8 >

let BranchTarget = < Mem : Mem | Label : Text >

let Cond = < Eq | Neq | Lt | Lte | Gt | Gte >

let Arch =
      λ(Directive : Type) →
        { pointerSize : Size
        , ld : Size → Reg → Mem → Directive
        , ldl : Reg → Text → Directive
        , st : Size → Mem → Val → Directive
        , mv : Size → Reg → Val → Directive
        , add : Size → Reg → Val → Val → Directive
        , sub : Size → Reg → Val → Val → Directive
        , mul : Size → Reg → Val → Val → Directive
        , smul : Size → Reg → Val → Val → Directive
        , div : Size → Reg → Val → Val → Directive
        , sdiv : Size → Reg → Val → Val → Directive
        , shr : Size → Reg → Val → Val → Directive
        , sshr : Size → Reg → Val → Val → Directive
        , shl : Size → Reg → Val → Val → Directive
        , and : Size → Reg → Val → Val → Directive
        , or : Size → Reg → Val → Val → Directive
        , xor : Size → Reg → Val → Val → Directive
        , cmp : Size → Val → Val → Directive
        , b : Cond → BranchTarget → Directive
        , call : BranchTarget → Directive
        , return : Directive
        , syscall : Directive
        }

in  { Reg
    , Val
    , Mem
    , Size
    , BranchTarget
    , Cond
    , Arch
    , error =
        λ(msg : Text) →
          ''

          .error ${msg}
          ''
    }
