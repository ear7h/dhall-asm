let dasm = ../package.dhall
let dasm = dasm.mkPrelude dasm.x86_64
in dasm.writeFile
  [ dasm.global "main"
  , dasm.text
  , dasm.label "main"
  , dasm.ldl (dasm.Reg.Gp 4) "hello_string"
  , dasm.call (dasm.BranchTarget.Label "puts")
  , dasm.mv dasm.Size._8 (dasm.Reg.Gp 4) (dasm.Val.Imm 42)
  , dasm.call (dasm.BranchTarget.Label "exit")
  , dasm.label "hello_string"
  , dasm.asciz "Hello DhallASM"
  ]
