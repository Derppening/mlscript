package hkmc2.codegen.wasm.text

import mlscript.utils.*, shorthands.*

object Instructions:
  def unreachable: FoldedInstr = FoldedInstr(
    mnemonic = "unreachable",
    instrargs = Seq.empty,
    stackargs = Seq.empty,
    exprType = UnreachableType
  )

  object i32:
    def const(value: Int): FoldedInstr = FoldedInstr(
      mnemonic = "i32.const",
      instrargs = Seq(value),
      stackargs = Seq.empty,
      exprType = I32Type
    )
  end i32

  object ref:
    def i31(value: FoldedInstr): FoldedInstr = FoldedInstr(
      mnemonic = "ref.i31",
      instrargs = Seq.empty,
      stackargs = Seq(S(value)),
      exprType = RefType.i31ref
    )
  end ref

end Instructions
