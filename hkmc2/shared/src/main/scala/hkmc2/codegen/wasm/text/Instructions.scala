package hkmc2.codegen.wasm.text

object Instructions:
  def unreachable: FoldedInstr = FoldedInstr(
    mnemonic = "unreachable",
    instrargs = Seq.empty,
    stackargs = Seq.empty,
    exprType = UnreachableType
  )

end Instructions
