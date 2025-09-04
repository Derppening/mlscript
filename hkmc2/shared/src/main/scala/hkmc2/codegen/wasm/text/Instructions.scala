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

    def add(lhs: FoldedInstr, rhs: FoldedInstr): FoldedInstr = FoldedInstr(
      mnemonic = "i32.add",
      instrargs = Seq.empty,
      stackargs = Seq(S(lhs), S(rhs)),
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

    def test(
        value: FoldedInstr,
        castType: RefType
    ): FoldedInstr = FoldedInstr(
      mnemonic = "ref.test",
      instrargs = Seq(castType.toWat),
      stackargs = Seq(S(value)),
      exprType = I32Type
    )
  end ref

  object i31ref:
    def get(i31: FoldedInstr, signed: Bool): FoldedInstr = FoldedInstr(
      mnemonic = s"i31ref.get_${if signed then 's' else 'u'}",
      instrargs = Seq.empty,
      stackargs = Seq(S(i31)),
      exprType = I32Type
    )
  end i31ref

end Instructions
