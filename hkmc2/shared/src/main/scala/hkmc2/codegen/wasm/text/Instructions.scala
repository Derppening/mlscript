package hkmc2
package codegen.wasm.text

import mlscript.utils.*, shorthands.*

import document.*

object Instructions:
  def block(
      label: Opt[Str],
      children: Seq[FoldedInstr],
      resultTypes: Seq[Result]
  ): FoldedInstr =
    val labelWat = label.map(lbl => doc"$$$lbl")

    FoldedInstr(
      mnemonic = "block",
      instrargs =
        labelWat.toSeq ++ resultTypes.map(resTy => SignatureType(NoneType, resTy.valtype)),
      stackargs = children.map(S(_)),
      resultTypes = resultTypes.map(_.valtype)
    )

  def `if`(
      condition: FoldedInstr,
      ifTrue: FoldedInstr,
      ifFalse: Opt[FoldedInstr]
  ): FoldedInstr =
    // TODO(Derppening): Add support for subtyping relation between value of ifTrue/ifFalse
    val resultTypes =
      (condition.resultType_!, ifTrue.resultTypes, ifFalse.map(_.resultTypes)) match
        case (S(UnreachableType), _, _) => Seq(UnreachableType)
        case (_, thenTy, elseTy) if thenTy == elseTy => thenTy
        case (_, thenTy, S(Seq(UnreachableType))) => thenTy
        case (_, UnreachableType, S(elseTy)) => elseTy
        case _ => Seq.empty

    val thenInstr = FoldedInstr(
      mnemonic = "then",
      instrargs = Seq.empty,
      stackargs = Seq(S(ifTrue)),
      resultTypes = ifTrue.resultTypes
    )
    val elseInstr = ifFalse.map: elseExpr =>
      FoldedInstr(
        mnemonic = "else",
        instrargs = Seq.empty,
        stackargs = Seq(S(elseExpr)),
        resultTypes = elseExpr.resultTypes
      )

    FoldedInstr(
      mnemonic = "if",
      instrargs = resultTypes.map(SignatureType(NoneType, _).toWat),
      stackargs = Seq(S(condition), S(thenInstr)) ++ elseInstr.map(S(_)).toSeq,
      resultTypes
    )

  def call(
      funcidx: FuncIdx,
      operands: Seq[FoldedInstr],
      returnTypes: Seq[Result]
  ): FoldedInstr = FoldedInstr(
    mnemonic = "call",
    instrargs = Seq(funcidx.toWat),
    stackargs = operands.map(S(_)),
    resultTypes = returnTypes.map(_.valtype)
  )

  def call_ref(
      target: FoldedInstr,
      operands: Seq[FoldedInstr],
      typeIdx: TypeIdx,
      funcType: FunctionType
  ): FoldedInstr = FoldedInstr(
    mnemonic = "call_ref",
    instrargs = Seq(typeIdx.toWat),
    stackargs = operands.map(S(_)) :+ S(target),
    resultTypes = funcType.sigType.results.map(_.valtype)
  )

  def nop: FoldedInstr = FoldedInstr(
    mnemonic = "nop",
    instrargs = Seq.empty,
    stackargs = Seq.empty,
    resultType = None
  )

  def `return`(value: Opt[FoldedInstr]): FoldedInstr = FoldedInstr(
    mnemonic = "return",
    instrargs = Seq.empty,
    stackargs = Seq(value),
    resultTypes = value.fold(Seq.empty)(_.resultTypes)
  )

  def unreachable: FoldedInstr = FoldedInstr(
    mnemonic = "unreachable",
    instrargs = Seq.empty,
    stackargs = Seq.empty,
    resultType = S(UnreachableType)
  )

  object i32:
    def const(value: Int): FoldedInstr = FoldedInstr(
      mnemonic = "i32.const",
      instrargs = Seq(doc"$value"),
      stackargs = Seq.empty,
      resultType = S(I32Type)
    )

    def add(lhs: FoldedInstr, rhs: FoldedInstr): FoldedInstr = FoldedInstr(
      mnemonic = "i32.add",
      instrargs = Seq.empty,
      stackargs = Seq(lhs, rhs),
      resultType = S(
        (lhs.exprType, rhs.exprType) match
          case (UnreachableType, _) | (_, UnreachableType) => UnreachableType
          case _ => I32Type
      )
    )
  end i32

  object ref:
    def func(idx: FuncIdx, ty: RefType): FoldedInstr = FoldedInstr(
      mnemonic = "ref.func",
      instrargs = Seq(idx.toWat),
      stackargs = Seq.empty,
      resultType = S(ty)
    )

    def i31(value: FoldedInstr): FoldedInstr = FoldedInstr(
      mnemonic = "ref.i31",
      instrargs = Seq.empty,
      stackargs = Seq(value),
      resultType = S(if value.exprType is UnreachableType then UnreachableType else RefType.i31ref)
    )

    def test(value: FoldedInstr, castType: RefType): FoldedInstr = FoldedInstr(
      mnemonic = "ref.test",
      instrargs = Seq(castType.toWat),
      stackargs = Seq(value),
      resultType = S(if value.exprType is UnreachableType then UnreachableType else I32Type)
    )

    def cast(value: FoldedInstr, castType: RefType): FoldedInstr = FoldedInstr(
      mnemonic = "ref.cast",
      instrargs = Seq(castType.toWat),
      stackargs = Seq(value),
      resultType = S(if value.exprType is UnreachableType then UnreachableType else castType)
    )
  end ref

  object i31:
    def get(i31: FoldedInstr, signed: Bool): FoldedInstr = FoldedInstr(
      mnemonic = s"i31.get_${if signed then 's' else 'u'}",
      instrargs = Seq.empty,
      stackargs = Seq(i31),
      resultType = S(if i31.exprType is UnreachableType then UnreachableType else I32Type)
    )

    def get_s(i31: FoldedInstr): FoldedInstr = get(i31, true)
  end i31

  object local:
    def get(index: LocalIdx, ty: WasmType): FoldedInstr = FoldedInstr(
      mnemonic = "local.get",
      instrargs = Seq(index),
      stackargs = Seq.empty,
      resultType = S(ty)
    )

    def set(index: LocalIdx, value: FoldedInstr): FoldedInstr = FoldedInstr(
      mnemonic = "local.set",
      instrargs = Seq(index),
      stackargs = Seq(value),
      resultType = if value.exprType is UnreachableType then S(UnreachableType) else N
    )
  end local

  object struct:
    def new_default(ty: TypeIdx): FoldedInstr = FoldedInstr(
      mnemonic = "struct.new_default",
      instrargs = Seq(ty.toWat),
      stackargs = Seq.empty,
      resultType = S(RefType(ty, nullable = false))
    )
  end struct

end Instructions
