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
      exprType = resultTypes.size match
        case 0 => NoneType
        case 1 => resultTypes.head.valtype
        case _ => MultiValueType(resultTypes.map(_.valtype))
    )

  def `if`(
      condition: FoldedInstr,
      ifTrue: FoldedInstr,
      ifFalse: Opt[FoldedInstr]
  ): FoldedInstr =
    // TODO(Derppening): Add support for subtyping relation between value of ifTrue/ifFalse
    val resultType =
      (condition.exprType, ifTrue.exprType, ifFalse.map(_.exprType)) match
        case (UnreachableType, _, _) => UnreachableType
        case (_, thenTy, S(elseTy)) if thenTy eq elseTy => thenTy
        case (_, thenTy, S(UnreachableType)) => thenTy
        case (_, UnreachableType, S(elseTy)) => elseTy
        case _ => NoneType

    val thenInstr = FoldedInstr(
      mnemonic = "then",
      instrargs = Seq.empty,
      stackargs = Seq(S(ifTrue)),
      exprType = ifTrue.exprType
    )
    val elseInstr = ifFalse.map: elseExpr =>
      FoldedInstr(
        mnemonic = "else",
        instrargs = Seq.empty,
        stackargs = Seq(S(elseExpr)),
        exprType = elseExpr.exprType
      )

    FoldedInstr(
      mnemonic = "if",
      instrargs = resultType.toSeq.map(SignatureType(NoneType, _).toWat),
      stackargs = Seq(S(condition), S(thenInstr)) ++ elseInstr.map(S(_)).toSeq,
      exprType = resultType
    )

  def call(
    funcidx: FuncIdx,
    operands: Seq[FoldedInstr],
    returnTypes: Seq[Result]
  ): FoldedInstr = FoldedInstr(
    mnemonic = "call",
    instrargs = Seq(funcidx.toWat),
    stackargs = operands.map(S(_)),
    exprType = returnTypes.size match
      case 0 => NoneType
      case 1 => returnTypes.head.valtype
      case _ => MultiValueType(returnTypes.map(_.valtype))
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
    exprType = funcType.sigType.results match
      case Seq() => NoneType
      case ty +: Seq() => ty.valtype
      case tys => MultiValueType(tys.map(_.valtype))
  )

  def nop: FoldedInstr = FoldedInstr(
    mnemonic = "nop",
    instrargs = Seq.empty,
    stackargs = Seq.empty,
    exprType = NoneType
  )

  def `return`(value: Opt[FoldedInstr]): FoldedInstr = FoldedInstr(
    mnemonic = "return",
    instrargs = Seq.empty,
    stackargs = Seq(value),
    exprType = value.fold(NoneType)(_.exprType)
  )

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
      exprType = (lhs.exprType, rhs.exprType) match
        case (UnreachableType, _) | (_, UnreachableType) => UnreachableType
        case _ => I32Type
    )
  end i32

  object ref:
    def func(idx: FuncIdx, ty: RefType): FoldedInstr = FoldedInstr(
      mnemonic = "ref.func",
      instrargs = Seq(idx.toWat),
      stackargs = Seq.empty,
      ty
    )

    def i31(value: FoldedInstr): FoldedInstr = FoldedInstr(
      mnemonic = "ref.i31",
      instrargs = Seq.empty,
      stackargs = Seq(S(value)),
      exprType =
        if value.exprType is UnreachableType then UnreachableType
        else RefType.i31ref
    )

    def test(value: FoldedInstr, castType: RefType): FoldedInstr = FoldedInstr(
      mnemonic = "ref.test",
      instrargs = Seq(castType.toWat),
      stackargs = Seq(S(value)),
      exprType =
        if value.exprType is UnreachableType then UnreachableType else I32Type
    )

    def cast(value: FoldedInstr, castType: RefType): FoldedInstr = FoldedInstr(
      mnemonic = "ref.cast",
      instrargs = Seq(castType.toWat),
      stackargs = Seq(S(value)),
      exprType =
        if value.exprType is UnreachableType then UnreachableType else castType
    )
  end ref

  object i31:
    def get(i31: FoldedInstr, signed: Bool): FoldedInstr = FoldedInstr(
      mnemonic = s"i31.get_${if signed then 's' else 'u'}",
      instrargs = Seq.empty,
      stackargs = Seq(S(i31)),
      exprType =
        if i31.exprType is UnreachableType then UnreachableType else I32Type
    )

    def get_s(i31: FoldedInstr): FoldedInstr = get(i31, true)
  end i31

  object local:
    def get(index: LocalIdx, ty: WasmType): FoldedInstr = FoldedInstr(
      mnemonic = "local.get",
      instrargs = Seq(index),
      stackargs = Seq.empty,
      exprType = ty
    )

    def set(index: LocalIdx, value: FoldedInstr): FoldedInstr = FoldedInstr(
      mnemonic = "local.set",
      instrargs = Seq(index),
      stackargs = Seq(S(value)),
      exprType =
        if value.exprType is UnreachableType then UnreachableType else NoneType
    )
  end local

  object struct:
    def new_default(ty: TypeIdx): FoldedInstr = FoldedInstr(
      mnemonic = "struct.new_default",
      instrargs = Seq(ty.toWat),
      stackargs = Seq.empty,
      exprType = RefType(ty, nullable = false)
    )
  end struct

end Instructions
