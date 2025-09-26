package hkmc2
package codegen.wasm.text

import mlscript.utils.*, shorthands.*

import document.*

object Instructions:
  def block(
      label: Opt[Str],
      children: Seq[Expr],
      resultTypes: Seq[Result]
  ): FoldedInstr =
    val labelWat = label.map(lbl => doc"$$$lbl")

    FoldedInstr(
      mnemonic = "block",
      instrargs =
        labelWat.toSeq ++ resultTypes.map(resTy =>
          SignatureType(params = Seq.empty, results = Seq(resTy))
        ),
      stackargs = children,
      resultTypes = resultTypes.map(_.valtype)
    )

  def `if`(
      condition: Expr,
      ifTrue: Expr,
      ifFalse: Opt[Expr]
  ): FoldedInstr =
    // TODO(Derppening): Add support for subtyping relation between value of ifTrue/ifFalse
    // TODO(Derppening): Stop propagation of UnreachableType
    val resultTypes =
      (condition.resultType, ifTrue.resultTypes, ifFalse.map(_.resultTypes)) match
        case (S(UnreachableType), _, _) => Seq(UnreachableType)
        case (_, thenTy, elseTy) if thenTy == elseTy => thenTy
        case (_, thenTy, S(Seq(UnreachableType))) => thenTy
        case (_, UnreachableType, S(elseTy)) => elseTy
        case _ => Seq.empty

    val thenInstr = FoldedInstr(
      mnemonic = "then",
      instrargs = Seq.empty,
      stackargs = Seq(ifTrue),
      resultTypes = ifTrue.resultTypes
    )
    val elseInstr = ifFalse.map: elseExpr =>
      FoldedInstr(
        mnemonic = "else",
        instrargs = Seq.empty,
        stackargs = Seq(elseExpr),
        resultTypes = elseExpr.resultTypes
      )

    FoldedInstr(
      mnemonic = "if",
      instrargs = resultTypes.map(resTy =>
        SignatureType(params = Seq.empty, results = Seq(Result(resTy.asValType_!))).toWat
      ),
      stackargs = Seq(condition, thenInstr) ++ elseInstr.toSeq,
      resultTypes
    )

  def call(
      funcidx: FuncIdx,
      operands: Seq[Expr],
      returnTypes: Seq[Result]
  ): FoldedInstr =
    FoldedInstr(
      mnemonic = "call",
      instrargs = Seq(funcidx.toWat),
      stackargs = operands,
      resultTypes = returnTypes.map(_.valtype)
    )

  def call_ref(
      target: Expr,
      operands: Seq[Expr],
      typeIdx: TypeIdx,
      funcType: FunctionType
  ): FoldedInstr = FoldedInstr(
    mnemonic = "call_ref",
    instrargs = Seq(typeIdx.toWat),
    stackargs = operands :+ target,
    resultTypes = funcType.sigType.results.map(_.valtype)
  )

  def nop: FoldedInstr = FoldedInstr(
    mnemonic = "nop",
    instrargs = Seq.empty,
    stackargs = Seq.empty,
    resultType = N
  )

  def `return`(value: Opt[Expr]): FoldedInstr = FoldedInstr(
    mnemonic = "return",
    instrargs = Seq.empty,
    stackargs = value.toSeq,
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

    def add(lhs: Expr, rhs: Expr): FoldedInstr = FoldedInstr(
      mnemonic = "i32.add",
      instrargs = Seq.empty,
      stackargs = Seq(lhs, rhs),
      resultType = S(
        (lhs.resultType, rhs.resultType) match
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

    def i31(value: Expr): FoldedInstr = FoldedInstr(
      mnemonic = "ref.i31",
      instrargs = Seq.empty,
      stackargs = Seq(value),
      resultType =
        S(if value.resultType is UnreachableType then UnreachableType else RefType.i31ref)
    )

    def test(value: Expr, castType: RefType): FoldedInstr = FoldedInstr(
      mnemonic = "ref.test",
      instrargs = Seq(castType.toWat),
      stackargs = Seq(value),
      resultType = S(if value.resultType is UnreachableType then UnreachableType else I32Type)
    )

    def cast(value: Expr, castType: RefType): FoldedInstr = FoldedInstr(
      mnemonic = "ref.cast",
      instrargs = Seq(castType.toWat),
      stackargs = Seq(value),
      resultType = S(if value.resultType is UnreachableType then UnreachableType else castType)
    )
  end ref

  object i31:
    def get(i31: Expr, signed: Bool): FoldedInstr = FoldedInstr(
      mnemonic = s"i31.get_${if signed then 's' else 'u'}",
      instrargs = Seq.empty,
      stackargs = Seq(i31),
      resultType = S(if i31.resultType is UnreachableType then UnreachableType else I32Type)
    )

    def get_s(i31: Expr): FoldedInstr = get(i31, true)
  end i31

  object local:
    def get(index: LocalIdx, ty: WasmType): FoldedInstr = FoldedInstr(
      mnemonic = "local.get",
      instrargs = Seq(index),
      stackargs = Seq.empty,
      resultType = S(ty)
    )

    def set(index: LocalIdx, value: Expr): FoldedInstr = FoldedInstr(
      mnemonic = "local.set",
      instrargs = Seq(index),
      stackargs = Seq(value),
      resultType = if value.resultType is UnreachableType then S(UnreachableType) else N
    )
  end local

  object struct:
    def new_default(ty: TypeIdx): FoldedInstr = FoldedInstr(
      mnemonic = "struct.new_default",
      instrargs = Seq(ty.toWat),
      stackargs = Seq.empty,
      resultType = S(RefType(ty, nullable = false))
    )

    def set(index: FieldIdx, ref: Expr, value: FoldedInstr): FoldedInstr = FoldedInstr(
      mnemonic = "struct.set",
      instrargs = Seq(ref.resultType.get.asInstanceOf[RefType].heapType, index),
      stackargs = Seq(ref, value),
      resultType = if value.resultType is UnreachableType then S(UnreachableType) else N
    )

    def get(index: FieldIdx, ref: Expr, ty: WasmType): FoldedInstr = FoldedInstr(
      mnemonic = "struct.get",
      instrargs = Seq(ref.resultType.get.asInstanceOf[RefType].heapType, index),
      stackargs = Seq(ref),
      resultType = S(if ref.resultType is UnreachableType then UnreachableType else ty)
    )

  end struct

end Instructions
