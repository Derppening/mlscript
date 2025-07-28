package hkmc2
package codegen
package wasm
package text

import mlscript.utils.*, shorthands.*

import document.*
import semantics.*
import syntax.Tree.{BoolLit, IntLit, UnitLit}
import wasm.Module as WasmModule
import Message.MessageContext

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable

/**
 * A reference to an `export` field in a module.
 *
 * @param mod
 *   The module that contains the export.
 * @param intName
 *   The internal name of the export.
 */
case class ExportRef(mod: ModuleProxy, intName: Str) extends Export[ExportRef]

/**
 * A reference to an expression.
 *
 * @param inner
 *   The [[Expr]] that this proxy represents.
 */
class ExprProxy(val inner: Expr) extends Expression[ExprProxy]:
  /** Whether this expression consists of exactly zero instructions. */
  def isEmpty: Boolean = inner match
    case stackInstr: Ls[StackInstr] => stackInstr.isEmpty
    case foldedInstr: Opt[FoldedInstr] => foldedInstr.isEmpty

  /** See [[isEmpty]]. */
  def nonEmpty: Boolean = !isEmpty

  /** Returns the type of this expression. */
  def getType: WasmType =
    val instrType = inner match
      case stackInstr: Ls[StackInstr] => stackInstr.lastOption.map(_.exprType)
      case foldedInstr: Opt[FoldedInstr] => foldedInstr.map(_.exprType)
    instrType.getOrElse(NoneType)

  /**
   * Returns the type of this expression, converted into a WAT-compatible type
   * if needed.
   *
   * @param expectsValue
   *   Whether this expression is in a context where a value is expected to be
   *   generated.
   */
  def getWasmType(expectsValue: Bool): WasmType = getType match
    case UnreachableType =>
      if expectsValue then RefType(HeapType.Any, nullable = true) else NoneType
    case ty => ty

  /**
   * Converts the inner expression into a [[List]] of
   * [[StackInstr stack instructions]].
   */
  def toStack: ExprProxy = inner match
    case _: Ls[StackInstr] => this
    case foldedInstr: Opt[FoldedInstr] =>
      ExprProxy(foldedInstr.map(_.toStack).getOrElse(Ls()))

  def fmtDoc: Document = inner match
    case stackInstr: Ls[StackInstr] =>
      stackInstr.map(_.fmtDoc).mkDocument(" # ")
    case foldedInstr: Opt[FoldedInstr] => foldedInstr.dlof(_.fmtDoc)(doc"")
end ExprProxy

/**
 * A reference to a `func` field in a module.
 *
 * @param mod
 *   The module that contains the function.
 * @param name
 *   The name of the function.
 */
case class FuncRef(mod: ModuleProxy, name: Str) extends Function[FuncRef]:
  type Expr = ExprProxy
end FuncRef

/**
 * A structure containing function information.
 *
 * @param name
 *   The name of the function.
 * @param results
 *   The result type of the function.
 */
case class FunctionInfo(
    name: Str,
    params: WasmType,
    results: WasmType
) extends wasm.FunctionInfo[WasmType]

/**
 * A reference to a `global` field in a module.
 *
 * @param mod
 *   The module that contains the global.
 * @param name
 *   The name of the global.
 */
class GlobalRef(mod: ModuleProxy, name: Str) extends Global[GlobalRef]

/** A builder for creating heap types. */
class TypeBuilder(private val gen: WatBackend, size: Int)
    extends wasm.TypeBuilder[WasmType, WasmPackedType]:
  private val entries = mutable.ArrayBuffer[HeapType]()
  entries.sizeHint(size)

  /**
   * Ensures that the `entries` buffer has at least `index` number of entries.
   */
  private def ensureFieldSize(index: Int) =
    // Pad `entries` until we have the correct number of elements
    entries ++= Seq.fill((index - entries.size + 1) max 0)(null)

  def setSignatureType(
      index: Int,
      paramTypes: WasmType,
      resultTypes: WasmType
  ): Unit =
    ensureFieldSize(index)
    entries(index) = SignatureType(paramTypes, resultTypes)

  def setStructType(
      index: Int,
      fields: Seq[(WasmType | WasmPackedType, Bool)]
  ): Unit =
    ensureFieldSize(index)
    entries(index) = StructType(
      fields.map: (ty, mut) =>
        ty match
          case packedTy: WasmPackedType => Field(packedTy, mut)
          case ty: WasmType => Field(ty, mut)
    )

  def build(): WasmType =
    gen.createType(entries.map(entry => RefType(entry, false)).toSeq)
end TypeBuilder

/**
 * A reference to a WebAssembly module.
 *
 * @param gen
 *   The [[WatBackend]] that generates constructs for this module.
 * @param mod
 *   The underlying [[wasm.Module]] that this proxy represents.
 */
class ModuleProxy(private val gen: WatBackend, private var mod: Module)
    extends WasmModule[WasmType, ExprProxy]:

  /** Monotonically increasing counter for giving unique names to types. */
  private val anonTypeCounter = AtomicLong()

  /**
   * Adds a type to this module.
   *
   * @param name
   *   The name of the type, or [[None]] if a type name should be generated.
   * @param tyDoc
   *   The document representing the type specification.
   */
  private def addType(name: Opt[Str], tyDoc: Document): Str =
    assume(
      name.forall(name => !mod.ty.exists((nm, _) => nm == name)),
      s"Type `$name` already exists"
    )

    val intName = name.getOrElse:
      s"_${anonTypeCounter.getAndIncrement()}"

    mod = mod.copy(ty = mod.ty :+ (intName -> doc"(type $$$intName $tyDoc)"))
    intName

  /**
   * Adds a function type to this module.
   *
   * @param name
   *   The name of the type, or [[None]] if a type name should be generated.
   * @param params
   *   The parameter types of the function.
   * @param results
   *   The result types of the function.
   */
  private def addFunctionType(
      name: Opt[Str],
      params: WasmType,
      results: WasmType
  ): Str = addType(name, gen.fmtFuncType(params, results))

  type Exprt = ExportRef
  type Func = FuncRef
  type FuncInfo = FunctionInfo
  type Glob = GlobalRef

  def addFunction(
      name: Str,
      params: WasmType,
      results: WasmType,
      vars: Seq[WasmType],
      body: ExprProxy
  ): Func =
    assume(
      !mod.fn.exists((nm, _) => nm == name),
      s"Function `$name` already exists"
    )

    val fnTypeStrIndex = addFunctionType(N, params, results)

    val fnDecl =
      doc"(func $$$name${gen.fmtFuncSig(params, results).optionUnless(
          _.isEmpty
        ).dlof(sig => doc" $sig ")(doc"")}${(vars
          .map(v => doc"(local ${gen.fmtType(v)})") :+ body.fmtDoc)
          .filterNot(_.isEmpty)
          .optionIf(_.nonEmpty)
          .dlof(docs => doc" #{  # ${docs.mkDocument(Document.forceBreak)}) #} ")(doc")")}"

    mod = mod.copy(
      fn = mod.fn :+ name -> ModFunc(fnTypeStrIndex, params, results, fnDecl),
      el = mod.el :+ name -> doc"(elem declare func $$$name)"
    )
    new Func(this, name)

  /**
   * Gets a function by name.
   *
   * Generates a [[NoSuchElementException]] if the function does not exist.
   */
  def getFunction(name: Str): Func =
    mod.fn.find(_._1 == name).map((nme, _) => new Func(this, nme)).get

  def removeFunction(name: Str): Unit =
    mod = mod.copy(fn = mod.fn.filterNot((nm, _) => nm == name))

  def addFunctionImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      params: WasmType,
      results: WasmType
  ): Unit =
    val funcImp =
      doc"(import \"$externalModuleName\" \"$externalBaseName\" (func $$$internalName${gen
          .expandType(params)
          .optionIf(_.nonEmpty)
          .dlof(_.map(p => doc"(param ${gen.fmtType(p)})").mkDocument(" ", " ", ""))(doc"")}${gen
          .expandType(results)
          .optionIf(_.nonEmpty)
          .dlof(_.map(r => doc"(result ${gen.fmtType(r)})").mkDocument(" ", " ", ""))(doc"")}))"

    mod = mod.copy(im = mod.im :+ internalName -> funcImp)

  def addTableImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit =
    val tableImp =
      doc"(import \"$externalModuleName\" \"$externalBaseName\" (table $$$internalName funcref))"

    mod = mod.copy(im = mod.im :+ internalName -> tableImp)

  def addMemoryImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit =
    val memImp =
      doc"(import \"$externalModuleName\" \"$externalBaseName\" (memory $$$internalName 0 65536))"

    mod = mod.copy(im = mod.im :+ internalName -> memImp)

  def addGlobalImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      globalType: WasmType
  ): Unit =
    val globalImp =
      doc"(import \"$externalModuleName\" \"$externalBaseName\" (global $$$internalName ${gen.fmtType(globalType)}))"

    mod = mod.copy(im = mod.im :+ internalName -> globalImp)

  def addFunctionExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    val funcExp = doc"""(export "$externalName" (func $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> funcExp)
    new Exprt(this, externalName)

  def addTableExport(internalName: Str, externalName: Str): Exprt =
    val tableExp = doc"""(export "$externalName" (table $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> tableExp)
    new Exprt(this, externalName)

  def addMemoryExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    val memoryExp = doc"""(export "$externalName" (memory $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> memoryExp)
    new Exprt(this, externalName)

  def addGlobalExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    val globalExp = doc"""(export "$externalName" (global $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> globalExp)
    new Exprt(this, externalName)

  def addGlobal(
      name: Str,
      ty: WasmType,
      mutable: Bool,
      value: ExprProxy
  ): Glob =
    val globalDecl = doc"(global $name ${
        if mutable then doc"(mut ${gen.fmtType(ty)})" else gen.fmtType(ty)
      } (${value.fmtDoc}))"

    mod = mod.copy(gl = mod.gl :+ name -> globalDecl)
    new Glob(this, name)

  def removeGlobal(name: Str): Unit =
    mod = mod.copy(gl = mod.gl.filterNot((nm, _) => nm == name))

    // TODO(Derppening): We probably will need to relax this to support the multiple
    //                   memories feature in Wasm...
  def setMemory(
      initial: Int,
      maximum: Int,
      exportName: Opt[Str],
      segments: Seq[MemorySegment[ExprProxy]],
      shared: Bool
  ): Unit =
    val memDecl =
      doc"(memory $$0 $initial $maximum${if shared then " shared" else ""})"

    mod = mod.copy(
      me = Seq("0" -> memDecl),
      da = segments.zipWithIndex.map: (segment, index) =>
        s"$index" -> s"(data $$$index${
            if segment.passive then doc"" else doc" ${segment.offset.fmtDoc}"
          } \"${segment.data.mkString}\")"
    )
    exportName.foreach:
      this.addMemoryExport("0", _)

  def setStart(start: Func): Unit =
    mod = mod.copy(st = S(start.name))

  def getFunctionInfo(ftype: Func): FuncInfo =
    val func = mod.fn.find(_._1 == ftype.name).map(_._2).get
    new FunctionInfo(
      name = func._1,
      params = func.paramTypes,
      results = func.resultTypes
    )

  def block(
      label: Opt[Str],
      children: Seq[ExprProxy],
      resultType: Opt[WasmType]
  ): ExprProxy =
    new ExprProxy(
      S(
        FoldedInstr(
          "block",
          label.map(label => s"$$$label").toSeq ++ resultType
            .map(gen.expandType(_))
            .map(_.map(resTy => s"(result ${gen.fmtType(resTy)})")),
          children.map(_.inner),
          resultType.getOrElse(NoneType)
        )
      )
    )

  def `if`(
      condition: ExprProxy,
      ifTrue: ExprProxy,
      ifFalse: Opt[ExprProxy]
  ): ExprProxy =
    // TODO(Derppening): Add support for `condition.getType is UnreachableType`
    // TODO(Derppening): Add support for subtyping relation between value of ifTrue/ifFalse
    val resultType = (ifTrue.getType, ifFalse.map(_.getType)) match
      case (thenTy, S(elseTy)) if thenTy eq elseTy => thenTy
      case (thenTy, S(UnreachableType)) => thenTy
      case (UnreachableType, S(elseTy)) => elseTy
      case _ => gen.none

    new ExprProxy(
      S(
        FoldedInstr(
          "if",
          if resultType eq gen.none then Seq()
          else Seq(s"(result ${gen.fmtType(resultType)})"),
          Seq(
            condition.inner,
            S(FoldedInstr("then", Seq(), Seq(ifTrue.inner), ifTrue.getType))
          ) ++
            ifFalse
              .map: iff =>
                S(FoldedInstr("else", Seq(), Seq(iff.inner), iff.getType))
              .toSeq,
          resultType
        )
      )
    )

  def nop(): ExprProxy =
    new ExprProxy(S(FoldedInstr("nop", Seq(), Seq(), NoneType)))

  def ret(value: Opt[ExprProxy]): ExprProxy =
    new ExprProxy(
      S(
        FoldedInstr(
          "return",
          Seq(),
          value.map(_.inner).toSeq,
          value.dlof(_.getType)(NoneType)
        )
      )
    )

  def unreachable(): ExprProxy =
    new ExprProxy(S(FoldedInstr("unreachable", Seq(), Seq(), UnreachableType)))

  def drop(value: ExprProxy): ExprProxy =
    new ExprProxy(S(FoldedInstr("drop", Seq(), Seq(value.inner), NoneType)))

  def call(
      name: Str,
      operands: Seq[ExprProxy],
      returnType: WasmType
  ): ExprProxy =
    new ExprProxy(
      S(FoldedInstr("call", Seq(s"$$$name"), operands.map(_.inner), returnType))
    )

  def callRef(
      target: ExprProxy,
      operands: Seq[ExprProxy],
      params: WasmType,
      results: WasmType
  ): ExprProxy =
    val fnTypeStrIndex = addFunctionType(N, params, results)
    new ExprProxy(
      S(
        FoldedInstr(
          "call_ref",
          Seq(s"$$$fnTypeStrIndex"),
          Seq(target.inner) ++ operands.map(_.inner),
          results
        )
      )
    )

  def i32 = new I32:
    def const(value: Int): ExprProxy =
      new ExprProxy(S(FoldedInstr("i32.const", Seq(s"$value"), Seq(), I32Type)))

    def add(left: ExprProxy, right: ExprProxy): ExprProxy =
      new ExprProxy(
        S(FoldedInstr("i32.add", Seq(), Seq(left.inner, right.inner), I32Type))
      )
  end i32

  def ref = new Ref:
    def func(name: Str, ty: WasmType): ExprProxy =
      // TODO(Derppening): See if need to convert `ty` into an exact type,
      //                   since the instruction's return type in Binaryen is
      //                   `(ref (exact $idx))`, but this appears to be a Wasm
      //                   proposal...
      require(ty.isInstanceOf[RefType])
      require(ty.asInstanceOf[RefType].heapType.isInstanceOf[SignatureType])
      new ExprProxy(
        S(FoldedInstr("ref.func", Seq(s"$$$name"), Seq(), ty))
      )

    def i31(value: ExprProxy): ExprProxy =
      new ExprProxy(
        S(FoldedInstr("ref.i31", Seq(), Seq(value.inner), gen.i31ref))
      )

    def cast(value: ExprProxy, castType: WasmType): ExprProxy =
      new ExprProxy(
        S(
          FoldedInstr(
            "ref.cast",
            Seq(gen.fmtType(castType)),
            Seq(value.inner),
            castType
          )
        )
      )
  end ref

  def i31ref = new I31Ref:
    def get(i31: ExprProxy, signed: Bool): ExprProxy =
      ExprProxy(
        S(
          FoldedInstr(
            s"i31.get_${if signed then 's' else 'u'}",
            Seq(),
            Seq(i31.inner),
            I32Type
          )
        )
      )
  end i31ref

  def emitText: Document = mod.emitText
end ModuleProxy

/** A [[WasmGenerator]] backend that produces text-based WAT as its output. */
class WatBackend
    extends WasmGenerator[
      WasmType,
      WasmPackedType,
      ModuleProxy,
      TypeBuilder,
      ExprProxy
    ]:
  type TypeRefs = Seq[WasmType]

  lazy val none: WasmType = NoneType
  lazy val i32: WasmType = I32Type
  lazy val i64: WasmType = I64Type
  lazy val f32: WasmType = F32Type
  lazy val f64: WasmType = F64Type
  lazy val v128: WasmType = V128Type
  lazy val funcref: WasmType = RefType(HeapType.Func, nullable = true)
  lazy val externref: WasmType = RefType(HeapType.Ext, nullable = true)
  lazy val anyref: WasmType = RefType(HeapType.Any, nullable = true)
  lazy val eqref: WasmType = RefType(HeapType.Eq, nullable = true)
  lazy val i31ref: WasmType = RefType(HeapType.I31, nullable = true)
  lazy val structref: WasmType = RefType(HeapType.Struct, nullable = true)
  lazy val stringref: WasmType = RefType(HeapType.String, nullable = true)
  lazy val unreachable: WasmType = UnreachableType
  lazy val notPacked: WasmPackedType = WasmPackedType.NotPacked
  lazy val i8: WasmPackedType = WasmPackedType.I8
  lazy val i16: WasmPackedType = WasmPackedType.I16

  def createType(types: TypeRefs): WasmType =
    types.size match
      case 0 => NoneType
      case 1 => types.head
      case _ => MultiValueType(types)
  def expandType(ty: WasmType): TypeRefs = ty match
    case MultiValueType(types) => types
    case NoneType => Seq()
    case _ => Seq(ty)

  def getExpressionType(expr: ExprProxy): WasmType = expr.getType
  def getExpressionWasmType(
      expr: ExprProxy,
      expectsValue: Bool
  ): WasmType = expr.getWasmType(expectsValue)

  /** Formats a [[HeapType]] into its text representation. */
  def fmtType(ty: HeapType): Document = ty match
    case HeapType.Func => doc"func"
    case HeapType.I31 => doc"i31"
    case _ => ???

  /** Formats a [[WasmType]] into its text representation. */
  def fmtType(ty: WasmType): Document = ty match
    case I32Type => doc"i32"
    case RefType(heapType, nullable) =>
      doc"(ref ${if nullable then "null " else ""}${fmtType(heapType)})"
    case _ => TODO(s"WatBackend::fmtType not implemented for type `$ty`")

  /**
   * Formats a function signature with the given [[params parameters]] and
   * [[results]] into its text representation.
   *
   * This function will only generate `(param ...)` and `(result ...)` clauses.
   * Use [[fmtFuncType]] to generate the function type.
   */
  def fmtFuncSig(params: WasmType, results: WasmType): Document =
    (expandType(params).map(p => doc"(param ${fmtType(p)})") ++
      expandType(results)
        .map(r => doc"(result ${fmtType(r)})")).mkDocument(" ")

  /**
   * Formats a function type with the given [[params parameters]] and
   * [[results]] into its text representation.
   *
   * This function will generate the full function type. Use [[fmtFuncSig]] to
   * only generate the parameter and result clauses.
   */
  def fmtFuncType(params: WasmType, results: WasmType): Document =
    doc"(func${fmtFuncSig(params, results)
        .optionUnless(_.isEmpty)
        .dlof(tyDoc => doc" $tyDoc")(doc"")})"

  def newModule: ModuleProxy = ModuleProxy(this, Module())

  def newTypeBuilder(size: Int): TypeBuilder = TypeBuilder(this, size)

  /* Functions taken from JSBuilder */

  def errExpr(errMsg: Message)(using ModuleProxy, Raise): ExprProxy =
    raise(
      ErrorReport(errMsg -> N :: Nil, source = Diagnostic.Source.Compilation)
    )
    summon[ModuleProxy].unreachable()

  def getVar(l: Local)(using ModuleProxy, Raise): ExprProxy =
    l match
      case ts: semantics.TermSymbol =>
        raise(
          WarningReport(
            msg"WasmBackend::getVar for ${ts.toString} not implemented yet" -> N :: Nil,
            source = Diagnostic.Source.Compilation
          )
        )
        summon[ModuleProxy].unreachable()
      case ts: semantics.InnerSymbol =>
        raise(
          WarningReport(
            msg"WasmBackend::getVar for ${ts.toString} not implemented yet" -> N :: Nil,
            source = Diagnostic.Source.Compilation
          )
        )
        summon[ModuleProxy].unreachable()
      case ts: semantics.BlockMemberSymbol if ts.isParameterizedMethod =>
        val mod = summon[ModuleProxy]
        val func = mod.getFunction(ts.nme)
        val funcInfo = mod.getFunctionInfo(func)
        val sigType = newTypeBuilder(1)
        sigType.setSignatureType(0, funcInfo.params, funcInfo.results)
        mod.ref.func(ts.nme, sigType.build())
      case l =>
        raise(
          WarningReport(
            msg"WasmBackend::getVar for ${l.toString} (${l.getClass().getName()}) not implemented yet" -> N :: Nil,
            source = Diagnostic.Source.Compilation
          )
        )
        summon[ModuleProxy].unreachable()

  def argument(a: Arg)(using ModuleProxy, Raise): ExprProxy =
    if a.spread then
      raise(
        WarningReport(
          msg"WasmBackend::argument for `${a.toString}` (spread == true) not implemented yet" -> N :: Nil,
          source = Diagnostic.Source.Compilation
        )
      )
      summon[ModuleProxy].unreachable()
    else result(a.value)

  def operand(
      a: Arg
  )(using ModuleProxy, Raise): ExprProxy =
    if a.spread then die else subexpression(a.value)

  def subexpression(
      r: Result
  )(using ModuleProxy, Raise): ExprProxy = result(r)

  def result(
      r: Result
  )(using ModuleProxy, Raise): ExprProxy =
    val mod = summon[ModuleProxy]
    r match
      case Value.Lit(BoolLit(value)) =>
        mod.i32.const(if value then 1 else 0)
      case Value.Lit(IntLit(value)) =>
        mod.i32.const(value.toInt)
      case Value.Ref(l: BuiltinSymbol) =>
        if l.nullary then
          raise(
            WarningReport(
              msg"WasmBackend::result for ${r.toString} not implemented yet" -> N :: Nil,
              source = Diagnostic.Source.Compilation
            )
          )
          mod.unreachable()
        else errExpr(msg"Illegal reference to builtin symbol '${l.nme}'")
      case Value.Ref(l) => getVar(l)

      case Call(Value.Ref(l: BuiltinSymbol), lhs :: rhs :: Nil)
          if !l.functionLike =>
        if l.binary then
          l.nme match
            case "+" =>
              // TODO(Derppening): Refactor to call `plus_impl`
              // TODO(Derppening): Omit emitting sanity checks
              val lhsOpRaw = operand(lhs)
              val lhsOp = lhsOpRaw.getType match
                case RefType(HeapType.I31, _) =>
                  mod.i31ref.get(
                    mod.ref.cast(lhsOpRaw, i31ref),
                    true
                  )
                case I32Type => lhsOpRaw
                case _ => ???
              val rhsOpRaw = operand(rhs)
              val rhsOp = rhsOpRaw.getType match
                case RefType(HeapType.I31, _) =>
                  mod.i31ref.get(
                    mod.ref.cast(rhsOpRaw, i31ref),
                    true
                  )
                case I32Type => lhsOpRaw
                case _ => ???
              mod.i32.add(lhsOp, rhsOp)
            case lNme =>
              raise(
                WarningReport(
                  msg"WasmBackend::result for binary builtin symbol '${lNme.toString}' not implemented yet" -> N :: Nil,
                  source = Diagnostic.Source.Compilation
                )
              )
              mod.unreachable()
        else errExpr(msg"Cannot call non-binary builtin symbol '${l.nme}'")
      case Call(Value.Ref(l: BuiltinSymbol), rhs :: Nil) if !l.functionLike =>
        if l.unary then
          raise(
            WarningReport(
              msg"WasmBackend::result for unary builtin symbol '${l.nme.toString}' not implemented yet" -> N :: Nil,
              source = Diagnostic.Source.Compilation
            )
          )
          mod.unreachable()
        else errExpr(msg"Cannot call non-unary builtin symbol '${l.nme}'")
      case Call(Value.Ref(l: BuiltinSymbol), args) =>
        if l.functionLike then
          raise(
            WarningReport(
              msg"WasmBackend::result for builtin symbol '${l.nme.toString}' not implemented yet" -> N :: Nil,
              source = Diagnostic.Source.Compilation
            )
          )
          mod.unreachable()
        else errExpr(msg"Illegal arity for builtin symbol '${l.nme}'")

      case Call(s @ Select(_, id), lhs :: rhs :: Nil) =>
        raise(
          WarningReport(
            msg"WasmBackend::result for ${r.toString} not implemented yet" -> N :: Nil,
            source = Diagnostic.Source.Compilation
          )
        )
        mod.unreachable()
      case c @ Call(fun, args) =>
        val base = subexpression(fun)
        // Propagate `unreachable` to its parent expression
        if base.getType != UnreachableType then
          val baseTy = base.getType.asInstanceOf[
            RefType
          ].heapType.asInstanceOf[SignatureType]
          val wasmArgs = args.map(argument)
          mod.callRef(base, wasmArgs, baseTy.params, baseTy.results)
        else base
      case r =>
        raise(
          WarningReport(
            msg"WasmBackend::result for ${r.toString} not implemented yet" -> N :: Nil,
            source = Diagnostic.Source.Compilation
          )
        )
        mod.unreachable()

  def returningTerm(
      t: Block
  )(using ModuleProxy, Raise): ExprProxy =
    val mod = summon[ModuleProxy]
    t match
      case Define(defn, rst) =>
        defn match
          case FunDefn(owner, sym, params, body) =>
            if owner.nonEmpty then
              raise(
                WarningReport(
                  msg"WasmBackend::returningTerm for ${defn.toString} (owner.nonEmpty == true) not implemented yet" -> N :: Nil,
                  source = Diagnostic.Source.Compilation
                )
              )
              ???
            val bodyExpr = block(body)
            mod.addFunction(
              sym.nme,
              params = this.createType(
                params.flatMap(_.params).map(_ => this.anyref).toSeq
              ),
              results = bodyExpr.getWasmType(false),
              vars = Seq(),
              body = bodyExpr
            )
            returningTerm(rst)
          case defn =>
            raise(
              WarningReport(
                msg"WasmBackend::returningTerm for ${defn.toString} not implemented yet" -> N :: Nil,
                source = Diagnostic.Source.Compilation
              )
            )
            mod.unreachable()
      case Return(Value.Lit(UnitLit(false)), false) => mod.ret(N)
      case Return(res, true) =>
        val resValue = result(res)
        resValue.getType match
          case I32Type => mod.ref.i31(resValue)
          case _ => resValue
      case Return(res, false) =>
        val resValue = result(res)
        resValue.getType match
          case I32Type => mod.ret(S(mod.ref.i31(resValue)))
          case _ => mod.ret(S(resValue))
      case End(_) =>
        // TODO: Insert `drop`s
        mod.nop()
      case t =>
        raise(
          WarningReport(
            msg"WasmBackend::returningTerm for ${t.toString} not implemented yet" -> N :: Nil,
            source = Diagnostic.Source.Compilation
          )
        )
        mod.unreachable()

  def program(p: Program, exprt: Opt[BlockMemberSymbol])(using
      Raise
  ): ModuleProxy =
    if p.imports.nonEmpty then
      raise(
        WarningReport(
          msg"Imports of external symbols ${p.imports.mkString("[", ", ", "]")} not implemented yet" -> N :: Nil,
          source = Diagnostic.Source.Compilation
        )
      )
    val module = newModule
    val mainFnExpr = block(p.main)(using module)
    if exprt.isDefined then
      raise(
        WarningReport(
          msg"Exports of symbols not implemented yet" -> N :: Nil,
          source = Diagnostic.Source.Compilation
        )
      )
    val mainFn = module.addFunction(
      name = "main",
      params = this.none,
      results = mainFnExpr.getWasmType(false),
      vars = Seq(),
      mainFnExpr
    )
    module.addFunctionExport("main", "main")
    // TODO(Derppening): Do we treat `main` as a main function, or just a launchpad from
    //                   JS? Start functions must not return any value though...
    // module.setStart(mainFn)
    module

  // TODO(Derppening): Make this return Seq[ExprProxy], since Wasm allows
  //                   returning multiple values
  def block(
      t: Block
  )(using ModuleProxy, Raise): ExprProxy =
    returningTerm(t)
end WatBackend

@main
def main(): Unit =
  println(WasmGenerator.mkSimpleModule(WatBackend()).emitText)
