package hkmc2
package codegen
package wasm
package text

import mlscript.utils.*, shorthands.*

import document.*
import semantics.*
import semantics.Elaborator.State
import syntax.Tree.{BoolLit, IntLit, UnitLit}
import wasm.Module as WasmModule
import text.Instructions as WasmInstr
import Locals.locals
import Message.MessageContext

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.util.boundary, boundary.break

/**
 * A reference to an `export` field in a module.
 *
 * @param mod
 *   The module that contains the export.
 * @param intName
 *   The internal name of the export.
 */
case class ExportRef(mod: ModuleProxy, intName: Str) extends Export[ExportRef],
      ToWat:
  def toWat: Document = doc"$$$intName"
end ExportRef

/**
 * A reference to an expression.
 *
 * @param inner
 *   The [[Expr]] that this proxy represents.
 */
class ExprProxy(val inner: Expr) extends Expression[ExprProxy], ToWat:
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

  def toWat: Document = inner match
    case stackInstr: Ls[StackInstr] =>
      stackInstr.map(_.toWat).mkDocument(" # ")
    case foldedInstr: Opt[FoldedInstr] => foldedInstr.dlof(_.toWat)(doc"")
end ExprProxy

/**
 * A reference to a `func` field in a module.
 *
 * @param mod
 *   The module that contains the function.
 * @param name
 *   The name of the function.
 */
case class FuncRef(mod: ModuleProxy, name: Str) extends Function[FuncRef],
      ToWat:
  type Expr = ExprProxy

  def toWat: Document = doc"$$$name"
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
class GlobalRef(mod: ModuleProxy, name: Str) extends Global[GlobalRef], ToWat:
  def toWat: Document = doc"$$$name"
end GlobalRef

/**
 * An index representing a local variable in a function.
 */
case class LocalIdx(idx: Int) extends ToWat:
  def toWat: Document = doc"$idx"
end LocalIdx

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
          case packedTy: WasmPackedType => Field(packedTy, mut, N)
          case ty: WasmType => Field(ty, mut, N)
    )

  def setStructTypeNamed(
      index: Int,
      fields: Seq[(WasmType | WasmPackedType, Bool, Str)]
  ): Unit =
    ensureFieldSize(index)
    entries(index) = StructType(
      fields.map: (ty, mut, id) =>
        ty match
          case packedTy: WasmPackedType => Field(packedTy, mut, S(id))
          case ty: WasmType => Field(ty, mut, S(id))
    )

  def build(): WasmType =
    gen.createType(entries.map(entry => RefType(entry, false)).toSeq)
end TypeBuilder

object Locals:
  enum Scope:
    case Global
    case Local
  end Scope

  def locals(using locals: Locals): Locals = locals

  def empty: Locals = Locals(N, N, Seq.empty)

/**
 * A scope for tracking local variables within a function.
 *
 * This implementation is loosely based on [[hkmc2.utils.Scope]], using numeric
 * identifiers to adhere to WebAssembly requirements.
 *
 * @param parent
 *   The parent scope, or [[None]] if this is the global scope.
 * @param curThis
 *   The current `this` symbol. See [[hkmc2.utils.Scope]] for an explanation of
 *   the nested use of [Opt].
 * @param params
 *   The parameters of the function, if any.
 */
class Locals(
    val parent: Opt[Locals],
    val curThis: Opt[Opt[(InnerSymbol, WasmType)]],
    params: Seq[(Local, WasmType)]
):
  parent match
    case S(p) =>
      require(p.parent.isEmpty, "Nested local scopes are not supported")
    case N =>
      require(params.isEmpty, "Global scope should not contain parameters")

  private val bindings = mutable.Map[Local, Int]()
  private val paramTypes = mutable.ArrayBuffer[WasmType]()
  private val localTypes = mutable.ArrayBuffer[WasmType]()

  // Insert all parameters into scope
  params.foreach: (l, ty) =>
    allocateName(l, ty)

  def nParams: Int = paramTypes.size

  private def inferScope: Locals.Scope =
    parent.dlof(_ => Locals.Scope.Local)(Locals.Scope.Global)

    /**
     * Finds and returns the appropriate global/local index for the given
     * `thisSym` symbol.
     */
  def findThis_!(thisSym: InnerSymbol)(using Raise): (Locals.Scope, Int) =
    curThis.map(_.map(_._1)) match
      case S(S(`thisSym`)) =>
        // `this` is bound to the first local variable
        (inferScope, nParams)
      case _ =>
        raise(
          ErrorReport(
            msg"Resolution of `thisSym` (${thisSym.toString}) not yet supported" -> N :: Nil,
            source = Diagnostic.Source.Compilation
          )
        )
        (inferScope, -1)

  def lookup(l: Local): Opt[(Locals.Scope, Int)] =
    bindings.get(l).map:
      (inferScope, _)
    .orElse:
      parent.flatMap(_.lookup(l))

  def lookup_!(l: Local)(using Raise): (Locals.Scope, Int) =
    lookup(l).getOrElse:
      // Prevent long-winded error messages which quote the entire definition.
      val loc = l match
        case sym: semantics.BlockMemberSymbol =>
          sym.trees.collectFirst:
            case t: syntax.Tree.TypeDef => t.head.toLoc
          .flatten.orElse(l.toLoc)
        case other => other.toLoc
      raise(ErrorReport(
        msg"No definition found in scope for '${l.nme}'" -> loc :: Nil,
        extraInfo = Some(l -> l.getClass),
        source = Diagnostic.Source.Compilation
      ))
      (inferScope, -1)

  def allocateName(
      l: Local,
      ty: WasmType,
      isParam: Bool = false
  ): (Locals.Scope, Int) =
    val index = if isParam then
      require(
        localTypes.isEmpty,
        "Cannot allocate name for parameter after local v"
      )
      val index = paramTypes.size
      paramTypes += ty
      bindings += l -> index
      index
    else
      val index =
        paramTypes.size + curThis.flatten.dlof(_ => 1)(0) + localTypes.size
      localTypes += ty
      bindings += l -> index
      index
    (inferScope, index)

  def getThisScope: Opt[Locals] =
    curThis.fold(parent.flatMap(_.getThisScope))(_ => S(this))

  def getOuterThisScope: Opt[Locals] = parent.flatMap(_.getThisScope)

  def nestRebindThis[R](thisSym: Opt[(
      InnerSymbol,
      WasmType
  )])(k: Locals ?=> R)(using Raise): (Opt[?], R) =
    val nested = Locals(S(this), S(thisSym), Seq.empty)
    val res = k(using nested)
    getOuterThisScope match
      case N => (N, res)
      case S(outer) =>
        raise(ErrorReport(
          msg"Locals::nestRebindThis: Getting outer scope (`getOuterThisScope.isDefined`) not supported" -> N :: Nil,
          source = Diagnostic.Source.Compilation
        ))
        (N, res)

  /**
   * Returns a [Seq] representing the types of all local variables declared with
   * `(local ...)`.
   *
   * This includes the implicit `this` in the first position, if present in the
   * function.
   */
  def getLocalsTypes: Seq[WasmType] =
    curThis.flatten.map(_._2).toSeq ++ localTypes.toSeq

  /**
   * Returns a [Seq] representing the types of all local variables, including
   * parameters.
   */
  def getTypes: Seq[WasmType] =
    paramTypes.toSeq ++ curThis.flatten.map(_._2).toSeq ++ getLocalsTypes

  /** Returns a [Seq] representing the types of all global variables. */
  def getGlobalTypes: Seq[WasmType] = parent.dlof(_.getGlobalTypes)(getTypes)

end Locals

/**
 * A reference to a WebAssembly module.
 *
 * @param gen
 *   The [[WatBackend]] that generates constructs for this module.
 * @param mod
 *   The underlying [[wasm.Module]] that this proxy represents.
 */
class ModuleProxy(private val gen: WatBackend, private var mod: Module)
    extends WasmModule[WasmType, ExprProxy], ToWat:

  /** Monotonically increasing counter for giving unique names to types. */
  private val anonTypeCounter = AtomicLong()

  /**
   * Adds a type to this module.
   *
   * @param name
   *   The name of the type, or [[None]] if a type name should be generated.
   * @param ty
   *   The Wasm composite type to add.
   */
  // TODO(Derppening): Consider relaxing `ty` to `rectype` when it is needed
  def addType(name: Opt[Str], ty: CompType): TypeRef =
    assume(
      name.forall(name => !mod.ty.exists((nm, _) => nm == name)),
      s"Type `$name` already exists"
    )

    val intName = name.getOrElse:
      s"_${anonTypeCounter.getAndIncrement()}"

    mod = mod.copy(ty =
      mod.ty :+ (intName -> ModType(ty, doc"(type $$$intName ${ty.toWat})"))
    )
    TypeRef(intName)

  /** Gets a type by name. */
  def getType(name: Str): Opt[CompType] =
    mod.ty.find(_nm => _nm._1 == name).map(_._2.defn)

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
  ): TypeRef = addType(name, SignatureType(params, results))

  type Exprt = ExportRef
  type Func = FuncRef
  type FuncInfo = FunctionInfo
  type Glob = GlobalRef

  // TODO(Derppening): Add overload for naming params/vars
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
      doc"(func $$$name${SignatureType(params, results).signatureToWat.optionUnless(
          _.isEmpty
        ).dlof(sig => doc" $sig ")(doc"")}${(vars
          .map(v => doc"(local ${v.toWat})") :+ body.toWat)
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
      doc"(import \"$externalModuleName\" \"$externalBaseName\" (func $$$internalName${SignatureType(params, results).signatureToWat.optionUnless(
          _.isEmpty
        ).dlof(s => doc" $s")(doc"")}"

    mod = mod.copy(im = mod.im :+ internalName -> funcImp)

  def addTableImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit =
    val tableImp =
      doc"(import \"$externalModuleName\" \"$externalBaseName\" (table $$$internalName ${gen.funcref.toWat}))"

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
      doc"(import \"$externalModuleName\" \"$externalBaseName\" (global $$$internalName ${globalType.toWat}))"

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
    val globalDecl = doc"(global $$$name ${
        if mutable then doc"(mut ${ty.toWat})" else ty.toWat
      } ${value.toWat})"

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
            if segment.passive then doc"" else doc" ${segment.offset.toWat}"
          } \"${segment.data.mkString}\")"
    )
    exportName.foreach:
      this.addMemoryExport("0", _)

  def setStart(start: Func): Unit =
    mod = mod.copy(st = S(start.name))

  def getFunctionInfo(ftype: Func): FuncInfo =
    val func = mod.fn.find(_._1 == ftype.name).map(_._2).get
    new FunctionInfo(
      name = func._1.id,
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
          label.map(label => s"$$$label").toSeq ++ resultType.map(
            _.toSeq.map(SignatureType(
              NoneType,
              _
            ).signatureToWat).mkDocument(doc" # ")
          ),
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
          resultType.toSeq.map(SignatureType(NoneType, _).signatureToWat),
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

  def `return`(value: Opt[ExprProxy]): ExprProxy =
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

  def unreachable(): ExprProxy = new ExprProxy(S(WasmInstr.unreachable))

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

  def call_ref(
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
          Seq(fnTypeStrIndex.toWat),
          Seq(target.inner) ++ operands.map(_.inner),
          results
        )
      )
    )

  def i32 = new I32:
    def const(value: Int): ExprProxy =
      new ExprProxy(S(WasmInstr.i32.const(value)))

    def add(left: ExprProxy, right: ExprProxy): ExprProxy =
      new ExprProxy(
        S(FoldedInstr("i32.add", Seq(), Seq(left.inner, right.inner), I32Type))
      )
  end i32

  def ref = new Ref:
    def `null`(ty: WasmType): ExprProxy =
      val refType = ty.asInstanceOf[RefType]
      require(
        refType.nullable,
        "`ref.null` requires its reference type to be nullable"
      )
      new ExprProxy(
        S(FoldedInstr(
          "ref.null",
          Seq(refType.heapType.toWat),
          Seq(),
          ty
        ))
      )

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

    def i31(value: ExprProxy): ExprProxy = new ExprProxy(S(
      value.inner match
        case S(value: FoldedInstr) => WasmInstr.ref.i31(value)
        case value => FoldedInstr("ref.i31", Seq(), Seq(value), gen.i31ref)
    ))

    def test(value: ExprProxy, castType: WasmType): ExprProxy = new ExprProxy(S(
      (value.inner, castType) match
        case (S(value: FoldedInstr), castType: RefType) =>
          WasmInstr.ref.test(value, castType)
        case (value, castType) => FoldedInstr(
            "ref.test",
            Seq(castType.toWat),
            Seq(value),
            gen.i32
          )
    ))

    def cast(value: ExprProxy, castType: WasmType): ExprProxy =
      new ExprProxy(
        S(
          FoldedInstr(
            "ref.cast",
            Seq(castType.toWat),
            Seq(value.inner),
            castType
          )
        )
      )
  end ref

  def i31ref = new I31Ref:
    def get(i31: ExprProxy, signed: Bool): ExprProxy = new ExprProxy(S(
      i31.inner match
        case S(value: FoldedInstr) => WasmInstr.i31ref.get(value, signed)
        case _ => FoldedInstr(
            s"i31.get_${if signed then 's' else 'u'}",
            Seq(),
            Seq(i31.inner),
            I32Type
          )
    ))
  end i31ref

  def struct = new Struct:
    private def unwrapStructNewType(
        ty: HeapType,
        typeref: Opt[TypeRef],
        instr: Str
    ): TypeRef =
      ty match
        case typeref @ TypeRef(id) =>
          unwrapStructNewType(getType(id).get, S(typeref), instr)
        case structType: StructType =>
          typeref match
            case S(typeref) => typeref
            case N => addType(N, structType)
        case _ =>
          throw IllegalArgumentException(
            s"Expected `$instr` to have a `(ref (struct ...))` type, but got `${ty.toWat}`"
          )

    /**
     * Unwraps the [[WasmType]] passed into `struct.new{,_default}` into a
     * `StrucType`.
     */
    private def unwrapStructNewType(
        ty: WasmType,
        instr: Str
    ): TypeRef =
      val refType = ty match
        case RefType(heapType, _) => heapType
        // TODO(Derppening): Update assertion message
        case ty => throw IllegalArgumentException(
            s"Expected `$instr` to have a `(ref (struct ...))` type, but got `${ty.toWat}`"
          )
      unwrapStructNewType(refType, N, instr)

    def `new`(operands: Seq[ExprProxy], ty: WasmType): ExprProxy =
      val structTy = unwrapStructNewType(ty, "struct.new")

      ExprProxy(
        S(
          FoldedInstr(
            s"struct.new",
            Seq(structTy.toWat),
            operands.map(_.inner),
            ty
          )
        )
      )

    def new_default(ty: WasmType): ExprProxy =
      val structTy = unwrapStructNewType(ty, "struct.new_default")

      ExprProxy(
        S(
          FoldedInstr(
            "struct.new_default",
            Seq(structTy.toWat),
            Seq(),
            ty
          )
        )
      )

    def get(
        index: Int,
        ref: ExprProxy,
        ty: WasmType,
        isSigned: Bool
    ): ExprProxy =
      ExprProxy(
        S(
          FoldedInstr(
            "struct.get",
            Seq(
              ref.getWasmType(true).asInstanceOf[RefType].heapType.toWat,
              index
            ),
            Seq(ref.inner),
            ty
          )
        )
      )

    def set(index: Int, ref: ExprProxy, value: ExprProxy): ExprProxy =
      ExprProxy(
        S(
          FoldedInstr(
            "struct.set",
            Seq(
              ref.getWasmType(true).asInstanceOf[RefType].heapType.toWat,
              index
            ),
            Seq(ref.inner, value.inner),
            gen.none
          )
        )
      )
  end struct

  def local = new Local:
    def get(index: Int, ty: WasmType): ExprProxy =
      ExprProxy(
        S(
          FoldedInstr(
            "local.get",
            Seq(s"$index"),
            Seq(),
            ty
          )
        )
      )

    def set(index: Int, value: ExprProxy): ExprProxy =
      ExprProxy(
        S(
          FoldedInstr(
            "local.set",
            Seq(s"$index"),
            Seq(value.inner),
            NoneType
          )
        )
      )
  end local

  def global = new Global:
    def get(index: Int, ty: WasmType): ExprProxy =
      ExprProxy(
        S(
          FoldedInstr(
            "global.get",
            Seq(s"$index"),
            Seq(),
            ty
          )
        )
      )

    def set(index: Int, value: ExprProxy): ExprProxy =
      ExprProxy(
        S(
          FoldedInstr(
            "global.set",
            Seq(s"$index"),
            Seq(value.inner),
            NoneType
          )
        )
      )
  end global

  def toWat: Document = mod.toWat
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
  lazy val anyref: WasmType = RefType.anyref
  lazy val eqref: WasmType = RefType(HeapType.Eq, nullable = true)
  lazy val i31ref: WasmType = RefType(HeapType.I31, nullable = true)
  lazy val structref: WasmType = RefType(HeapType.Struct, nullable = true)
  lazy val unreachable: WasmType = UnreachableType
  lazy val notPacked: WasmPackedType = WasmPackedType.NotPacked
  lazy val i8: WasmPackedType = WasmPackedType.I8
  lazy val i16: WasmPackedType = WasmPackedType.I16

  def createType(types: TypeRefs): WasmType =
    types.size match
      case 0 => NoneType
      case 1 => types.head
      case _ => MultiValueType(types)
  def expandType(ty: WasmType): TypeRefs = ty.toSeq

  def getExpressionType(expr: ExprProxy): WasmType = expr.getType
  def getExpressionWasmType(
      expr: ExprProxy,
      expectsValue: Bool
  ): WasmType = expr.getWasmType(expectsValue)

  def newModule: ModuleProxy = ModuleProxy(this, Module())

  def newTypeBuilder(size: Int): TypeBuilder = TypeBuilder(this, size)

  /**
   * Returns an expression equivalent to `expr` but returning a reference value
   * instead, by inserting instructions as necessary.
   */
  private def toRefExpr(expr: ExprProxy)(using mod: ModuleProxy): ExprProxy =
    expr.getType match
      case I32Type => mod.ref.i31(expr)
      case _ => expr

  /* Functions taken from JSBuilder */

  def errExpr(errMsg: Message)(using ModuleProxy, Raise): ExprProxy =
    raise(
      ErrorReport(errMsg -> N :: Nil, source = Diagnostic.Source.Compilation)
    )
    summon[ModuleProxy].unreachable()

  def getVar(l: Local)(using ModuleProxy, Locals, Raise): ExprProxy =
    l match
      case ts: semantics.TermSymbol =>
        raise(
          WarningReport(
            msg"WatBackend::getVar for ${ts.toString} not implemented yet" -> N :: Nil,
            source = Diagnostic.Source.Compilation
          )
        )
        summon[ModuleProxy].unreachable()
      case ts: semantics.InnerSymbol =>
        raise(
          WarningReport(
            msg"WatBackend::getVar for ${ts.toString} not implemented yet" -> N :: Nil,
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
      case _ =>
        val mod = summon[ModuleProxy]
        locals.lookup_!(l) match
          case (Locals.Scope.Global, idx) =>
            mod.global.get(idx, locals.getGlobalTypes(idx))
          case (Locals.Scope.Local, idx) =>
            mod.local.get(idx, locals.getLocalsTypes(idx))

  def argument(a: Arg)(using ModuleProxy, Locals, Raise): ExprProxy =
    if a.spread.nonEmpty then
      raise(
        WarningReport(
          msg"WatBackend::argument for `${a.toString}` (spread.nonEmpty) not implemented yet" -> N :: Nil,
          source = Diagnostic.Source.Compilation
        )
      )
      summon[ModuleProxy].unreachable()
    else result(a.value)

  def operand(
      a: Arg
  )(using ModuleProxy, Locals, Raise): ExprProxy =
    if a.spread.nonEmpty then die else subexpression(a.value)

  def subexpression(
      r: Result
  )(using ModuleProxy, Locals, Raise): ExprProxy = result(r)

  def fieldSelect(`this`: ExprProxy, s: Str)(using mod: ModuleProxy): Int =
    `this`.getType match
      case RefType(TypeRef(id), _) =>
        mod.getType(
          id
        ).get.asInstanceOf[StructType].fields.indexWhere(_.id.exists(_ == s))
      case RefType(StructType(fields), _) =>
        fields.indexWhere(_.id.exists(_ == s))
      case _ =>
        throw IllegalArgumentException(
          s"Expected `this` to be a `(ref (struct ...))` type, but got `${`this`.getWasmType(true).toWat}`"
        )

  def result(
      r: Result
  )(using ModuleProxy, Locals, Raise): ExprProxy =
    val mod = summon[ModuleProxy]
    r match
      case Value.This(sym) =>
        val (thisScope, thisLocal) = summon[Locals].findThis_!(sym)
        assert(
          thisScope == Locals.Scope.Local,
          "`this` should always be resolved locally"
        )
        mod.local.get(thisLocal, summon[Locals].getTypes(thisLocal))
      case Value.Lit(BoolLit(value)) =>
        mod.i32.const(if value then 1 else 0)
      case Value.Lit(IntLit(value)) =>
        mod.i32.const(value.toInt)
      case Value.Ref(l: BuiltinSymbol) =>
        if l.nullary then
          raise(
            WarningReport(
              msg"WatBackend::result for ${r.toString} not implemented yet" -> N :: Nil,
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
              def castOperand(expr: ExprProxy, opSide: Str): ExprProxy =
                expr.getType match
                  case RefType(HeapType.Any, _) =>
                    // TOOD(Derppening): Refactor to use `br_on_cast`/`br_on_cast_fail`
                    mod.`if`(
                      mod.ref.test(expr, this.i31ref),
                      ifTrue =
                        castOperand(mod.ref.cast(expr, this.i31ref), opSide),
                      ifFalse = S(mod.unreachable())
                    )
                  case RefType(HeapType.I31, _) =>
                    mod.i31ref.get(expr, true)
                  case I32Type => expr
                  case ty =>
                    raise(
                      WarningReport(
                        msg"WatBackend::result for binary builtin symbol '${l.nme.toString}' ($opSide.type=${ty.toString}) not implemented yet" -> N :: Nil,
                        source = Diagnostic.Source.Compilation
                      )
                    )
                    mod.unreachable()

              val lhsOp = castOperand(operand(lhs), "lhs")
              val rhsOp = castOperand(operand(rhs), "rhs")

              (lhsOp.getType, rhsOp.getType) match
                case (I32Type, I32Type) =>
                  mod.ref.i31(mod.i32.add(lhsOp, rhsOp))
                case (lhsType, rhsType) =>
                  raise(
                    WarningReport(
                      msg"WatBackend::result for binary builtin symbol '${l.nme.toString}' (${lhsType.toString}, ${rhsType.toString}) not implemented yet" -> N :: Nil,
                      source = Diagnostic.Source.Compilation
                    )
                  )
                  mod.unreachable()
            case lNme =>
              raise(
                WarningReport(
                  msg"WatBackend::result for binary builtin symbol '${lNme.toString}' not implemented yet" -> N :: Nil,
                  source = Diagnostic.Source.Compilation
                )
              )
              mod.unreachable()
        else errExpr(msg"Cannot call non-binary builtin symbol '${l.nme}'")
      case Call(Value.Ref(l: BuiltinSymbol), rhs :: Nil) if !l.functionLike =>
        if l.unary then
          raise(
            WarningReport(
              msg"WatBackend::result for unary builtin symbol '${l.nme.toString}' not implemented yet" -> N :: Nil,
              source = Diagnostic.Source.Compilation
            )
          )
          mod.unreachable()
        else errExpr(msg"Cannot call non-unary builtin symbol '${l.nme}'")
      case Call(Value.Ref(l: BuiltinSymbol), args) =>
        if l.functionLike then
          raise(
            WarningReport(
              msg"WatBackend::result for builtin symbol '${l.nme.toString}' not implemented yet" -> N :: Nil,
              source = Diagnostic.Source.Compilation
            )
          )
          mod.unreachable()
        else errExpr(msg"Illegal arity for builtin symbol '${l.nme}'")

      case Call(s @ Select(_, id), lhs :: rhs :: Nil) =>
        raise(
          WarningReport(
            msg"WatBackend::result for ${r.toString} not implemented yet" -> N :: Nil,
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
          mod.call_ref(base, wasmArgs, baseTy.params, baseTy.results)
        else base
      case sel @ Select(qual, id) =>
        sel.symbol match
          case S(sym) =>
            sym match
              case clsSym: ClassSymbol =>
                // Return the type of the class by using a dummy `struct.new_default` instruction
                // TODO(Derppening): Migrate to using fully-qualified name
                val clsSymNme = clsSym.nme
                mod.getType(clsSymNme) match
                  case S(_: StructType) =>
                    mod.struct.new_default(RefType(
                      TypeRef(clsSymNme),
                      nullable = true
                    ))
                  case _ =>
                    raise(
                      InternalError(
                        msg"Cannot find member ${sel.toString} in Wasm module" -> N :: Nil,
                        source = Diagnostic.Source.Compilation
                      )
                    )
                    mod.unreachable()
              case termSym: TermSymbol =>
                val qualExpr = result(qual)
                val termOwner = termSym.owner.get
                val termOwnerNme = termOwner.nme
                val termOwnerFields = mod.getType(termOwnerNme) match
                  case S(termOwnerType: StructType) => termOwnerType.fields
                  case S(_) =>
                    raise(
                      WarningReport(
                        msg"WatBackend::result for ${sel.toString} (`sel.symbol.asInstanceOf[TermSymbol].owner == ${termOwner.toString}`) not implemented yet" -> N :: Nil,
                        source = Diagnostic.Source.Compilation
                      )
                    )
                    return mod.unreachable()
                  case N => lastWords(
                      s"Expected type definition of $termOwner to be registered in the Wasm module"
                    )
                val fieldIdx =
                  termOwnerFields.indexWhere(_.id.exists(_ == id.name))

                mod.struct.get(
                  fieldIdx,
                  mod.ref.cast(
                    qualExpr,
                    RefType(TypeRef(termOwnerNme), nullable = false)
                  ),
                  ty = termOwnerFields(fieldIdx).ty,
                  isSigned = false
                )
              case sym =>
                raise(
                  WarningReport(
                    msg"WatBackend::result for ${sel.toString} (`sel.symbol == ${sym.toString}`) not implemented yet" -> N :: Nil,
                    source = Diagnostic.Source.Compilation
                  )
                )
                mod.unreachable()
          case N =>
            raise(
              InternalError(
                msg"Unable to resolve symbol `${sel.toString}`" -> N :: Nil,
                source = Diagnostic.Source.Compilation
              )
            )
            mod.unreachable()
      case Instantiate(_, cls, as) =>
        // TODO(Derppening): Do not use result(...) for resolving classes
        val clazz = result(cls)
        val clazzRefTy = clazz.getType.asInstanceOf[RefType]
        val clazzStructTy = clazzRefTy.heapType.asInstanceOf[TypeRef]

        mod.call(
          s"${clazzStructTy.id}::<constructor>",
          as.map(argument).map(toRefExpr(_)),
          clazzRefTy
        )
      case r =>
        raise(
          WarningReport(
            msg"WatBackend::result for ${r.toString} not implemented yet" -> N :: Nil,
            source = Diagnostic.Source.Compilation
          )
        )
        mod.unreachable()

  def returningTerm(
      t: Block
  )(using ModuleProxy, Locals, Raise): ExprProxy =
    val mod = summon[ModuleProxy]
    t match
      case _: HandleBlock =>
        errExpr(
          msg"This code requires effect handler instrumentation but was compiled without it."
        )
      case Assign(l, r, rst) =>
        val lExpr = getVar(l).inner match
          case stackInstrs @ _ :: _ => stackInstrs.last
          case S(foldedInstr) => foldedInstr
          case _ =>
            lastWords(s"getVar($l) should always return a non-empty expression")
        val lScope = lExpr.mnemonic match
          case "global.get" => Locals.Scope.Global
          case "local.get" => Locals.Scope.Local
          case _ => lastWords(
              s"Expected `global.get` or `local.get` when compiling instruction for `$l`, but got ${lExpr.mnemonic}"
            )
        val lIdx = lExpr.instrargs(0).toString.toInt

        val rExpr = result(r)
        val assignExpr = lScope match
          case Locals.Scope.Global => mod.global.set(lIdx, toRefExpr(rExpr))
          case Locals.Scope.Local => mod.local.set(lIdx, toRefExpr(rExpr))
        val rstBlk = returningTerm(rst)

        mod.block(
          label = N,
          Seq(
            assignExpr,
            rstBlk
          ),
          resultType = rstBlk.getType.optionUnless(_ is this.none)
        )
      case Define(defn, rst) =>
        def mkThis(sym: InnerSymbol): ExprProxy =
          result(Value.This(sym))
        val resWat = defn match
          case ValDefn(tsym, sym, p) =>
            tsym.owner match
              case N =>
                raise(
                  WarningReport(
                    msg"WatBackend::returningTerm for ${defn.toString} (`tsym.owner is N`) not implemented yet" -> N :: Nil,
                    source = Diagnostic.Source.Compilation
                  )
                )
                mod.unreachable()
              case S(owner) =>
                val thisWat = mkThis(owner)
                val fieldWat = fieldSelect(thisWat, sym.nme)
                val pWat = result(p)
                val rstWat = returningTerm(rst)
                mod.block(
                  label = N,
                  children = Seq(
                    mod.struct.set(fieldWat, thisWat, pWat),
                    rstWat
                  ),
                  resultType = S(rstWat.getType)
                )
          case defn: (FunDefn | ClsLikeDefn) =>
            val outerLocals = locals
            val (thisProxy, res) = locals.nestRebindThis(
              // * Either this is an InnerSymbol or this is a Fun,
              // * and we need to rebind `this` to None to shadow it.
              defn.innerSym.collectFirst:
                case s: InnerSymbol =>
                  val ty = defn match
                    case clsLikeDefn: ClsLikeDefn =>
                      mod.addType(
                        S(clsLikeDefn.isym.nme),
                        StructType(
                          (clsLikeDefn.publicFields.map(
                            _._2
                          ) ++ clsLikeDefn.privateFields).map: f =>
                            Field(this.anyref, mutable = true, id = S(f.nme))
                        )
                      )
                    case _ => TODO(s"innerSym for $defn not implemented")
                  (s, RefType(ty, nullable = false))
            ):
              boundary:
                defn match
                  case FunDefn(owner, sym, Nil, body) =>
                    lastWords("cannot generate function with no parameter list")
                  case FunDefn(owner, sym, params, body) =>
                    if owner.nonEmpty then
                      raise(
                        WarningReport(
                          msg"WatBackend::returningTerm for ${defn.toString} (owner.nonEmpty == true) not implemented yet" -> N :: Nil,
                          source = Diagnostic.Source.Compilation
                        )
                      )
                      break(mod.unreachable())
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
                    mod.nop()
                  case ClsLikeDefn(
                        ownr,
                        isym,
                        sym,
                        kind,
                        paramsOpt,
                        auxParams,
                        par,
                        mtds,
                        privFlds,
                        pubFlds,
                        preCtor,
                        ctor,
                        modo
                      ) =>
                    // Guard against unsupported features for now
                    if ownr.nonEmpty then
                      raise(
                        WarningReport(
                          msg"WatBackend::returningTerm for ${defn.toString} (`owner.nonEmpty == true`) not implemented yet" -> N :: Nil,
                          source = Diagnostic.Source.Compilation
                        )
                      )
                      break(mod.unreachable())
                    if !(kind is syntax.Cls) then
                      raise(
                        WarningReport(
                          msg"WatBackend::returningTerm for ${defn.toString} (`!(kind is syntax.Cls)`) not implemented yet" -> N :: Nil,
                          source = Diagnostic.Source.Compilation
                        )
                      )
                      break(mod.unreachable())
                    if auxParams.nonEmpty then
                      raise(
                        WarningReport(
                          msg"WatBackend::returningTerm for ${defn.toString} (`auxParams.nonEmpty == true`) not implemented yet" -> N :: Nil,
                          source = Diagnostic.Source.Compilation
                        )
                      )
                      break(mod.unreachable())
                    if par.nonEmpty then
                      raise(
                        WarningReport(
                          msg"WatBackend::returningTerm for ${defn.toString} (`parentPath.nonEmpty == true`) not implemented yet" -> N :: Nil,
                          source = Diagnostic.Source.Compilation
                        )
                      )
                      break(mod.unreachable())
                    if mtds.nonEmpty then
                      raise(
                        WarningReport(
                          msg"WatBackend::returningTerm for ${defn.toString} (`methods.nonEmpty == true`) not implemented yet" -> N :: Nil,
                          source = Diagnostic.Source.Compilation
                        )
                      )
                    preCtor match
                      case End(_) => ()
                      case _ => raise(
                          WarningReport(
                            msg"WatBackend::returningTerm for ${defn.toString} (`preCtor != End`) not implemented yet" -> N :: Nil,
                            source = Diagnostic.Source.Compilation
                          )
                        )
                    if modo.isDefined then
                      raise(
                        WarningReport(
                          msg"WatBackend::returningTerm for ${defn.toString} (`companion.isDefined == true`) not implemented yet" -> N :: Nil,
                          source = Diagnostic.Source.Compilation
                        )
                      )

                    val clsParams = paramsOpt.fold(Nil)(_.paramSyms)
                    val ctorParams = clsParams.map: p =>
                      p -> locals.allocateName(p, this.anyref, true)
                    val ctorFields = ctorParams.filter: p =>
                      p._1.decl match
                        case S(Param(flags = FldFlags(isVal = true))) => true
                        case _ => false
                    val ctorAuxParams = auxParams.map: ps =>
                      ps.params.map: p =>
                        p.sym -> locals.allocateName(p.sym, this.anyref)

                    val isModule = kind is syntax.Mod

                    // TODO(Derppening): Prepend s"$fileName/${isym.nme}$$${counter++}"
                    val typeref =
                      mod.getType(isym.nme).dlof(_ => TypeRef(isym.nme)):
                        lastWords(
                          "Expected type to be present in WAT during codegen for class definition"
                        )

                    // If there are no ctor params, pop one param list off the aux params
                    val (newCtorAuxParams, initialCtorParams) = paramsOpt match
                      case None => ctorAuxParams match
                          case head :: next => (next, head)
                          case Nil => (ctorAuxParams, Nil)
                      case Some(_) => (ctorAuxParams, ctorParams)

                    val thisLocalIdx = initialCtorParams.size
                    val ctorCode = mod.block(
                      label = N,
                      Seq(
                        mod.local.set(
                          thisLocalIdx,
                          mod.struct.new_default(RefType(
                            typeref,
                            nullable = false
                          ))
                        ),
                        block(ctor),
                        mod.`return`(S(mod.local.get(
                          thisLocalIdx,
                          RefType(typeref, nullable = false)
                        )))
                      ),
                      resultType = S(RefType(typeref, nullable = false))
                    )

                    val ctorAux = if newCtorAuxParams.isEmpty then
                      ctorCode
                    else
                      raise(
                        WarningReport(
                          msg"WatBackend::returningTerm for ${defn.toString} (auxiliary constructor generation) not implemented yet" -> N :: Nil,
                          source = Diagnostic.Source.Compilation
                        )
                      )
                      mod.unreachable()

                    val ctorBod = if isModule then
                      raise(
                        InternalError(
                          msg"WatBackend::returningTerm: `isModule` should be guarded and should not reach here!" -> N :: Nil,
                          source = Diagnostic.Source.Compilation
                        )
                      )
                      mod.unreachable()
                    else
                      ctorAux

                    mod.addFunction(
                      s"${isym.nme}::<constructor>",
                      params =
                        createType(
                          Seq.fill(initialCtorParams.size)(this.anyref)
                        ),
                      results = RefType(typeref, nullable = false),
                      vars = Seq(RefType(typeref, nullable = false)),
                      body = ctorBod
                    )

                    mod.nop()
                end match
            end val

            thisProxy match
              case S(proxy) => ???
              case N =>
                val retWat = returningTerm(rst)
                mod.block(
                  N,
                  Seq(res, retWat),
                  retWat.getType.optionUnless(_ is this.none)
                )

        resWat
      case Return(Value.Lit(UnitLit(false)), false) => mod.`return`(N)
      case Return(res, true) =>
        val resValue = result(res)
        resValue.getType match
          case I32Type => mod.ref.i31(resValue)
          case _ => resValue
      case Return(res, false) =>
        val resValue = result(res)
        resValue.getType match
          case I32Type => mod.`return`(S(mod.ref.i31(resValue)))
          case _ => mod.`return`(S(resValue))
      case End(_) =>
        // TODO: Insert `drop`s
        mod.nop()
      case t =>
        raise(
          WarningReport(
            msg"WatBackend::returningTerm for ${t.toString} not implemented yet" -> N :: Nil,
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
    val mainFnExpr = block(p.main)(using module, Locals.empty)
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

  def blockPreamble(ss: Iterable[Symbol])(using
      ModuleProxy,
      Raise,
      Locals
  ): ModuleProxy =
    val mod = summon[ModuleProxy]
    val vars =
      ss.filter(locals.lookup(_).isEmpty).toArray.sortBy(_.uid).iterator.map:
        l =>
          l -> locals.allocateName(l, this.anyref, false)
    for (_, (_, (nme))) <- vars do
      mod.addGlobal(
        nme.toString,
        ty = this.anyref,
        mutable = true,
        value = mod.ref.`null`(this.anyref)
      )
    mod

  // TODO(Derppening): Make this return Seq[ExprProxy], since Wasm allows
  //                   returning multiple values
  def block(
      t: Block
  )(using ModuleProxy, Locals, Raise): ExprProxy =
    val modWithPreamble = blockPreamble(t.definedVars)
    returningTerm(t)(using modWithPreamble, summon[Locals], summon[Raise])
end WatBackend

@main
def main(): Unit =
  println(WasmGenerator.mkSimpleModule(WatBackend()).toWat)
