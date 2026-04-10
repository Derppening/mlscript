package hkmc2
package codegen
package wasm
package text

import mlscript.utils.*, shorthands.*
import hkmc2.utils.*

import document.*
import document.Document
import semantics.{BlockMemberSymbol, Elaborator, LabelSymbol, ModuleOrObjectSymbol, Symbol, TempSymbol},
  Elaborator.State
import text.Param as WasmParam
import Instructions.*

import scala.annotation.{nowarn, targetName}
import scala.collection.immutable.ListMap
import scala.collection.mutable.{ArrayBuffer as ArrayBuf, Map as MutMap}
import scala.reflect.ClassTag

/** A Wasm function and its associated information.
  *
  * Each instance of [[FuncInfo]] represents a single function definition in a WebAssembly module.
  *
  * @param sym
  *   The source [[BlockMemberSymbol]] which this function is generated from.
  * @param typeUse
  *   [[TypeUse]] of the function's type in the module's type section.
  * @param params
  *   [[Seq]] of parameter local variables and their names.
  * @param nResults
  *   Number of results the function returns.
  * @param locals
  *   [[Seq]] of local variables (excluding parameters) and their names.
  * @param body
  *   The expression of the function body.
  * @param export
  *   Optional export name for the function.
  */
class FuncInfo(
    val sym: BlockMemberSymbol | TempSymbol,
    val typeUse: TypeUse,
    params: Seq[Local -> SymIdx],
    nResults: Int,
    locals: Seq[Local -> SymIdx],
    val body: Expr,
    val `export`: Opt[Str],
)(using Ctx, Raise) extends ToWat:

  @deprecated("Use the overload that takes `sym` directly instead.")
  def this(
      id: SymIdx,
      typeUse: TypeUse,
      params: Seq[Local -> SymIdx],
      nResults: Int,
      locals: Seq[Local -> SymIdx],
      body: Expr,
      `export`: Opt[Str],
  )(using Ctx, Raise, State) = this(TempSymbol(N, id.id), typeUse, params, nResults, locals, body, `export`)

  @deprecated("Use the overload that takes `sym` directly instead.")
  def this(
      id: Opt[SymIdx],
      typeUse: TypeUse,
      params: Seq[Local -> SymIdx],
      nResults: Int,
      locals: Seq[Local -> SymIdx],
      body: Expr,
      `export`: Opt[Str],
  )(using Ctx, Raise, Scope, State) = this(
    TempSymbol(N, id.map(_.id).getOrElse("")),
    typeUse,
    params,
    nResults,
    locals,
    body,
    `export`,
  )

  /** Symbolic identifier for the type. */
  val id = SymIdx(summon[Ctx].funcScp.allocateName(sym))

  /** Returns the type of this function as a [[SignatureType]]. */
  def getSignatureType: SignatureType = SignatureType(
    params = params.map((_, varNme) => WasmParam(varNme, RefType.anyref)),
    results = Seq.fill(nResults)(Result(RefType.anyref)),
  )

  def toWat: Document =
    doc"""(func ${id.toWat}${
        `export`.fold(doc""): e =>
          doc""" (export "$e")"""
      } ${typeUse.toWat}${
        getSignatureType.toWat.surroundUnlessEmpty(doc" ")
      } #{ ${
        locals.map: p =>
          doc"(local ${p._2.toWat} ${RefType.anyref.toWat})"
        .mkDocument(doc" # ").surroundUnlessEmpty(doc" # ")
      } # ${body.toWat} #} )"""
end FuncInfo

/** A Wasm global and its associated information.
  *
  * Each instance of [[GlobalInfo]] represents a single global definition in a WebAssembly module.
  *
  * @param valType
  *   The value type of the global.
  * @param mutable
  *   Whether the global is mutable.
  * @param init
  *   The initializer expression for the global.
  * @param sym
  *   The source [[Symbol]] which this global is generated from.
  */
class GlobalInfo(val valType: ValType, val mutable: Bool, val init: Expr, val sym: Symbol)(using Ctx, Raise)
    extends ToWat:

  val id: SymIdx = SymIdx(summon[Ctx].globalScp.allocateName(sym))

  def toWat: Document =
    val typeDoc =
      if mutable then doc"(mut ${valType.toWat})"
      else valType.toWat
    doc"(global ${id.toWat} $typeDoc ${init.toWat})"
end GlobalInfo

/** A WebAssembly memory and its associated information.
  *
  * Each instance of [[MemInfo]] represents a single memory definition in a WebAssembly module.
  *
  * @param id
  *   Symbolic identifier for the memory.
  * @param memType
  *   The type of the memory.
  */
class MemInfo(val id: SymIdx, val memType: MemType) extends ToWat:

  def toWat: Document = doc"(memory ${id.toWat} ${memType.toWat})"
end MemInfo

/** A Wasm type and its associated information.
  *
  * Each instance of [[TypeInfo]] represents a single type defintion in a WebAssembly module.
  *
  * @param sym
  *   The source [[Symbol]] which this type is generated from.
  * @param compType
  *   The composite type this type definition represents.
  * @param objectTag
  *   An optional object tag number associated with this type.
  */
class TypeInfo(
    val sym: BlockMemberSymbol | TempSymbol,
    val compType: CompType,
    val objectTag: Opt[Int],
)(using Ctx, Raise) extends ToWat:

  @deprecated
  def this(id: SymIdx, compType: CompType, objectTag: Opt[Int])(using Ctx, Raise, State) =
    this(TempSymbol(N, id.id), compType, objectTag)

  @deprecated
  def this(id: Opt[SymIdx], compType: CompType)(using Ctx, Raise, State) =
    this(
      TempSymbol(N, id.map(_.id).getOrElse("")),
      compType,
      N,
    )

  /** Symbolic identifier for the type. */
  val id = SymIdx(summon[Ctx].typeScp.allocateName(sym))

  def toWat: Document = doc"(type ${id.toWat} ${compType.toWat})"
end TypeInfo

/** A WebAssembly exception tag declaration.
  *
  * In Wasm, a `tag` names an exception kind and points to a function type that describes the payload values carried by
  * `throw tag ...` and extracted by matching `catch tag ...`.
  */
class TagInfo(val typeUse: TypeUse, val sym: Symbol)(using Ctx, Raise) extends ToWat:

  @deprecated("Use the overload that takes `sym` directly instead.")
  def this(id: SymIdx, typeUse: TypeUse)(using Ctx, Raise, State) =
    this(typeUse, TempSymbol(N, id.id))

  val id: SymIdx = SymIdx(summon[Ctx].tagScp.allocateName(sym))

  def toWat: Document =
    doc"""(tag ${id.toWat} (export "${id.id}") ${typeUse.toWat})"""
end TagInfo

enum WasmIntrinsicType:
  case TupleArray(mutable: Bool)

case class LabelTarget(
    breakLabel: Label,
    continueLabel: Opt[Label],
)

object FunctionCtx:

  def funcCtx(using funcCtx: FunctionCtx): FunctionCtx = funcCtx

  /** Context for tracking control flow jump targets.
    *
    * @param scp
    *   [[Scope]] for generating WAT identifiers of labels in this control flow context.
    * @param breakLabel
    *   The label to jump to for exiting this control flow context, e.g. for `break` statements.
    * @param continueLabel
    *   The label to jump to for continuing this control flow context, e.g. for `continue` statements in loops. This is
    *   `None` for non-loop contexts.
    */
  private case class ControlFlowCtx(scp: Scope, breakLabel: LabelSymbol, continueLabel: Opt[LabelSymbol])

/** Context associated with codegen for a Wasm function.
  *
  * @param _params
  *   The parameters of this function.
  */
class FunctionCtx(private val _params: Seq[Local])(using Raise, State):

  /** [[Scope]] for generating WAT identifiers of locals. */
  private[text] val localScp = Scope.empty(Scope.Cfg.default)

  /** The parameter of this function, represented by a tuple of the symbol representing the parameter and its symbolic
    * identifier.
    */
  val params: Seq[Local -> SymIdx] = _params.map(p => p -> SymIdx(localScp.allocateName(p)))
  private val _locals = ArrayBuf.empty[Local]
  private var labels = ListMap.empty[LabelSymbol, FunctionCtx.ControlFlowCtx]

  /** Adds a Wasm local into this context.
    *
    * @param customName
    *   An optional name for the local variable. If provided, the local will be emitted with the given name instead of
    *   an auto-generated one.
    */
  def addLocal(local: Local, customName: Opt[Str] = N): LocalIdx =
    customName match
      case S(name) => localScp.addToBindings(local, name, shadow = false)
      case N => localScp.allocateName(local)
    _locals += local
    LocalIdx(SymIdx(localScp.lookup_!(local, N)))

  /** Returns `true` if a local or a parameter is already defined in this function context. */
  def containsLocal(sym: Local): Bool = _params.contains(sym) || _locals.contains(sym)

  /** Looks up the given `sym` in this function context, returning its [[LocalIdx]] if it exists. */
  def lookupLocal(sym: Local): Opt[LocalIdx] =
    localScp.lookup(sym).map(idx => LocalIdx(SymIdx(idx)))

  /** Similar to [[lookupLocal]], but throws an exception if `sym` is not in this context. */
  def lookupLocal_!(sym: Local, loc: Opt[Loc]): LocalIdx =
    LocalIdx(SymIdx(localScp.lookup_!(sym, loc)))

  /** The locals of this function, represented by a tuple of the symbol representing the parameter and its symbolic
    * identifier.
    */
  def locals: Seq[Local -> SymIdx] = _locals.map(l => l -> SymIdx(localScp.lookup_!(l, N))).toSeq

  /** Pushes a label target for the dynamic extent of `body` and pops it afterwards.
    *
    * The `body` function is given a [[LabelTarget]] containing the `break` and `continue` labels corresponding to
    * `label`.
    *
    * @param hasContinueLabel
    *   Indicates whether a `continue` label should be generated for this control flow context, e.g. for loops.
    */
  def withLabel[T](label: LabelSymbol, hasContinueLabel: Bool)(body: LabelTarget => T): T =
    import Scope.scope

    val ctrlFlowCtx = FunctionCtx.ControlFlowCtx(
      scp = labels.lastOption.fold(Scope.empty(Scope.Cfg.default))(_._2.scp.nest),
      breakLabel = label,
      continueLabel = if hasContinueLabel then S(LabelSymbol(N, s"${label.nme}_cont")) else N,
    )
    labels = labels + (label -> ctrlFlowCtx)
    val res = body(
      LabelTarget(
        breakLabel = Label(SymIdx(ctrlFlowCtx.scp.allocateName(label))),
        continueLabel = ctrlFlowCtx.continueLabel.map(cl => Label(SymIdx(ctrlFlowCtx.scp.allocateName(cl)))),
      ),
    )
    labels = labels.init
    res

  /** Looks up the nearest in-scope target for `label`. */
  def lookupLabel(label: LabelSymbol): Opt[LabelTarget] =
    labels.last._2.scp.lookup(label)
      .map: labelId =>
        LabelTarget(
          breakLabel = Label(SymIdx(labelId)),
          continueLabel = labels(label).continueLabel.map(cl => Label(SymIdx(labels.last._2.scp.lookup_!(cl, N)))),
        )
end FunctionCtx

/** Generates a function body, providing an instance of [[FunctionCtx]] for parameter and locals tracking.
  *
  * Returns the result of the `mkBody` function along with the [[FunctionCtx]].
  */
def genFuncBody[T](params: Seq[Local])(mkBody: FunctionCtx ?=> T)(using Raise, State): T -> FunctionCtx =
  val funcCtx = FunctionCtx(params)
  val result = mkBody(using funcCtx)
  result -> funcCtx

object Ctx:
  case class SingletonInfo(
      globalName: Str,
      globalTy: RefType,
  )

  val binaryOps: Map[Str, (Expr, Expr) => Expr] = Map(
    "plus_impl" -> i32.add,
    "minus_impl" -> i32.sub,
    "times_impl" -> i32.mul,
    "div_impl" -> i32.div_s,
    "mod_impl" -> i32.rem_s,
    "eq_impl" -> i32.eq,
    "neq_impl" -> i32.ne,
    "lt_impl" -> i32.lt_s,
    "le_impl" -> i32.le_s,
    "gt_impl" -> i32.gt_s,
    "ge_impl" -> i32.ge_s,
  )
  val unaryOps: Map[Str, Expr => Expr] = Map(
    "neg_impl" -> (value => i32.sub(i32.const(0), value)),
    "pos_impl" -> identity,
    "not_impl" -> i32.eqz,
  )
  val wasmIntrinsicArities: Map[Str, Int] = (binaryOps.keys.map(_ -> 2) ++ unaryOps.keys.map(_ -> 1)).toMap
  val wasmIntrinsicNameSet: Set[Str] = wasmIntrinsicArities.keySet

  def empty(using State): Ctx = Ctx()

  def ctx(using ctx: Ctx): Ctx = ctx

  extension (ref: CtxIdx | Symbol)
    private def prettyString: Str = ref match
      case idx: CtxIdx => s"type index `${idx.toWat.mkString()}`"
      case sym: Symbol => s"symbol `${sym.toString}`"
end Ctx

/** Context for [[WatBuilder]]. */
class Ctx(using State) extends ToWat:

  import Ctx.prettyString

  /** [[Scope]] for generating WAT identifiers of types. */
  private[text] val typeScp = Scope.empty(Scope.Cfg.default)

  /** [[ListMap]] containing all type definitions in the module mapped by their symbolic identifiers. */
  private var types = ListMap.empty[SymIdx, TypeInfo]

  /** [[MutMap]] containing type symbols mapped to their corresponding [[TypeInfo]] instance. */
  private val namedTypes = MutMap.empty[BlockMemberSymbol, TypeInfo]

  /** [[Scope]] for generating WAT identifiers of data segments. */
  private[text] val dataSegmentScp = Scope.empty(Scope.Cfg.default)

  /** [[ListMap]] containing all data segments in the module. */
  private var dataSegments = ListMap.empty[SymIdx, DataSegment]

  /** [[Scope]] for generating WAT identifiers of element segments. */
  private[text] val elemSegmentScp = Scope.empty(Scope.Cfg.default)

  /** [[ListMap]] containing all element segments in the module. */
  private var elemSegments = ListMap.empty[SymIdx, ElemSegment]

  /** [[Scope]] for generating WAT identifiers of functions. */
  private[text] val funcScp = Scope.empty(Scope.Cfg.default)

  /** [[ListMap]] containing all function definitions and imports in the module mapped by their symbolic identifiers. */
  private var funcs = ListMap.empty[SymIdx, FuncInfo | Import[ExternType.Func]]

  /** [[MutMap]] containing function symbols mapped to the corresponding [[FuncInfo]] or [[Import]] instance. */
  private val namedFuncs = MutMap.empty[Symbol, FuncInfo | Import[ExternType.Func]]

  /** [[Scope]] for generating WAT identifiers of memories. */
  private[text] val memoryScp = Scope.empty(Scope.Cfg.default)

  /** [[ListMap]] containing all memory definitions and imports in the module mapped by their symbolic identifiers. */
  private var memories = ListMap.empty[SymIdx, MemInfo | Import[ExternType.Mem]]

  /** [[Scope]] for generating WAT identifiers of tags. */
  private[text] val tagScp = Scope.empty(Scope.Cfg.default)

  /** [[ListMap]] containing all tag definitions in the module. */
  private var tags = ListMap.empty[SymIdx, TagInfo]

  /** [[Scope]] for generating WAT identifiers of globals. */
  private[text] val globalScp = Scope.empty(Scope.Cfg.default)

  /** [[ListMap]] containing all global definitions in the module. */
  private var globals = ListMap.empty[SymIdx, GlobalInfo]

  /** [[MutMap]] containing global symbols mapped to their corresponding Wasm global indices. */
  private val namedGlobals = MutMap.empty[Symbol, GlobalInfo]

  private var startFunc = N: Opt[FuncIdx]

  /** Counter for generating object tags. */
  private var objectTagNum = 0

  private val wasmIntrinsicFuncs = MutMap.empty[Str, FuncIdx]
  private val wasmIntrinsicTypes = MutMap.empty[WasmIntrinsicType, TypeIdx]
  private val wasmIntrinsicTags = MutMap.empty[Str, TagIdx]

  private val cachedMemoryImport = MutMap.empty[(Str, Str), SymIdx]
  private val cachedFunctionImports = MutMap.empty[(Str, Str), FuncIdx]

  /** [[Scope]] for generating WAT identifiers of labels. */
  @deprecated("Use the label management functions in FunctionCtx instead.")
  private[text] val labelScp = Scope.empty(Scope.Cfg.default)
  private var labelTargets = Nil: List[(LabelSymbol, LabelTarget)]

  private val singletonByBms = MutMap.empty[BlockMemberSymbol, Ctx.SingletonInfo]
  private val singletonByIsym = MutMap.empty[ModuleOrObjectSymbol, Ctx.SingletonInfo]
  private val singletonInitActions = ArrayBuf.empty[Expr]

  private def imports: Seq[Import[?]] =
    val importedFuncs = funcs.collect:
      case (_, imp: Import[ExternType.Func]) => imp
    val importedMems = memories.collect:
      case (_, imp: Import[ExternType.Mem]) => imp
    (importedFuncs ++ importedMems).toSeq

  private def lastWordsForLabel(funcName: Str): Nothing =
    lastWords(s"$funcName is no longer supported; Please use label management functions in FunctionCtx instead.")

  /** Pushes a label target for the dynamic extent of `body` and pops it afterwards. */
  @deprecated("Use `withLabel` in FunctionCtx instead to manage labels within function bodies.")
  def withLabel[T](label: LabelSymbol, target: LabelTarget)(body: => T): T =
    lastWordsForLabel("withLabel")

  /** Looks up the nearest in-scope target for `label`. */
  @deprecated("Use `withLabel` in FunctionCtx instead to manage labels within function bodies.")
  def lookupLabel(label: LabelSymbol): Opt[LabelTarget] =
    lastWordsForLabel("lookupLabel")

  /** Returns a new number to be used as an object tag. */
  def getFreshObjectTag(): Int =
    val tag = objectTagNum
    objectTagNum += 1
    tag

  /** Adds a type into this context. */
  def addType(typeInfo: TypeInfo): TypeIdx =
    val id = typeInfo.id
    types = types + (id -> typeInfo)
    typeInfo.sym match
      case bms: BlockMemberSymbol => namedTypes(bms) = typeInfo
      case _ =>
    TypeIdx(id)

  /** Adds a type into this context. */
  @deprecated("Directly construct the `TypeInfo` using `sym` instead.")
  def addType(sym: Opt[BlockMemberSymbol], typeInfo: TypeInfo): TypeIdx =
    addType(typeInfo)

  @deprecated("Use the overload without `resolveSymIdx` instead.")
  def getType(typeref: TypeIdx | BlockMemberSymbol, resolveSymIdx: Bool): Opt[TypeIdx] =
    if resolveSymIdx then
      typeref match
        case TypeIdx(idx @ SymIdx(_)) =>
          types.zipWithIndex.collectFirst:
            case ((symIdx, _), i) if symIdx == idx => TypeIdx(NumIdx(i))
        case typeidx: TypeIdx => S(typeidx)
        case sym: BlockMemberSymbol =>
          namedTypes.get(sym).flatMap: typeInfo =>
            types.zipWithIndex.collectFirst:
              case ((_, ti), i) if ti === typeInfo => TypeIdx(NumIdx(i))
    else getType(typeref)

  /** Returns the [[TypeIdx]] of the given `typeref`.
    */
  def getType(typeref: TypeIdx | BlockMemberSymbol): Opt[TypeIdx] = typeref match
    case typeidx: TypeIdx => S(typeidx)
    case sym: BlockMemberSymbol => getTypeInfo(typeref).map(ti => TypeIdx(ti.id))

  @deprecated("Use the overload without `resolveSymIdx` instead.")
  def getType_!(typeref: TypeIdx | BlockMemberSymbol, resolveSymIdx: Bool): TypeIdx =
    getType(typeref, resolveSymIdx).getOrElse:
      lastWords(s"Missing type definition for ${typeref.prettyString}")

  /** Same as [[getType]] but throws an exception when the `typeref` is not found. */
  def getType_!(typeref: TypeIdx | BlockMemberSymbol): TypeIdx =
    getType(typeref).getOrElse:
      lastWords(s"Missing type definition for ${typeref.prettyString}")

  /** Returns the [[TypeInfo]] instance associated with the given `typeref`. */
  @nowarn("cat=deprecation")
  def getTypeInfo(typeref: TypeIdx | BlockMemberSymbol): Opt[TypeInfo] = typeref match
    case TypeIdx(NumIdx(idx)) => types.drop(idx).headOption.map(_._2)
    case TypeIdx(idx @ SymIdx(nme)) => types.get(idx)
    case sym: BlockMemberSymbol => namedTypes.get(sym)

  /** Same as [[getTypeInfo]] but throws an exception when the `typeref` is not found. */
  def getTypeInfo_!(typeref: TypeIdx | BlockMemberSymbol): TypeInfo =
    getTypeInfo(typeref).getOrElse:
      lastWords(s"Missing type definition for ${typeref.prettyString}")

  /** Adds a function import into this context.
    *
    * Returns the function index in the global function index space.
    */
  def addFunctionImport(funcImport: Import[ExternType.Func]): FuncIdx =
    val id = funcImport.externType.id
    funcs = funcs + (id -> funcImport)
    funcImport.externType.sym match
      case bms: BlockMemberSymbol => namedFuncs(bms) = funcImport
      case _ =>
    FuncIdx(id)

  @deprecated("Use the `Import[ExternType.Func]` overload instead.")
  def addFunctionImport(sym: Opt[Symbol], funcImport: FuncImport)(using Ctx, Raise): FuncIdx =
    addFunctionImport(
      sym,
      Import(
        funcImport.module,
        funcImport.name,
        ExternType.Func(TypeUse(funcImport.typeIdx), sym.getOrElse(TempSymbol(N, funcImport.id.id))),
      ),
    )

  @deprecated("Use the overload that takes `Import[ExternType.Func]` directly instead.")
  def addFunctionImport(sym: Opt[Symbol], funcImport: Import[ExternType.Func]): FuncIdx =
    val id = funcImport.externType.id
    funcs = funcs + (id -> funcImport)
    sym.foreach:
      namedFuncs(_) = funcImport
    FuncIdx(id)

  @deprecated("Use the `Import[ExternType.Func]` overload instead.")
  @targetName("getOrCreateFuncImport")
  def getOrCreateFunctionImport(
      module: Str,
      name: Str,
  )(createImport: => FuncImport)(using Ctx, Raise): FuncIdx =
    cachedFunctionImports.getOrElseUpdate((module, name), addFunctionImport(N, createImport))

  /** Returns the cached function import for (`module`, `name`), creating it with `createImport` if needed.
    */
  def getOrCreateFunctionImport(
      module: Str,
      name: Str,
  )(createImport: => Import[ExternType.Func]): FuncIdx =
    cachedFunctionImports.getOrElseUpdate((module, name), addFunctionImport(createImport))

  /** Adds or updates a memory import. If the import already exists, its minimum pages are increased to at least
    * `minPages`.
    */
  def ensureMemoryImport(module: Str, name: Str, minPages: Int)(using Ctx, Raise): Unit =
    val key = module -> name
    cachedMemoryImport.get(key) match
      case S(idx) =>
        val existing = memories(idx) match
          case imp: Import[ExternType.Mem] => imp
          case _ => lastWords(
              s"Expected an existing memory import \"$module\".\"$name\" for `${idx.toWat}`, got a definition instead.",
            )
        val newMin = existing.externType.memType.lim.min max minPages
        if newMin > existing.externType.memType.lim.min then
          memories = memories +
            (idx -> existing.copy(
              externType = existing.externType.copy(
                memType = existing.externType.memType.copy(lim = existing.externType.memType.lim.copy(min = newMin)),
              ),
            ))
      case N =>
        val id = SymIdx(name)
        memories = memories +
          (id -> Import(module, name, ExternType.Mem(MemType(Limits(minPages)), TempSymbol(N, id.id))))
        cachedMemoryImport(key) = SymIdx(name)
    end match
  end ensureMemoryImport

  /** Returns the minimum page requirement of memory import (`module`, `name`) if present. */
  @deprecated("Use `getMemoryImport` instead to get the full memory import information.")
  def getMemoryImportMinPages(module: Str, name: Str): Opt[Int] =
    getMemoryImport(module, name).map(_.memType.lim.min)

  /** Returns the memory import information for the given (`module`, `name`) tuple if present. */
  def getMemoryImport(module: Str, name: Str): Opt[ExternType.Mem] =
    memories.collectFirst:
      case (_, imp @ Import(`module`, `name`, mem: ExternType.Mem)) => mem

  /** Adds a data segment into this context. */
  def addDataSegment(seg: DataSegment): Unit =
    dataSegments = dataSegments + (seg.id -> seg)

  /** Adds a tag into this context. */
  def addTag(tagInfo: TagInfo): TagIdx =
    val id = tagInfo.id
    tags = tags + (id -> tagInfo)
    TagIdx(id)

  /** Adds a function into this context. */
  def addFunc(funcInfo: FuncInfo)(using Ctx, Raise): FuncIdx =
    val id = funcInfo.id
    funcs = funcs + (id -> funcInfo)
    funcInfo.sym match
      case bms: BlockMemberSymbol => namedFuncs(bms) = funcInfo
      case _ =>
    val idx = FuncIdx(funcInfo.id)
    val refType = RefType(funcInfo.typeUse.typeIdx, nullable = false)
    elemSegments = elemSegments +
      (id -> ElemSegment.Declare(refType -> Seq(ref.func(idx, refType)), funcInfo.sym))
    idx

  @deprecated("Use the overload that takes `FuncInfo` directly instead.")
  def addFunc(sym: Opt[Symbol], funcInfo: FuncInfo)(using Ctx, Raise): FuncIdx =
    addFunc(funcInfo)

  @deprecated("Use the overload without `resolveSymIdx` instead.")
  def getFunc(funcref: FuncIdx | Symbol, resolveSymIdx: Bool): Opt[FuncIdx] =
    if resolveSymIdx then
      funcref match
        case FuncIdx(idx @ SymIdx(_)) =>
          funcs.zipWithIndex.collectFirst:
            case ((symIdx, _), i) if symIdx == idx => FuncIdx(NumIdx(i))
        case funcidx: FuncIdx => S(funcidx)
        case sym: Symbol =>
          namedFuncs.get(sym).flatMap: funcInfo =>
            funcs.zipWithIndex.collectFirst:
              case ((_, fi), i) if fi === funcInfo => FuncIdx(NumIdx(i))
    else getFunc(funcref)

  /** Returns the [[FuncIdx]] of the given `funcref`.
    */
  def getFunc(funcref: FuncIdx | Symbol): Opt[FuncIdx] = funcref match
    case funcidx: FuncIdx => S(funcidx)
    case sym: Symbol =>
      namedFuncs.get(sym).map: funcInfo =>
        funcInfo match
          case fi: FuncInfo => FuncIdx(fi.id)
          case imp: Import[ExternType.Func] => FuncIdx(imp.externType.id)

  @deprecated("Use the overload without `resolveSymIdx` instead.")
  def getFunc_!(funcref: FuncIdx | Symbol, resolveSymIdx: Bool): FuncIdx =
    getFunc(funcref, resolveSymIdx).getOrElse:
      lastWords(s"Missing function definition for ${funcref.prettyString}")

  /** Same as [[getFunc]] but throws an exception when the `funcref` is not found. */
  def getFunc_!(funcref: FuncIdx | Symbol): FuncIdx =
    getFunc(funcref).getOrElse:
      lastWords(s"Missing function definition for ${funcref.prettyString}")

  /** Returns the [[FuncInfo]] instance associated with the given `funcref`. */
  @nowarn("cat=deprecation")
  def getFuncInfo(funcref: FuncIdx | Symbol): Opt[FuncInfo] =
    val func = funcref match
      case FuncIdx(NumIdx(idx)) => funcs.drop(idx).headOption.map(_._2)
      case FuncIdx(idx @ SymIdx(_)) => funcs.get(idx)
      case funcref: Symbol => namedFuncs.get(funcref)
    func.collect:
      case funcInfo: FuncInfo => funcInfo

  /** Same as [[getFuncInfo]] but throws an exception when the `funcref` is not found. */
  def getFuncInfo_!(funcref: FuncIdx | Symbol): FuncInfo =
    getFuncInfo(funcref).getOrElse:
      lastWords(s"Missing function definition for ${funcref.prettyString}")

  private def lastWordsForLocals(funcName: Str): Nothing =
    lastWords(s"$funcName is no longer supported; Please use genFuncBody instead.")

  @deprecated("Use genFuncBody instead to manage local variables within function bodies.")
  def pushLocal(): Unit = lastWordsForLocals("pushLocal")

  @deprecated("Use genFuncBody instead to manage local variables within function bodies.")
  def popLocal(): Unit = lastWordsForLocals("popLocal")

  /** Adds a new local variable into the top-most variable scope. */
  @deprecated("Use genFuncBody instead to manage local variables within function bodies.")
  def addLocal(sym: Local): LocalIdx = lastWordsForLocals("addLocal")

  /** Adds a [[Seq]] of local variables into the top-most variable scope. */
  @deprecated("Use genFuncBody instead to manage local variables within function bodies.")
  def addLocals(syms: Seq[Local]): Seq[LocalIdx] = lastWordsForLocals("addLocals")

  /** Checks whether the top-most level local variable scope contains the local variable `sym`. */
  @deprecated("Use genFuncBody instead to manage local variables within function bodies.")
  def containsLocal(sym: Local): Bool = lastWordsForLocals("containsLocal")

  /** Adds a new variable into the global variable scope. */
  def addGlobal(globalInfo: GlobalInfo): GlobalIdx =
    val id = globalInfo.id
    globals = globals + (id -> globalInfo)
    namedGlobals(globalInfo.sym) = globalInfo
    GlobalIdx(id)

  /** Adds a [[Seq]] of variables into the global variable scope. */
  def addGlobals(globalDefs: Seq[GlobalInfo]): Seq[GlobalIdx] =
    globalDefs.map(addGlobal)

  /** Checks whether the global variable scope contains the variable `sym`. */
  def containsGlobal(sym: Symbol): Bool = namedGlobals.contains(sym)

  /** Checks whether singleton metadata has been registered for class symbol `sym`. */
  def containsSingleton(sym: BlockMemberSymbol): Bool = singletonByBms.contains(sym)

  /** Returns singleton metadata for `sym` when it resolves to either the block-member symbol or module/object symbol
    * used during singleton registration.
    */
  def getSingletonInfo(sym: Local): Opt[Ctx.SingletonInfo] = sym match
    case bms: BlockMemberSymbol => singletonByBms.get(bms)
    case isym: ModuleOrObjectSymbol => singletonByIsym.get(isym)
    case _ => N

  /** Registers singleton metadata under both its block-member symbol and optional module/object symbol alias.
    */
  def registerSingleton(
      bms: BlockMemberSymbol,
      isym: Opt[ModuleOrObjectSymbol],
      info: Ctx.SingletonInfo,
  ): Unit =
    singletonByBms(bms) = info
    isym.foreach(singletonByIsym(_) = info)

  /** Appends one eager singleton initialization action for synthesized module start code. */
  def addSingletonInitAction(action: Expr): Unit =
    singletonInitActions += action

  /** Returns the singleton initialization actions in deterministic insertion order. */
  def getSingletonInitActions: Seq[Expr] = singletonInitActions.toSeq

  /** Configures the module start function. */
  def setStartFunc(funcIdx: FuncIdx): Unit =
    startFunc = S(funcIdx)

  /** Returns the symbolic index for a global symbol, or `None` if the symbol does not represent a global variable. */
  def getGlobalIndex(sym: Symbol): Opt[GlobalIdx] = globalScp.lookup(sym).map(idx => GlobalIdx(SymIdx(idx)))

  /** Similar to [[getGlobalIndex]] but throws an exception if `sym` is not found. */
  def getGlobalIndex_!(sym: Symbol, loc: Opt[Loc])(using Raise): GlobalIdx =
    GlobalIdx(SymIdx(globalScp.lookup_!(sym, loc)))

  /** Returns all globals in this context.
    */
  def getGlobals: Seq[Symbol] =
    namedGlobals.keys.toSeq

  private def lastWordsForLocalsGlobals(funcName: Str): Nothing =
    lastWords(s"$funcName is no longer supported; Please use genFuncBody and global-related functions in Ctx instead.")

  /** Returns a tuple containing the variables in the current `global` and `local` scopes respectively.
    */
  @deprecated("Use `getGlobals` and `FunctionCtx` instead to get the variables in global and local scopes separately.")
  def getWasmLocals: Seq[Symbol] -> Opt[Seq[Local]] =
    lastWordsForLocalsGlobals("getWasmLocals")

  /** Returns all local variable scopes and their variables. */
  @deprecated("Use `getGlobals` and `FunctionCtx` instead to get the variables in global and local scopes separately.")
  def getAllWasmLocals: Ls[Seq[Local]] =
    lastWordsForLocalsGlobals("getAllWasmLocals")

  /** Returns the cached [[FuncIdx]] for the intrinsic named `name`, creating it with `createIntrinsic` if it does not
    * yet exist in this context.
    */
  def getOrCreateWasmIntrinsic(name: Str, createIntrinsic: => FuncIdx): FuncIdx =
    wasmIntrinsicFuncs.getOrElseUpdate(name, createIntrinsic)

  /** Returns the cached [[TypeIdx]] for the intrinsic type `key`, creating it with `createType` if it does not yet
    * exist in this context.
    */
  def getOrCreateWasmIntrinsicType(key: WasmIntrinsicType)(createType: => TypeIdx): TypeIdx =
    wasmIntrinsicTypes.getOrElseUpdate(key, createType)

  /** Returns the cached [[TagIdx]] for the intrinsic tag named `name`, creating it if absent. */
  def getOrCreateWasmIntrinsicTag(name: Str, createTag: => TagIdx): TagIdx =
    wasmIntrinsicTags.getOrElseUpdate(name, createTag)

  def toWat: Document =
    val memDefns = memories.valuesIterator.collect:
      case memInfo: MemInfo => memInfo.toWat
    val funcDefns = funcs.valuesIterator.collect:
      case funcInfo: FuncInfo => funcInfo.toWat
    doc"(module #{  # ${
        (
          types.valuesIterator.map(_.toWat)
            ++ imports.iterator.map(_.toWat)
            ++ tags.valuesIterator.map(_.toWat)
            ++ globals.valuesIterator.map(_.toWat)
            ++ memDefns
            ++ funcDefns
            ++ dataSegments.valuesIterator.map(_.toWat)
            ++ elemSegments.valuesIterator.map(_.toWat)
            ++ startFunc.iterator.map(funcIdx => doc"(start ${funcIdx.toWat})")
        ).toSeq.mkDocument(doc" # ")
      } #} )"

end Ctx
