package hkmc2
package codegen
package wasm
package text

import mlscript.utils.*, shorthands.*
import hkmc2.utils.*

import document.*
import document.Document
import js.CodeBuilder
import semantics.*, Elaborator.State
import syntax.Tree.{BoolLit, IntLit}
import text.Param as WasmParam
import Message.MessageContext
import Scope.scope
import Value.Lam

import scala.collection.Map
import scala.collection.mutable.{ArrayBuffer as ArrayBuf, Map as MutMap}
import scala.util.boundary, boundary.break

extension (doc: Document)
  private def surroundUnlessEmpty(
      prefix: Document = Document.empty,
      postfix: Document = Document.empty
  ): Document =
    doc.optionUnless(_.isEmpty).fold(doc):
      prefix :: _ :: postfix

extension (instr: FoldedInstr)
  private def mnemonicPrefix: Opt[Str] =
    instr.mnemonic.split('.').optionUnless(_.size == 1).map(_.head)

object FuncInfo:
  def apply(
      sym: BlockMemberSymbol,
      typeIdx: TypeIdx,
      params: Seq[Local -> Str],
      nResults: Int,
      locals: Seq[Local -> Str],
      body: FoldedInstr
  ): FuncInfo = new FuncInfo(
    sym.optionIf(_.nameIsMeaningful).map(sym => SymIdx(sym.nme)),
    typeIdx,
    params,
    nResults,
    locals,
    body
  )

private final case class FuncInfo(
    val id: Opt[SymIdx],
    val typeIdx: TypeIdx,
    val params: Seq[Local -> Str],
    val nResults: Int,
    val locals: Seq[Local -> Str],
    val body: FoldedInstr
) extends ToWat:
  def getSignatureType: SignatureType = SignatureType(
    params = params.map((_, varNme) => WasmParam(S(varNme), RefType.anyref)),
    results = Seq.fill(nResults)(Result(RefType.anyref))
  )

  def toWat: Document =
    doc"""(func ${id.fold(doc"")(_.toWat)} (type ${typeIdx.toWat})${
        getSignatureType.toWat.surroundUnlessEmpty(doc" ")
      } #{ ${
        locals.map: p =>
          doc"(local $$${p._2} ${RefType.anyref.toWat})"
        .toSeq.mkDocument(doc" # ").surroundUnlessEmpty(doc" # ")
      } # ${body.toWat} #} )${
        id.fold(doc""): id =>
          doc""" # (export "${id.id}" (func ${id.toWat})) # (elem declare func ${id.toWat})"""
      }"""
end FuncInfo

object TypeInfo:
  def apply(sym: BlockMemberSymbol, compType: CompType): TypeInfo = new TypeInfo(
    sym.optionIf(_.nameIsMeaningful).map(sym => SymIdx(sym.nme)),
    compType
  )

private final case class TypeInfo(
    val id: Opt[SymIdx],
    val compType: CompType
) extends ToWat:

  def toWat: Document =
    doc"(type ${id.fold(doc"")(_.toWat).surroundUnlessEmpty(postfix = doc" ")}${compType.toWat})"
end TypeInfo

object Ctx:
  def empty: Ctx = Ctx(
    types = ArrayBuf.empty,
    namedTypes = MutMap.empty,
    funcs = ArrayBuf.empty,
    namedFuncs = MutMap.empty,
    locals = MutMap() :: Nil
  )

  def ctx(using ctx: Ctx): Ctx = ctx

  extension (ref: CtxIdx | Symbol)
    private def prettyString: Str = ref match
      case idx: CtxIdx => s"type index `${idx.toWat.toString}`"
      case sym: Symbol => s"symbol `sym.toString`"

private final case class Ctx(
    private val types: ArrayBuf[TypeInfo],
    private val namedTypes: MutMap[Symbol, NumIdx],
    private val funcs: ArrayBuf[FuncInfo],
    private val namedFuncs: MutMap[Symbol, NumIdx],
    private var locals: Ls[MutMap[Local, NumIdx]]
) extends ToWat:

  import Ctx.prettyString

  def addType(sym: Opt[Symbol], typeInfo: TypeInfo): TypeIdx =
    val numIdx = NumIdx(types.size)
    types += typeInfo
    sym.foreach:
      namedTypes(_) = numIdx
    TypeIdx(typeInfo.id.getOrElse(numIdx))

  def getType(typeref: TypeIdx | Symbol, resolveSymIdx: Bool = false): Opt[TypeIdx] = typeref match
    case TypeIdx(SymIdx(nme)) if resolveSymIdx =>
      namedTypes.find(_._1.nme == nme).map(t => TypeIdx(t._2))
    case typeidx: TypeIdx => S(typeidx)
    case sym: Symbol if resolveSymIdx => namedTypes.get(sym).map(TypeIdx(_))
    case sym: Symbol =>
      getType(sym, resolveSymIdx = true).map: numIdx =>
        getTypeInfo(numIdx).flatMap(_.id).fold(numIdx)(TypeIdx(_))

  def getType_!(typeref: TypeIdx | Symbol, resolveSymIdx: Bool = false): TypeIdx =
    getType(typeref, resolveSymIdx).getOrElse:
      lastWords(s"Missing type definition for ${typeref.prettyString}")

  def getTypeInfo(typeref: TypeIdx | Symbol): Opt[TypeInfo] = typeref match
    case TypeIdx(NumIdx(idx)) => types.unapply(idx.toInt)
    case TypeIdx(SymIdx(nme)) =>
      namedTypes.find(_._1.nme == nme).flatMap(t => getTypeInfo(TypeIdx(t._2)))
    case sym: Symbol => namedTypes.get(sym).flatMap(idx => getTypeInfo(TypeIdx(idx)))

  def getTypeInfo_!(typeref: TypeIdx | Symbol): TypeInfo =
    getTypeInfo(typeref).getOrElse:
      lastWords(s"Missing type definition for ${typeref.prettyString}")

  def addFunc(sym: Opt[Symbol], funcInfo: FuncInfo): FuncIdx =
    val numIdx = NumIdx(funcs.size)
    funcs += funcInfo
    sym.foreach:
      namedFuncs(_) = numIdx
    FuncIdx(funcInfo.id.getOrElse(numIdx))

  def getFunc(funcref: FuncIdx | Symbol, resolveSymIdx: Bool = false): Opt[FuncIdx] = funcref match
    case FuncIdx(SymIdx(nme)) if resolveSymIdx =>
      namedFuncs.find(_._1.nme == nme).map(f => FuncIdx(f._2))
    case funcidx: FuncIdx => S(funcidx)
    case sym: Symbol if resolveSymIdx => namedFuncs.get(sym).map(FuncIdx(_))
    case sym: Symbol =>
      getFunc(sym, resolveSymIdx = true).map: numIdx =>
        getFuncInfo(numIdx).flatMap(_.id).fold(numIdx)(FuncIdx(_))

  def getFunc_!(funcref: FuncIdx | Symbol, resolveSymIdx: Bool = false): FuncIdx =
    getFunc(funcref, resolveSymIdx).getOrElse:
      lastWords(s"Missing function definition for ${funcref.prettyString}")

  def getFuncInfo(funcref: FuncIdx | Symbol): Opt[FuncInfo] = funcref match
    case FuncIdx(NumIdx(idx)) => funcs.unapply(idx.toInt)
    case funcref => getFunc(funcref, resolveSymIdx = true).flatMap(getFuncInfo(_))

  def getFuncInfo_!(funcref: FuncIdx | Symbol): FuncInfo =
    getFuncInfo(funcref).getOrElse:
      lastWords(s"Missing function definition for ${funcref.prettyString}")

  def pushLocal(): Unit = locals = MutMap() :: locals
  def popLocal(): Unit = locals = locals.tail

  def addLocal(sym: Local): LocalIdx =
    val numIdx = NumIdx(locals.head.size)
    locals.head(sym) = numIdx
    LocalIdx(numIdx)

  def addLocals(syms: Seq[Local]): Seq[LocalIdx] =
    syms.map(addLocal)

  def containsLocal(sym: Local): Bool = locals.head.contains(sym)

  def getLocals: Ls[Seq[Local]] = locals.map(_.toSeq.sortBy(_._2.index).map(_._1))

  def toWat: Document =
    doc"""(module #{  # ${(types.toSeq ++ funcs.toSeq).map(_.toWat).mkDocument(doc" # ")}) #} """

end Ctx

final class WatBuilder(using TraceLogger, State) extends CodeBuilder:
  import Ctx.ctx
  import Instructions.*

  type Context = Unit
  type Expr = FoldedInstr

  def warnExpr(warnMsg: Message -> Opt[Loc])(using Ctx, Raise): Expr =
    warnExpr(warnMsg :: Nil)

  def warnExpr(warnMsgs: Ls[Message -> Opt[Loc]])(using Ctx, Raise): Expr =
    raise(
      WarningReport(warnMsgs, source = Diagnostic.Source.Compilation)
    )
    unreachable

  def errExpr(errMsg: Message -> Opt[Loc])(using Ctx, Raise): Expr =
    errExpr(errMsg :: Nil)

  def errExpr(errMsgs: Ls[Message -> Opt[Loc]])(using Ctx, Raise): Expr =
    raise(
      ErrorReport(errMsgs, source = Diagnostic.Source.Compilation)
    )
    unreachable

  def getVar(l: Local, loc: Opt[Loc])(using Ctx, Raise, Scope): Expr = l match
    case ts: semantics.TermSymbol =>
      warnExpr(Ls(
        msg"WatBuilder::getVar for TermSymbol not implemented yet" -> l.toLoc,
        msg"Note: Block IR of expression is `${l.toString}`" -> N
      ))
    case ts: semantics.ModuleOrObjectSymbol if ts.asMod.isDefined =>
      warnExpr(Ls(
        msg"WatBuilder::getVar for ModuleOrObjectSymbol (`ts.asMod.isDefined`) not implemented yet" -> l.toLoc,
        msg"Note: Block IR of expression is `${l.toString}`" -> N
      ))
    case ts: semantics.InnerSymbol =>
      if !ctx.containsLocal(l) then
        return warnExpr(Ls(
          msg"WatBuilder::getVar for InnerSymbol (symbol not in top-level scope) not implemented yet" -> ts.toLoc,
          msg"Note: Block IR of expression is `${ts.toString}`" -> N,
          msg"Note: Scope is ${scope.toString}" -> N,
          msg"Note: Locals is ${ctx.getLocals.toString}" -> N
        ))
      local.get(LocalIdx(SymIdx(scope.findThis_!(ts))), RefType.anyref)
    case l =>
      if !ctx.containsLocal(l) then
        return warnExpr(Ls(
          msg"WatBuilder::getVar for ${l.getClass.getSimpleName} (symbol not in top-level scope) not implemented yet" -> l.toLoc,
          msg"Note: Block IR of expression is `${l.toString}`" -> N,
          msg"Note: Scope is ${scope.toString}" -> N,
          msg"Note: Locals is ${ctx.getLocals.toString}" -> N
        ))
      local.get(LocalIdx(SymIdx(scope.lookup_!(l, l.toLoc))), RefType.anyref)

  def argument(a: Arg)(using Ctx, Raise, Scope): Expr =
    if a.spread.nonEmpty then
      warnExpr(Ls(
        msg"WatBackend::argument for spread expression not implemented yet" -> a.value.toLoc,
        msg"Note: Block IR of expression is `${a.toString}`" -> N
      ))
      unreachable
    else result(a.value)

  def operand(a: Arg)(using Ctx, Raise, Scope): Expr =
    if a.spread.nonEmpty then die else subexpression(a.value)

  def subexpression(r: codegen.Result)(using Ctx, Raise, Scope): Expr = r match
    case r: Value.Lam =>
      warnExpr(Ls(
        msg"WatBuilder::subexpression for Value.Lam not implemented yet" -> r.toLoc,
        msg"Note: Block IR of expression is `${r.toString}`" -> N
      ))
    case r => result(r)

  def result(r: codegen.Result)(using Ctx, Raise, Scope): Expr = r match
    case Value.Lit(BoolLit(value)) =>
      ref.i31(i32.const(if value then 1 else 0))
    case Value.Lit(IntLit(value)) =>
      ref.i31(i32.const(value.toInt))
    case Value.Ref(l) =>
      ctx.getFunc(l) match
        case S(funcIdx) =>
          ref.func(funcIdx, RefType(ctx.getFuncInfo_!(l).typeIdx, nullable = false))
        case N => getVar(l, r.toLoc)

    case Call(Value.Ref(l: BuiltinSymbol), lhs :: rhs :: Nil)
        if !l.functionLike =>
      if l.binary then
        l.nme match
          case "+" =>
            // TODO(Derppening): Refactor to lower to `Call(plus_impl, ...)`
            def castOperand(expr: FoldedInstr, opSide: Str): FoldedInstr =
              expr.exprType match
                case RefType(HeapType.Any, _) => `if`(
                    ref.test(expr, RefType.i31ref),
                    ifTrue =
                      castOperand(ref.cast(expr, RefType.i31ref), opSide),
                    ifFalse = S(unreachable)
                  )
                case RefType(HeapType.I31, _) => i31.get(expr, true)
                case I32Type => expr
                case ty =>
                  warnExpr(Ls(
                    msg"WatBuilder::result for binary builtin symbol '${l.nme.toString}' ($opSide.type=${ty.toWat.toString}) not implemented yet" -> r.toLoc,
                    msg"Note: Block IR of expression is `${r.toString}`" -> N
                  ))
                  unreachable

            val lhsOp = castOperand(operand(lhs), "lhs")
            val rhsOp = castOperand(operand(rhs), "rhs")

            (lhsOp.exprType, rhsOp.exprType) match
              case (I32Type, I32Type) =>
                ref.i31(i32.add(lhsOp, rhsOp))
              case (lhsType, rhsType) =>
                warnExpr(Ls(
                  msg"WatBuilder::result for binary builtin symbol '${l.nme.toString}' for (${lhsType.toWat.toString}, ${rhsType.toWat.toString}) not implemented yet" -> r.toLoc,
                  msg"Note: Block IR of expression is `${r.toString}`" -> N
                ))
                unreachable
          case lNme =>
            warnExpr(Ls(
              msg"WatBuilder::result for binary builtin symbol '${lNme.toString}' not implemented yet" -> r.toLoc,
              msg"Note: Block IR of expression is `${r.toString}`" -> N
            ))
      else
        errExpr(
          msg"Cannot call non-binary builtin symbol '${l.nme}'" -> r.toLoc
        )
    case Call(fun, args) =>
      val base = subexpression(fun)
      if base.exprType is UnreachableType then return base
      val wasmArgs = args.map(argument)

      val baseTypeIdx = base.exprType match
        case RefType(idx: TypeIdx, _) => idx
        case ty => return errExpr(Ls(
            msg"Expected WAT of `fun` expression in Call(...) to have a `(ref <typeidx>)` type" -> r.toLoc,
            msg"Note: Block IR of `fun` expression is `${fun.toString}`" -> N,
            msg"Note: WAT of `fun` expression is `${base.toWat.toString}`" -> N,
            msg"      ... which has an expression type of `${ty.toWat.toString}`" -> N
          ))
      val baseTypeInfo = ctx.getTypeInfo_!(baseTypeIdx)

      call_ref(
        target = base,
        operands = wasmArgs.toSeq,
        typeIdx = baseTypeIdx,
        funcType = baseTypeInfo.compType.asInstanceOf[FunctionType]
      )

    case Instantiate(_, cls, as) =>
      val ctorClsPath = cls match
        case sel: Select => sel
        case cls => return warnExpr(Ls(
            msg"WatBuilder::result for Instantiate(...) where `cls` is not a Select(...) path not implemented yet " -> cls.toLoc,
            msg"Note: Block IR of `cls` expression is `${cls.toString}`" -> N
          ))
      val ctorClsSym = ctorClsPath.symbol match
        case S(sym) => sym
        case N => return errExpr(Ls(
            msg"Class path for an Instantiate(...) expression must be resolved" -> cls.toLoc,
            msg"Note: Block IR of `cls` expression is `${cls.toString}`" -> N
          ))
      val ctorClsBlkSym = ctorClsSym.asBlkMember match
        case S(sym) => sym
        case N => lastWords(
            s"Expected resolved class for an Instantiate(...) expression to be a BlockMemberSymbol, but got ${ctorClsSym.getClass.getName}"
          )
      val ctorFuncIdx = ctx.getFunc(ctorClsBlkSym) match
        case S(idx) => idx
        case N => lastWords(s"Missing constructor definition for class ${ctorClsBlkSym.toString}")

      val objType = ctx.getFuncInfo_!(ctorFuncIdx).body.exprType
      call(funcidx = ctorFuncIdx, as.map(argument), Seq(Result(objType.asInstanceOf[ValType])))

    case r =>
      warnExpr(Ls(
        msg"WatBackend::result for expression not implemented yet" -> r.toLoc,
        msg"Note: Block IR of expression is `${r.toString}`" -> N
      ))

  def returningTerm(t: Block)(using Ctx, Raise, Scope): Expr = t match
    case _: HandleBlock =>
      errExpr(
        msg"This code requires effect handler instrumentation but was compiled without it." -> N
      )
    case Assign(l, r, rst) =>
      val lExpr = getVar(l, l.toLoc)
      if lExpr.exprType is UnreachableType then return lExpr
      val rExpr = result(r)
      val idx = lExpr.instrargs(0).asInstanceOf[LocalIdx]
      val assignExpr = lExpr.mnemonicPrefix match
        case S("global") =>
          warnExpr(Ls(
            msg"WatBuilder::returningTerm for Assign(...) to global variable not implemented yet" -> l.toLoc,
            msg"Note: Block IR of expression is `${t.toString}`" -> N
          ))
        case S("local") => local.set(idx, rExpr)
        case _ =>
          lastWords(
            s"Expected `global.*` or `local.*` when compiling instruction for `$l`, but got ${lExpr.mnemonic}"
          )
      val rstBlk = returningTerm(rst)

      Instructions.block(
        label = N,
        children = Seq(assignExpr, rstBlk),
        resultTypes = rstBlk.exprType match
          case NoneType => Seq.empty
          case MultiValueType(tys) => tys.map(ty => Result(ty.asInstanceOf[ValType]))
          case ty => Seq(Result(ty.asInstanceOf[ValType]))
      )

    case Define(defn, rst) =>
      def mkThis(sym: InnerSymbol): Expr = result(Value.This(sym))
      defn match
        case defn: (FunDefn | ClsLikeDefn) =>
          val outerScope = scope
          val (thisProxy, res) = scope.nestRebindThis(
            // * Either this is an InnerSymbol or this is a Fun,
            // * and we need to rebind `this` to None to shadow it.
            defn.innerSym.collectFirst:
              case s: InnerSymbol => s
          ):
            boundary:
              defn match
                case FunDefn(own, sym, Nil, body) =>
                  lastWords("cannot generate function with no parameter list")
                case FunDefn(own, sym, ps :: pss, bod) =>
                  if own.nonEmpty then
                    break(warnExpr(Ls(
                      msg"WatBuilder::returningTerm for Define(...) with `owner.nonEmpty` not implemented yet" -> defn.sym.toLoc,
                      msg"Note: Block IR of definition is `${defn.toString}`" -> N
                    )))

                  val result = pss.foldRight(bod):
                    case (ps, block) =>
                      Return(Lam(ps, block), false)
                  val name = if sym.nameIsMeaningful then S(sym.nme) else N
                  val (params, bodyWat, locals) = setupFunction(name, ps, result)
                  if sym.nameIsMeaningful then
                    val funcTy = ctx.addType(
                      sym = N,
                      TypeInfo(
                        id = N,
                        FunctionType(
                          params = params.map(_._1),
                          results = Seq.fill(bodyWat.exprType.toSeq.length)(Result(RefType.anyref))
                        )
                      )
                    )

                    val funcInfo =
                      FuncInfo(
                        sym,
                        funcTy,
                        ps.params.zip(params.map(_._2)).map((p, nme) => p.sym -> nme),
                        bodyWat.exprType.toSeq.length,
                        locals.map(l => l -> scope.lookup_!(l, l.toLoc)),
                        bodyWat
                      )
                    val func = ctx.addFunc(S(defn.sym), funcInfo)

                    nop
                  else
                    warnExpr(Ls(
                      msg"WatBuilder::returningTerm for FunDefn(...) where `!sym.nameIsMeaningful` not implemented yet" -> defn.sym.toLoc,
                      msg"Note: Block IR of definition is `${defn.toString}`" -> N
                    ))
                case clsLikeDefn: ClsLikeDefn =>
                  // Guard against unsupported features
                  def warnUnimplExpr(cond: Str): Nothing = break(warnExpr(Ls(
                    msg"WatBackend::returningTerm for ClsLikeDefn(...) where `$cond` not implemented yet" -> clsLikeDefn.sym.toLoc,
                    msg"Note: Block IR of definition is `${defn.toString}`" -> N
                  )))
                  if clsLikeDefn.owner.nonEmpty then
                    break(warnUnimplExpr("owner.nonEmpty"))
                  if !(clsLikeDefn.k is syntax.Cls) then
                    break(warnUnimplExpr("!(k is Cls)"))
                  if clsLikeDefn.auxParams.nonEmpty then
                    break(warnUnimplExpr("auxParams.nonEmpty"))
                  if clsLikeDefn.parentPath.nonEmpty then
                    break(warnUnimplExpr("parentPath.nonEmpty"))
                  if clsLikeDefn.methods.nonEmpty then
                    break(warnUnimplExpr("methods.nonEmpty"))
                  clsLikeDefn.preCtor match
                    case End(_) => ()
                    case _ => break(warnUnimplExpr("preCtor is not End"))
                  if clsLikeDefn.companion.isDefined then
                    break(warnUnimplExpr("companion.isDefined"))

                  val clsParams = clsLikeDefn.paramsOpt.fold(Nil)(_.paramSyms)
                  val ctorParams = clsParams.map: p =>
                    p -> scope.allocateName(p)
                  val ctorAuxParams = clsLikeDefn.auxParams.map: ps =>
                    ps.params.map: p =>
                      p -> scope.allocateName(p.sym)

                  val typeref = ctx.addType(
                    sym = S(clsLikeDefn.sym),
                    typeInfo =
                      TypeInfo(
                        sym = clsLikeDefn.sym,
                        compType = StructType(
                          (clsLikeDefn.publicFields.map(_._2) ++ clsLikeDefn.privateFields).map:
                            f =>
                              Field(RefType.anyref, mutable = true, id = S(f.nme))
                        )
                      )
                  )

                  // * If there are no ctor params, pop one param list off the aux params
                  val (newCtorAuxParams, initialCtorParams) = clsLikeDefn.paramsOpt match
                    case None => ctorAuxParams match
                        case head :: next => (next, head)
                        case Nil => (ctorAuxParams, Nil)
                    case Some(_) => (ctorAuxParams, ctorParams)

                  ctx.addLocal(clsLikeDefn.isym)
                  val thisVar = getVar(clsLikeDefn.isym, N).instrargs(0).asInstanceOf[LocalIdx]
                  val ctorCode = Instructions.block(
                    label = N,
                    Seq(
                      local.set(thisVar, struct.new_default(typeref)),
                      // TODO(Derppening): block(ctor)
                      nop,
                      `return`(S(local.get(thisVar, RefType(typeref, nullable = false))))
                    ),
                    resultTypes = Seq(Result(RefType(typeref, nullable = false)))
                  )

                  val ctorAux = if newCtorAuxParams.isEmpty then
                    ctorCode
                  else
                    break(warnUnimplExpr("newCtorAuxParams.nonEmpty"))

                  val funcTy = ctx.addType(
                    sym = N,
                    TypeInfo(
                      id = N,
                      FunctionType(
                        params = ctorParams.map(p => WasmParam(S(p._2), RefType.anyref)),
                        results = Seq(Result(RefType.anyref))
                      )
                    )
                  )

                  ctx.addFunc(
                    S(clsLikeDefn.sym),
                    FuncInfo(
                      sym = clsLikeDefn.sym,
                      typeIdx = funcTy,
                      params = ctorParams,
                      nResults = ctorCode.exprType.toSeq.length,
                      locals = Seq(clsLikeDefn.isym -> scope.findThis_!(clsLikeDefn.isym)),
                      body = ctorAux
                    )
                  )

                  nop

                case defn =>
                  warnExpr(Ls(
                    msg"WatBuilder::returningTerm for Define(...) not implemented yet" -> defn.sym.toLoc,
                    msg"Note: Block IR of definition is `${defn.toString}`" -> N
                  ))
          end val

          val rstBlk = returningTerm(rst)
          thisProxy match
            case S(proxy) if !scope.thisProxyDefined =>
              scope.thisProxyDefined = true
              warnExpr(Ls(
                msg"WatBuilder::returningTerm for Define(...) where `!scope.thisProxyDefined` not implemented yet" -> defn.sym.toLoc,
                msg"Note: Block IR of definition is `${defn.toString}`" -> N
              ))
            case _ => Instructions.block(
                label = N,
                children = Seq(res, rstBlk),
                resultTypes = rstBlk.exprType match
                  case NoneType => Seq.empty
                  case MultiValueType(tys) => tys.map(ty => Result(ty.asInstanceOf[ValType]))
                  case ty => Seq(Result(ty.asInstanceOf[ValType]))
              )

        case defn =>
          warnExpr(Ls(
            msg"WatBuilder::returningTerm for Define(...) not implemented yet" -> defn.sym.toLoc,
            msg"Note: Block IR of expression is `${t.toString}`" -> N
          ))
    case Return(res, true) =>
      result(res)
    case Return(res, false) =>
      `return`(S(result(res)))
    case t =>
      warnExpr(Ls(
        msg"WatBuilder::returningTerm for expression not implemented yet" -> N,
        msg"Note: Block IR of expression is `${t.toString}`" -> N
      ))

  def program(p: Program, exprt: Opt[BlockMemberSymbol], wd: os.Path)(using
      Raise,
      Scope
  ): (Document, Str) =
    for imprt <- p.imports do
      raise(
        WarningReport(
          msg"Import of symbol `${imprt._2}` not implemented yet" -> imprt._1.toLoc :: Nil,
          source = Diagnostic.Source.Compilation
        )
      )
    exprt.foreach: exprt =>
      raise(
        WarningReport(
          msg"Export of symbol `${exprt.nme}` not implemented yet" -> exprt.toLoc :: Nil,
          source = Diagnostic.Source.Compilation
        )
      )

    val ctx = Ctx.empty
    val (entryFnExpr, entryFnLocals) =
      block(p.main)(using ctx, summon[Raise], summon[Scope])

    val entrySym = BlockMemberSymbol("entry", Nil)
    val entryNme = scope.allocateName(entrySym)

    val entryFnTy = ctx.addType(
      sym = N,
      TypeInfo(id = N, FunctionType(params = Seq.empty, results = Seq(Result(RefType.anyref))))
    )
    val entryFnInfo = FuncInfo(
      id = S(SymIdx(entryNme)),
      typeIdx = entryFnTy,
      params = Seq.empty,
      nResults = 1,
      // TODO(Derppening): Should we place top-level scope variables in the global section?
      locals = entryFnLocals.map(l => l -> scope.lookup_!(l, l.toLoc)),
      entryFnExpr
    )
    ctx.addFunc(S(entrySym), entryFnInfo)

    (ctx.toWat, entryNme)

  def blockPreamble(ss: Iterable[Symbol])(using Ctx, Raise, Scope): Seq[Local] =
    val vars = ss.filter(
      scope.lookup(_).toSeq.isEmpty
    ).toSeq.toArray.sortBy(_.uid).iterator.map: l =>
      scope.allocateName(l)
      l
    .toSeq
    ctx.addLocals(vars)
    vars

  def block(t: Block)(using Ctx, Raise, Scope): (Expr, Seq[Local]) =
    val locals = blockPreamble(t.definedVars)
    (returningTerm(t), locals)

  def body(t: Block)(using Ctx, Raise, Scope): (Expr, Seq[Local]) =
    scope.nest givenIn:
      block(t)

  def setupFunction(name: Option[Str], params: ParamList, body: Block)(using
      Ctx,
      Raise,
      Scope
  ): (Seq[WasmParam -> Str], Expr, Seq[Local]) =
    // Add a frame for `ctx.locals`
    ctx.pushLocal()

    val result = scope.nest givenIn:
      val paramsList = params.params.map: p =>
        val paramNme = scope.allocateName(p.sym)
        val param = WasmParam(S(paramNme), RefType.anyref)
        ctx.addLocal(p.sym)
        param -> paramNme
      val (wasmParams, (wasmBody, locals)) = (paramsList.toSeq, this.body(body))
      (wasmParams, wasmBody, locals)

    // Restore `ctx.locals`
    ctx.popLocal()

    result

end WatBuilder
