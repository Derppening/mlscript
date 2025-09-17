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
  ): FuncInfo = FuncInfo(
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
    doc"""(func ${id.fold(doc"")(_.toWat)}${
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
    locals = ArrayBuf() :: Nil
  )

  def ctx(using ctx: Ctx): Ctx = ctx

private final case class Ctx(
    private val types: ArrayBuf[TypeInfo],
    private val namedTypes: MutMap[Symbol, NumIdx],
    private val funcs: ArrayBuf[FuncInfo],
    private val namedFuncs: MutMap[Symbol, NumIdx],
    var locals: Ls[ArrayBuf[Local]]
) extends ToWat:

  def addType(sym: Opt[Symbol], typeInfo: TypeInfo): TypeIdx =
    val numIdx = NumIdx(types.size)
    types += typeInfo
    sym.foreach:
      namedTypes(_) = numIdx
    TypeIdx(typeInfo.id.getOrElse(numIdx))

  def getTypeInfo(typeref: TypeIdx | Symbol): Opt[TypeInfo] = typeref match
    case TypeIdx(NumIdx(idx)) => types.unapply(idx.toInt)
    case TypeIdx(SymIdx(nme)) =>
      namedTypes.find(_._1.nme == nme).flatMap(t => getTypeInfo(TypeIdx(t._2)))
    case sym: Symbol => namedTypes.get(sym).flatMap(idx => getTypeInfo(TypeIdx(idx)))

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

  def getFuncInfo(funcref: FuncIdx | Symbol): Opt[FuncInfo] = funcref match
    case FuncIdx(NumIdx(idx)) => funcs.unapply(idx.toInt)
    case funcref => getFunc(funcref, resolveSymIdx = true).flatMap(getFuncInfo(_))

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
      warnExpr(Ls(
        msg"WatBuilder::getVar for InnerSymbol not implemented yet" -> l.toLoc,
        msg"Note: Block IR of expression is `${l.toString}`" -> N
      ))
    case _ =>
      val lclIdx = ctx.locals.head.indexWhere(_ == l)
      if lclIdx >= 0 then
        local.get(lclIdx, RefType.anyref)
      else
        warnExpr(Ls(
          msg"WatBuilder::getVar for ${l.getClass.getSimpleName} (symbol not in top-level scope) not implemented yet" -> l.toLoc,
          msg"Note: Block IR of expression is `${l.toString}`" -> N,
          msg"Note: Scope is ${scope.toString}" -> N
        ))

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
          val funcInfo = ctx.getFuncInfo(l).get
          ref.func(funcIdx, RefType(funcInfo.typeIdx, nullable = false))
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
          msg"Expected WAT of `fun` expression in Call(...) to have a `(ref <typeidx>) type" -> r.toLoc,
          msg"Note: Block IR of `fun` expression is `${fun.toString}`" -> N,
          msg"Note: WAT of `fun` expression is `${base.toWat.toString}`" -> N,
          msg"      ... which has an expression type of `${ty.toWat.toString}`" -> N
        ))
      val baseTypeInfo = ctx.getTypeInfo(baseTypeIdx).get

      call_ref(
        target = base,
        operands = wasmArgs.toSeq,
        typeIdx = baseTypeIdx,
        funcType = baseTypeInfo.compType.asInstanceOf[FunctionType]
      )
    case r =>
      warnExpr(Ls(
        msg"WatBackend::result for expression not implemented yet" -> r.toLoc,
        msg"Note: Block IR of expression is `${r.toString}`" -> N
      ))

  def returningTerm(t: Block)(using Ctx, Raise, Scope): Expr = t match
    case _: HandleBlock =>
      errExpr(
        msg"This code requires effect handler instrumentation but was compiled without it." -> t.toLoc
      )
    case Assign(l, r, rst) =>
      val lExpr = getVar(l, t.toLoc)
      if lExpr.exprType is UnreachableType then return lExpr
      val rExpr = result(r)
      val idx = lExpr.instrargs(0).toString.toInt
      val assignExpr = lExpr.mnemonicPrefix match
        case S("global") =>
          warnExpr(Ls(
            msg"WatBuilder::returningTerm for Assign(...) to global variable not implemented yet" -> t.toLoc,
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
        resultType = rstBlk.exprType
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
            defn match
              case FunDefn(own, sym, Nil, body) =>
                lastWords("cannot generate function with no parameter list")
              case FunDefn(own, sym, ps :: pss, bod) =>
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
                        params,
                        results = Seq.fill(bodyWat.exprType.toSeq.length)(Result(RefType.anyref))
                      )
                    )
                  )

                  val funcInfo =
                    FuncInfo(
                      sym,
                      funcTy,
                      ps.params.map(p => p.sym -> scope.lookup_!(p.sym, p.toLoc)),
                      bodyWat.exprType.toSeq.length,
                      locals.map(l => l -> scope.lookup_!(l, l.toLoc)),
                      bodyWat
                    )
                  val func = ctx.addFunc(S(defn.sym), funcInfo)

                  nop
                else
                  warnExpr(Ls(
                    msg"WatBuilder::returningTerm for FunDefn(...) where `!sym.nameIsMeaningful` not implemented yet" -> t.toLoc,
                    msg"Note: Block IR of definition is `${defn.toString}`" -> N
                  ))
              case defn =>
                warnExpr(Ls(
                  msg"WatBuilder::returningTerm for Define(...) not implemented yet" -> t.toLoc,
                  msg"Note: Block IR of definition is `${defn.toString}`" -> N
                ))
          end val

          val rstBlk = returningTerm(rst)
          thisProxy match
            case S(proxy) if !scope.thisProxyDefined =>
              scope.thisProxyDefined = true
              warnExpr(Ls(
                msg"WatBuilder::returningTerm for Define(...) where `!scope.thisProxyDefined` not implemented yet" -> t.toLoc,
                msg"Note: Block IR of definition is `${defn.toString}`" -> N
              ))
            case _ => Instructions.block(
                label = N,
                children = Seq(res, rstBlk),
                resultType = rstBlk.exprType
              )

        case defn =>
          warnExpr(Ls(
            msg"WatBuilder::returningTerm for Define(...) not implemented yet" -> t.toLoc,
            msg"Note: Block IR of expression is `${t.toString}`" -> N
          ))
    case Return(res, true) =>
      result(res)
    case Return(res, false) =>
      `return`(S(result(res)))
    case t =>
      warnExpr(Ls(
        msg"WatBuilder::returningTerm for expression not implemented yet" -> t.toLoc,
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
    .to(ArrayBuf)
    ctx.locals.head ++= vars
    vars.toSeq

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
  ): (Seq[WasmParam], Expr, Seq[Local]) =
    // Add a frame for `ctx.locals`
    ctx.locals = ctx.locals match
      case globals :: Nil => ArrayBuf() :: globals :: Nil
      case locals @ (_ :: _ :: Nil) => locals
      case _ => lastWords(s"ctx.locals should only have 1-2 local scopes")

    scope.nest givenIn:
      val paramsList = params.params.map(p =>
        WasmParam(S(scope.allocateName(p.sym)), RefType.anyref)
      )
      val (wasmParams, (wasmBody, locals)) = (paramsList.toSeq, this.body(body))

      // Restore `ctx.locals`
      ctx.locals = ctx.locals.tail

      (wasmParams, wasmBody, locals)

end WatBuilder
