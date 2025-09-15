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

object FuncInfo:
  def apply(
      name: BlockMemberSymbol,
      params: Seq[Local],
      nResults: Int,
      locals: Seq[Local -> WasmType],
      body: FoldedInstr
  ): FuncInfo = FuncInfo(
    FuncRef(name.nme),
    params,
    nResults,
    locals,
    body
  )

private final case class FuncInfo(
    val id: FuncRef,
    val params: Seq[Local],
    val nResults: Int,
    val locals: Seq[Local -> WasmType],
    val body: FoldedInstr
) extends ToWat:
  def toWat: Document =
    doc"""(func ${id.toWat}${
        params.map(p =>
          doc"(param ${p.nme} ${RefType.anyref.toWat})"
        ).toSeq.mkDocument(doc" ").surroundUnlessEmpty(doc" ")
      }${
        Seq.fill(nResults)(
          doc"(result ${RefType.anyref.toWat})"
        ).mkDocument(doc" ").surroundUnlessEmpty(doc" ")
      } #{ ${
        locals.map(p => doc"(local ${p._2.toWat})").toSeq.mkDocument(
          doc" # "
        ).surroundUnlessEmpty(doc" # ")
      } # ${body.toWat} #} ) # (export "${id.id}" (func ${id.toWat})) # (elem declare func ${id.toWat})"""
end FuncInfo

private final case class TypeInfo(
    val id: Opt[TypeId],
    val compType: CompType
) extends ToWat:

  def toWat: Document =
    doc"(type ${id.fold(doc"")(_.toWat).surroundUnlessEmpty(postfix = doc" ")}${compType.toWat})"
end TypeInfo

object Ctx:
  def empty: Ctx = Ctx(
    _types = MutMap.empty,
    _anonTypes = ArrayBuf.empty,
    funcs = MutMap.empty,
    locals = ArrayBuf() :: Nil
  )

  def ctx(using ctx: Ctx): Ctx = ctx

private final case class Ctx(
    private val _types: MutMap[Symbol, TypeIdx -> TypeInfo],
    private val _anonTypes: ArrayBuf[TypeIdx -> TypeInfo],
    val funcs: MutMap[Symbol, FuncInfo],
    var locals: Ls[ArrayBuf[Local -> ValType]]
) extends ToWat:

  def types: Map[Symbol, TypeIdx -> TypeInfo] = _types
  def anonTypes: Seq[TypeIdx -> TypeInfo] = _anonTypes.toSeq

  def addType(sym: Opt[Symbol], typeInfo: TypeInfo): TypeRef =
    val typeEntry = TypeIdx(_types.size + _anonTypes.size) -> typeInfo
    sym match
      case S(sym) => _types(sym) = typeEntry
      case N => _anonTypes += typeEntry
    typeInfo.id.getOrElse(typeEntry._1)

  def toWat: Document =
    doc"""(module #{  # ${(_types.values ++ _anonTypes).map(
        _._2.toWat
      ).toSeq.mkDocument(doc" # ").surroundUnlessEmpty(postfix =
        doc" # "
      )}${funcs.values.toSeq.map(
        _.toWat
      ).mkDocument(doc" # ")}) #} """

end Ctx

final class WatBuilder(using TraceLogger, State) extends CodeBuilder:
  import Ctx.ctx
  import Instructions.*

  type Context = Unit
  type Expr = Opt[FoldedInstr]

  def warnExpr(warnMsg: Message -> Opt[Loc])(using Ctx, Raise): Expr =
    warnExpr(warnMsg :: Nil)

  def warnExpr(warnMsgs: Ls[Message -> Opt[Loc]])(using Ctx, Raise): Expr =
    raise(
      WarningReport(warnMsgs, source = Diagnostic.Source.Compilation)
    )
    S(unreachable)

  def errExpr(errMsg: Message -> Opt[Loc])(using Ctx, Raise): Expr =
    raise(
      ErrorReport(errMsg :: Nil, source = Diagnostic.Source.Compilation)
    )
    S(unreachable)

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
      val lclIdx = ctx.locals.head.indexWhere(_._1 == l)
      if lclIdx >= 0 then
        S(local.get(lclIdx, ctx.locals.head(lclIdx)._2))
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
      S(unreachable)
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
      S(ref.i31(i32.const(if value then 1 else 0)))
    case Value.Lit(IntLit(value)) =>
      S(ref.i31(i32.const(value.toInt)))
    case Value.Ref(l) => getVar(l, r.toLoc)

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

            val lhsOp = castOperand(operand(lhs).get, "lhs")
            val rhsOp = castOperand(operand(rhs).get, "rhs")

            S(
              (lhsOp.exprType, rhsOp.exprType) match
                case (I32Type, I32Type) =>
                  ref.i31(i32.add(lhsOp, rhsOp))
                case (lhsType, rhsType) =>
                  warnExpr(Ls(
                    msg"WatBuilder::result for binary builtin symbol '${l.nme.toString}' for (${lhsType.toWat.toString}, ${rhsType.toWat.toString}) not implemented yet" -> r.toLoc,
                    msg"Note: Block IR of expression is `${r.toString}`" -> N
                  ))
                  unreachable
            )
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
      val base = subexpression(fun).get
      if base.exprType is UnreachableType then return S(base)
      val wasmArgs = args.map(argument)

      val funcType = TypeInfo(
        id = N,
        compType = SignatureType(
          params = Seq.fill(wasmArgs.size)(WasmParam(N, RefType.anyref)),
          results = Seq(Result(RefType.anyref))
        )
      )
      val funcRefType = ctx.addType(N, funcType)

      S(call_ref(
        target = ref.cast(base, RefType(funcRefType, nullable = false)),
        operands = wasmArgs.map(_.get).toSeq,
        typeRef = funcRefType,
        sigType = funcType.compType.asInstanceOf[SignatureType]
      ))
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
      val lExpr = getVar(l, t.toLoc).get
      if lExpr.exprType is UnreachableType then return S(lExpr)
      val rExpr = result(r).get
      val idx = lExpr.instrargs(0).toString.toInt
      val assignExpr = lExpr.mnemonic match
        case "global.get" =>
          warnExpr(Ls(
            msg"WatBuilder::returningTerm for Assign(...) to global variable not implemented yet" -> t.toLoc,
            msg"Note: Block IR of expression is `${t.toString}`" -> N
          )).get
        case "local.get" => local.set(idx, rExpr)
        case mnemonic =>
          lastWords(
            s"Expected `global.get` or `local.get` when compiling instruction for `$l`, but got ${lExpr.mnemonic}"
          )
      val rstBlk = returningTerm(rst)

      S(Instructions.block(
        label = N,
        children = Seq(assignExpr) ++ rstBlk.toSeq,
        resultType = rstBlk.map(_.exprType).getOrElse(NoneType)
      ))

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
                  val funcVar = getVar(sym, sym.toLoc).get
                  val funcInfo =
                    FuncInfo(
                      sym,
                      ps.params.map(p => p.sym),
                      bodyWat.get.exprType.toSeq.length,
                      locals,
                      bodyWat.get
                    )
                  ctx.funcs(defn.sym) = funcInfo

                  val idx = funcVar.instrargs(0).toString.toInt
                  funcVar.mnemonic match
                    case "global.get" =>
                      S(warnExpr(Ls(
                        msg"WatBuilder::returningTerm for Assign(...) to global variable not implemented yet" -> t.toLoc,
                        msg"Note: Block IR of expression is `${t.toString}`" -> N
                      )).get)
                    case "local.get" =>
                      // Refine the type of the local variable to funcref -
                      // This is necessary as `any` and `func` are two
                      // distinct hierarchies
                      val (localId, _) = ctx.locals.head(idx)
                      ctx.locals.head(idx) = (localId, RefType.funcref)

                      S(local.set(
                        idx,
                        ref.func(
                          funcInfo.id,
                          RefType(TypeId(sym.nme), nullable = false)
                        )
                      ))
                    case mnemonic =>
                      lastWords(
                        s"Expected `global.get` or `local.get` when compiling instruction for `$funcVar`, but got ${funcVar.mnemonic}"
                      )
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
            case _ => S(Instructions.block(
                label = N,
                children = Seq(res.get, rstBlk.get),
                resultType = rstBlk.map(_.exprType).getOrElse(NoneType)
              ))

        case defn =>
          warnExpr(Ls(
            msg"WatBuilder::returningTerm for Define(...) not implemented yet" -> t.toLoc,
            msg"Note: Block IR of expression is `${t.toString}`" -> N
          ))
    case Return(res, true) =>
      result(res)
    case Return(res, false) =>
      S(`return`(result(res)))
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
    val entrySym = BlockMemberSymbol(
      "entry",
      trees = Nil,
      nameIsMeaningful = !ctx.funcs.exists(_._1.nme == "entry")
    )
    val entryNme = if entrySym.nameIsMeaningful then
      entrySym.nme
    else
      // TODO(Derppening): How does Scala actually infer whether an argument is passed as a closure vs the value?
      LazyList.continually(() => State.suid.nextUid).collectFirst:
        // JS requires identifiers to not start with a digit
        case uid if !ctx.funcs.exists(_._1.nme == s"_$uid") => s"_$uid"
      .get
    val entryFn = FuncInfo(
      id = FuncRef(entryNme),
      params = Seq.empty,
      nResults = 1,
      // TODO(Derppening): Should we place top-level scope variables in the global section?
      locals = entryFnLocals,
      entryFnExpr.get
    )
    ctx.funcs(entrySym) = entryFn
    (ctx.toWat, entryFn.id.id)

  def blockPreamble(ss: Iterable[Symbol])(using Ctx, Raise, Scope): Seq[Local] =
    val vars = ss.filter(
      scope.lookup(_).toSeq.isEmpty
    ).toSeq.toArray.sortBy(_.uid).iterator.map: l =>
      scope.allocateName(l)
      l -> RefType.anyref
    .to(ArrayBuf)
    ctx.locals.head ++= vars
    vars.map(_._1).toSeq

  def block(t: Block)(using Ctx, Raise, Scope): (Expr, Seq[Local -> WasmType]) =
    val locals = blockPreamble(t.definedVars)
    val returningExpr = returningTerm(t)
    val refinedLocals = ctx.locals.head.filter(_._1 in locals).toSeq
    (returningTerm(t), refinedLocals)

  def body(t: Block)(using Ctx, Raise, Scope): (Expr, Seq[Local -> WasmType]) =
    scope.nest givenIn:
      block(t)

  def setupFunction(name: Option[Str], params: ParamList, body: Block)(using
      Ctx,
      Raise,
      Scope
  ): (Seq[WasmParam], Expr, Seq[Local -> WasmType]) =
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
