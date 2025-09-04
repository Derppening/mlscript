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
import Message.MessageContext
import Scope.scope

import scala.collection.mutable.Map as MutMap

extension (doc: Document)
  private def surroundUnlessEmpty(
      prefix: Document = Document.empty,
      postfix: Document = Document.empty
  ): Document =
    doc.optionUnless(_.isEmpty).fold(doc):
      prefix :: _ :: postfix

private final case class FuncInfo(
    val name: BlockMemberSymbol,
    val params: Seq[Local],
    val nResults: Int,
    val locals: Seq[Local],
    val body: FoldedInstr
) extends ToWat:
  def toWat: Document = toWat()
  def toWat(emitElem: Bool = true): Document =
    doc"""(func $$${name.nme}${
        params.map(p =>
          doc"(param ${p.nme} ${RefType.anyref.toWat})"
        ).toSeq.mkDocument(doc" ").surroundUnlessEmpty(doc" ")
      }${
        Seq.fill(nResults)(
          doc"(result ${RefType.anyref.toWat})"
        ).mkDocument(doc" ").surroundUnlessEmpty(doc" ")
      } #{ ${
        locals.map(p => doc"(local ${RefType.anyref.toWat})").toSeq.mkDocument(
          doc" # "
        )
      } # ${body.toWat} #} )\n(export "${name.nme}" (func $$${name.nme}))${
        if emitElem then doc"\n(elem declare func $$${name.nme})" else doc""
      }"""
end FuncInfo

private final case class TypeInfo(
    val id: Str,
    val wasmType: WasmType
)

object Ctx:
  def empty: Ctx = Ctx(
    types = MutMap.empty,
    funcs = MutMap.empty,
    main = N
  )

  def ctx(using ctx: Ctx): Ctx = ctx

private final case class Ctx(
    val types: MutMap[ClsLikeDefn, TypeInfo],
    val funcs: MutMap[FunDefn, FuncInfo],
    var main: Opt[Symbol -> FuncInfo]
) extends ToWat:

  def toWat: Document =
    doc"""(module #{ ${funcs.values.toSeq.map(_.toWat).mkDocument(
        doc" # "
      )} # ${main.fold(doc"")(_._2.toWat(emitElem = false))}) #} """

end Ctx

final class WatBuilder(using TraceLogger, State) extends CodeBuilder:
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

  def operand(a: Arg)(using Ctx, Raise, Scope): Expr =
    if a.spread.nonEmpty then die else subexpression(a.value)

  def subexpression(r: Result)(using Ctx, Raise, Scope): Expr = r match
    case r: Value.Lam =>
      warnExpr(Ls(
        msg"WatBuilder::subexpression for Value.Lam not implemented yet" -> r.toLoc,
        msg"Note: Block IR of expression is `${r.toString}`" -> N
      ))
    case r => result(r)

  def result(r: Result)(using Ctx, Raise, Scope): Expr = r match
    case Value.Lit(BoolLit(value)) =>
      S(ref.i31(i32.const(if value then 1 else 0)))
    case Value.Lit(IntLit(value)) =>
      S(ref.i31(i32.const(value.toInt)))
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
                case ty => warnExpr(Ls(
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
    case Return(res, true) =>
      result(res)
    case t =>
      warnExpr(Ls(
        msg"WatBuilder::returningTerm for expression not implemented yet" -> t.toLoc,
        msg"Note: Block IR of expression is `${t.toString}`" -> N
      ))

  def program(p: Program, exprt: Opt[BlockMemberSymbol], wd: os.Path)(using
      Raise,
      Scope
  ): Ctx =
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
    val entryFnExpr = block(p.main)(using ctx, summon[Raise], summon[Scope]).get
    val entryFn = FuncInfo(
      BlockMemberSymbol("entry", Nil),
      params = Seq.empty,
      nResults = 1,
      locals = scope._3.keysIterator.toSeq,
      entryFnExpr
    )
    ctx.main = S(entryFn.name -> entryFn)
    ctx

  def blockPreamble(ss: Iterable[Symbol])(using Raise, Scope): Unit =
    ss.filter(
      scope.lookup(_).toSeq.isEmpty
    ).toSeq.toArray.sortBy(_.uid).toSeq.iterator.foreach(scope.allocateName(_))

  def block(t: Block)(using Ctx, Raise, Scope) =
    blockPreamble(t.definedVars)
    returningTerm(t)

end WatBuilder
