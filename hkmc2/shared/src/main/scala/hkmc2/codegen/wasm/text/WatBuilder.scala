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
import syntax.Tree.BoolLit
import Message.MessageContext
import Scope.scope

import scala.collection.mutable.Map as MutMap

private final case class FuncInfo(
    val name: BlockMemberSymbol,
    val params: Seq[Local],
    val nResults: Int,
    val locals: Seq[Local],
    val body: FoldedInstr
) extends ToWat:
  def toWat: Document = toWat()
  def toWat(emitElem: Bool = true): Document =
    doc"""(func $$${name.nme} ${
        params.map(p =>
          doc"(param ${p.nme} ${RefType.anyref.toWat})"
        ).toSeq.mkDocument(" ")
      } ${
        Seq.fill(nResults)(
          doc"(result ${RefType.anyref.toWat})"
        ).mkDocument(" ")
      } ${
        locals.map(p => doc"(local ${RefType.anyref.toWat})").toSeq.mkDocument(
          " "
        )
      } ${body.toWat})\n(export "${name.nme}" (func $$${name.nme}))${
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
    doc"""(module #  #{ ${funcs.values.toSeq.map(_.toWat).mkDocument(
        doc" # "
      )} # ${main.dlof(_._2.toWat(emitElem = false))(doc"")}) #} """

end Ctx

final class WatBuilder(using TraceLogger, State) extends CodeBuilder:
  import Instructions.*

  type Context = Unit
  type Expr = Opt[FoldedInstr]

  def errExpr(errMsg: Message)(using Ctx, Raise): Expr =
    raise(
      ErrorReport(errMsg -> N :: Nil, source = Diagnostic.Source.Compilation)
    )
    S(unreachable)

  def result(r: Result)(using Ctx, Raise, Scope): Expr = r match
    case Value.Lit(BoolLit(value)) =>
      S(ref.i31(i32.const(if value then 1 else 0)))
    case r =>
      raise(
        WarningReport(
          msg"WatBackend::result for ${r.toString} not implemented yet" -> N :: Nil,
          source = Diagnostic.Source.Compilation
        )
      )
      S(unreachable)

  def returningTerm(t: Block)(using Ctx, Raise, Scope): Expr = t match
    case _: HandleBlock =>
      errExpr(
        msg"This code requires effect handler instrumentation but was compiled without it."
      )
    case Return(res, true) =>
      result(res)
    case t =>
      raise(
        WarningReport(
          msg"WatBuilder::returningTerm for ${t.toString} not implemented yet" -> N :: Nil,
          source = Diagnostic.Source.Compilation
        )
      )
      S(unreachable)

  def program(p: Program, exprt: Opt[BlockMemberSymbol], wd: os.Path)(using
      Raise,
      Scope
  ): Ctx =
    if p.imports.nonEmpty then
      raise(
        WarningReport(
          msg"Imports of external symbols ${p.imports.mkString("[", ", ", "]")} not implemented yet" -> N :: Nil,
          source = Diagnostic.Source.Compilation
        )
      )
    if exprt.isDefined then
      raise(
        WarningReport(
          msg"Exports of symbols not implemented yet" -> N :: Nil,
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
