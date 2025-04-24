package hkmc2
package codegen
package wasm
package text

import mlscript.utils.*, shorthands.*

import document.*
import semantics.*
import syntax.Tree.{IntLit, UnitLit}
import wasm.Module as WasmModule
import Message.MessageContext

/** A reference to an `export` field in a module.
 *
 * @param mod
 *   The module that contains the export.
 * @param intName
 *   The internal name of the export.
 */
case class ExportRef(mod: ModuleProxy, intName: Str) extends Export[ExportRef]

/** A reference to an expression.
 *
 * @param inner
 *   The [[Expr]] that this proxy represents.
 */
class ExprProxy(val inner: Expr) extends Expression[ExprProxy]:
  /** Whether this expression consists of exactly zero instructions. */
  def isEmpty: Boolean = inner match
    case stackInstr: Ls[StackInstr]    => stackInstr.isEmpty
    case foldedInstr: Opt[FoldedInstr] => foldedInstr.isEmpty

  /** See [[isEmpty]]. */
  def nonEmpty: Boolean = !isEmpty

  /** Converts the inner expression into a [[List]] of
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

/** A reference to a `func` field in a module.
 *
 * @param mod
 *   The module that contains the function.
 * @param name
 *   The name of the function.
 */
case class FuncRef(mod: ModuleProxy, name: Str) extends Function[FuncRef]:
  override type Expr = ExprProxy
end FuncRef

/** A reference to a `global` field in a module.
 *
 * @param mod
 *   The module that contains the global.
 * @param name
 *   The name of the global.
 */
class GlobalRef(mod: ModuleProxy, name: Str) extends Global[GlobalRef]

/** A reference to a WebAssembly module.
 *
 * @param gen
 *   The [[WatBackend]] that generates constructs for this module.
 * @param mod
 *   The underlying [[wasm.Module]] that this proxy represents.
 */
class ModuleProxy(private val gen: WatBackend, private var mod: Module)
    extends WasmModule:
  override type Exprt = ExportRef
  override type Expr = ExprProxy
  override type Func = FuncRef
  override type Glob = GlobalRef

  override def addFunction(
      name: Str,
      params: Type,
      results: Type,
      vars: Seq[Type],
      body: Expr
  ): Func =
    assume(
      !mod.fn.exists((nm, _) => nm == name),
      s"Function `$name` already exists"
    )

    val fnDecl = doc"(func $$$name${gen
        .expandType(params)
        .optionIf(_.nonEmpty)
        .dlof(_.map(p => doc"(param ${gen.fmtType(p)})").mkDocument(" ", " ", ""))(doc"")}${gen
        .expandType(results)
        .optionIf(_.nonEmpty)
        .dlof(
          _.map(r => doc"(result ${gen.fmtType(r)})").mkDocument(" ", " ", "")
        )(doc"")}${(vars.map(v => doc"(local ${gen.fmtType(v)})") :+ body.fmtDoc)
        .filterNot(_.isEmpty)
        .optionIf(_.nonEmpty)
        .dlof(docs => doc" #{  # ${docs.mkDocument(Document.forceBreak)}) #} ")(doc")")}"

    mod = mod.copy(fn = mod.fn :+ name -> fnDecl)
    new Func(this, name)

  override def removeFunction(name: Str): Unit =
    mod = mod.copy(fn = mod.fn.filterNot((nm, _) => nm == name))

  override def addFunctionImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      params: Type,
      results: Type
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

  override def addTableImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit =
    val tableImp =
      doc"(import \"$externalModuleName\" \"$externalBaseName\" (table $$$internalName funcref))"

    mod = mod.copy(im = mod.im :+ internalName -> tableImp)

  override def addMemoryImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit =
    val memImp =
      doc"(import \"$externalModuleName\" \"$externalBaseName\" (memory $$$internalName 0 65536))"

    mod = mod.copy(im = mod.im :+ internalName -> memImp)

  override def addGlobalImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      globalType: Type
  ): Unit =
    val globalImp =
      doc"(import \"$externalModuleName\" \"$externalBaseName\" (global $$$internalName ${gen.fmtType(globalType)}))"

    mod = mod.copy(im = mod.im :+ internalName -> globalImp)

  override def addFunctionExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    val funcExp = doc"""(export "$externalName" (func $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> funcExp)
    new Exprt(this, externalName)

  override def addTableExport(internalName: Str, externalName: Str): Exprt =
    val tableExp = doc"""(export "$externalName" (table $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> tableExp)
    new Exprt(this, externalName)

  override def addMemoryExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    val memoryExp = doc"""(export "$externalName" (memory $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> memoryExp)
    new Exprt(this, externalName)

  override def addGlobalExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    val globalExp = doc"""(export "$externalName" (global $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> globalExp)
    new Exprt(this, externalName)

  override def addGlobal(
      name: Str,
      ty: Type,
      mutable: Bool,
      value: Expr
  ): Glob =
    val globalDecl = doc"(global $name ${
        if mutable then doc"(mut ${gen.fmtType(ty)})" else gen.fmtType(ty)
      } (${value.fmtDoc}))"

    mod = mod.copy(gl = mod.gl :+ name -> globalDecl)
    new Glob(this, name)

  override def removeGlobal(name: Str): Unit =
    mod = mod.copy(gl = mod.gl.filterNot((nm, _) => nm == name))

  override def setMemory(
      initial: Int,
      maximum: Int,
      exportName: Opt[Str],
      segments: Seq[MemorySegment[Expr]],
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

  override def setStart(start: Func): Unit =
    mod = mod.copy(st = S(start.name))

  override def block(
      label: Opt[Str],
      children: Seq[Expr],
      resultType: Opt[Type]
  ): Expr =
    new Expr(
      S(
        FoldedInstr(
          "block",
          label.map(label => s"$$$label").toSeq ++ resultType
            .map(gen.expandType(_))
            .map(_.map(resTy => s"(result ${gen.fmtType(resTy)})")),
          children.map(_.inner)
        )
      )
    )

  override def nop(): Expr =
    new Expr(S(FoldedInstr("nop", Seq(), Seq())))

  override def ret(value: Opt[Expr]): Expr =
    new Expr(S(FoldedInstr("return", Seq(), value.map(_.inner).toSeq)))

  override def unreachable(): Expr =
    new Expr(S(FoldedInstr("unreachable", Seq(), Seq())))

  override def drop(value: Expr): Expr =
    new Expr(S(FoldedInstr("drop", Seq(), Seq(value.inner))))

  override def call(name: Str, operands: Seq[Expr], returnType: Type): Expr =
    // TODO: Ensure that operands are either placed on the stack now, or use `local.get`
    //       Or - Use Seq[??? -> Expr] to lazily generate the expressions on the spot?
    if operands.nonEmpty then TODO("call with operands is not supported yet")
    new Expr(S(FoldedInstr("call", Seq(s"$$$name"), Seq())))

  override def i32 = new I32:
    override def const(value: Int): Expr =
      new Expr(S(FoldedInstr("i32.const", Seq(s"$value"), Seq())))

    override def add(left: Expr, right: Expr): Expr =
      new Expr(S(FoldedInstr("i32.add", Seq(), Seq(left.inner, right.inner))))
  end i32

  override def ref = new Ref:
    override def i31(value: Expr): Expr =
      new Expr(S(FoldedInstr("ref.i31", Seq(), Seq(value.inner))))
  end ref

  override def i31ref = new I31Ref:
    override def get(i31: Expr, signed: Bool): Expr =
      new Expr(
        S(
          FoldedInstr(
            s"i31.get_${if signed then 's' else 'u'}",
            Seq(),
            Seq(i31.inner)
          )
        )
      )
  end i31ref

  def emitText: Document = mod.emitText
end ModuleProxy

/** A [[WasmGenerator]] backend that produces text-based WAT as its output. */
class WatBackend extends WasmGenerator[ModuleProxy]:
  /** Formats a type into its text representation. */
  def fmtType(ty: Type): Document = ty match
    case I32Type    => doc"i32"
    case AnyRefType => doc"anyref"
    case I31RefType => doc"i31.ref"
    case _          => ???

  override def newModule: ModuleProxy = ModuleProxy(this, Module())

  def errExpr(errMsg: Message)(using ModuleProxy, Raise): ModuleProxy#Expr =
    raise(
      ErrorReport(errMsg -> N :: Nil, source = Diagnostic.Source.Compilation)
    )
    summon[ModuleProxy].unreachable()

  def operand(
      a: Arg
  )(using ModuleProxy, Raise): ModuleProxy#Expr =
    if a.spread then die else subexpression(a.value)

  def subexpression(
      r: Result
  )(using ModuleProxy, Raise): ModuleProxy#Expr = result(r)

  def result(
      r: Result
  )(using ModuleProxy, Raise): ModuleProxy#Expr =
    val mod = summon[ModuleProxy]
    r match
      case Value.Lit(IntLit(value)) =>
        mod.ref.i31(mod.i32.const(value.toInt))
      case Call(Value.Ref(l: BuiltinSymbol), lhs :: rhs :: Nil)
          if !l.functionLike =>
        if l.binary then
          l.nme match
            case "+" =>
              // TODO(Derppening): Do not assume i31ref
              val lhsOp = operand(lhs)
              val rhsOp = operand(rhs)
              mod.ref
                .i31(
                  mod.i32
                    .add(
                      mod.i31ref.get(lhsOp, true),
                      mod.i31ref.get(rhsOp, true)
                    )
                )
            case lNme =>
              raise(
                WarningReport(
                  msg"WasmBackend::result for binary builtin symbol '${lNme.toString}' not implemented yet" -> N :: Nil,
                  source = Diagnostic.Source.Compilation
                )
              )
              mod.unreachable()
        else errExpr(msg"Cannot call non-binary builtin symbol '${l.nme}'")
      case c @ Call(fun, args) =>
        // TODO
        val base = subexpression(fun)
        // val args = args.map(argument)
        println(base)
        mod.call("foo", Seq(), this.i31ref)
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
  )(using ModuleProxy, Raise): ModuleProxy#Expr =
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
              // TODO(Derppening): Infer whether we actually have a return value or ()
              results = this.anyref,
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
      case Return(res, true)                        => result(res)
      case Return(res, false)                       => mod.ret(S(result(res)))
      case End(_)                                   =>
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
      results = this.anyref,
      vars = Seq(),
      mainFnExpr
    )
    module.addFunctionExport("main", "main")
    // TODO(Derppening): Do we treat `main` as a main function, or just a launchpad from JS?
    //                   Start functions must not return any value though...
    // module.setStart(mainFn)
    module

  def block(
      t: Block
  )(using ModuleProxy, Raise): ModuleProxy#Expr =
    returningTerm(t)
end WatBackend

@main
def main(): Unit =
  println(WasmGenerator.mkSimpleModule(WatBackend()).emitText)
