package hkmc2
package codegen
package wasm
package text

import mlscript.utils.*
import shorthands.*

import document.*
import wasm.Module as WasmModule

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

  override def unwrap: ExprProxy = this
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
      body: ExprProxy
  ): FuncRef =
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
        .dlof(docs => doc" #{  # ${docs.mkDocument(Document.forceBreak)}) #} ")(doc"")}"

    mod = mod.copy(fn = mod.fn :+ name -> fnDecl)
    FuncRef(this, name)

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
  ): ExportRef =
    val funcExp = doc"""(export "$externalName" (func $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> funcExp)
    ExportRef(this, externalName)

  override def addTableExport(internalName: Str, externalName: Str): ExportRef =
    val tableExp = doc"""(export "$externalName" (table $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> tableExp)
    ExportRef(this, externalName)

  override def addMemoryExport(
      internalName: Str,
      externalName: Str
  ): ExportRef =
    val memoryExp = doc"""(export "$externalName" (memory $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> memoryExp)
    ExportRef(this, externalName)

  override def addGlobalExport(
      internalName: Str,
      externalName: Str
  ): ExportRef =
    val globalExp = doc"""(export "$externalName" (global $$$internalName))"""

    mod = mod.copy(ex = mod.ex :+ externalName -> globalExp)
    ExportRef(this, externalName)

  override def addGlobal(
      name: Str,
      ty: Type,
      mutable: Bool,
      value: ExprProxy
  ): GlobalRef =
    val globalDecl = doc"(global $name ${
        if mutable then doc"(mut ${gen.fmtType(ty)})" else gen.fmtType(ty)
      } (${value.fmtDoc}))"

    mod = mod.copy(gl = mod.gl :+ name -> globalDecl)
    GlobalRef(this, name)

  override def removeGlobal(name: Str): Unit =
    mod = mod.copy(gl = mod.gl.filterNot((nm, _) => nm == name))

  override def setMemory(
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

  override def setStart(start: Func): Unit =
    mod = mod.copy(st = S(start.name))

  override def block(
      label: Opt[Str],
      children: Seq[ExprProxy],
      resultType: Opt[Type]
  ): ExprProxy =
    ExprProxy(
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

  override def nop(): ExprProxy =
    ExprProxy(S(FoldedInstr("drop", Seq(), Seq())))

  override def ret(value: Opt[ExprProxy]): ExprProxy =
    ExprProxy(S(FoldedInstr("return", Seq(), value.map(_.inner).toSeq)))

  override def unreachable(): ExprProxy =
    ExprProxy(S(FoldedInstr("unreachable", Seq(), Seq())))

  override def drop(value: ExprProxy): ExprProxy =
    ExprProxy(S(FoldedInstr("drop", Seq(), Seq(value.inner))))

  override def i32: ModI32Proxy[ExprProxy] = ModI32Impl()
  override def ref: ModRefProxy[ExprProxy] = ModRefImpl()
  override def i31ref: ModI31RefProxy[ExprProxy] = ???

  def emitText: Document = mod.emitText
end ModuleProxy

class ModI32Impl extends ModI32Proxy[ExprProxy]:
  override def const(value: Int): Expression[ExprProxy] =
    ExprProxy(S(FoldedInstr("i32.const", Seq(s"$value"), Seq())))
end ModI32Impl

class ModRefImpl extends ModRefProxy[ExprProxy]:
  override def i31(value: ExprProxy): ModuleProxy#Expr =
    ExprProxy(S(FoldedInstr("ref.i31", Seq(), Seq(value.inner))))
end ModRefImpl

/** A [[WasmGenerator]] backend that produces text-based WAT as its output. */
class WatBackend(val folded: Bool) extends WasmGenerator[ModuleProxy]:
  /** Formats a type into its text representation. */
  def fmtType(ty: Type): Document = ty match
    case I32Type    => doc"i32"
    case AnyRefType => doc"anyref"
    case I31RefType => doc"i31.ref"
    case _          => ???

  def newModule: ModuleProxy = ModuleProxy(this, Module())
end WatBackend

@main
def main(): Unit =
  println(WasmGenerator.mkSimpleModule(WatBackend(true)).emitText)
