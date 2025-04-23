package hkmc2
package codegen
package wasm
package binaryen

import mlscript.utils.*

import document.*
import shorthands.*

import java.util.concurrent.atomic.AtomicLong

/** Trait indicating that a class can be lowered into JavaScript code, */
private trait ToJSRepr:
  /** Lowers this instance into JavaScript code for execution. */
  def toJSRepr: Document
end ToJSRepr

/** Abstraction over a JavaScript variable with a unique number as its
 * identifier.
 *
 * This is used to represent intermediate values when generating JavaScript
 * code.
 */
case class VarId(id: Long) extends ToJSRepr:
  /** Returns the name of the variable, i.e. `_$id`. */
  override def toJSRepr: Document = doc"_${id.toString}"
end VarId

/** A reference to a Binaryen module.
 *
 * @param gen
 *   The [[BinaryenJSBackend]] instance that generates constructs for this
 *   module.
 * @param varId
 *   The identifier of the module in JavaScript code.
 */
case class ModRef(gen: BinaryenJSBackend, varId: VarId)
    extends Module
    with ToJSRepr:
  override type Exprt = ExportRef
  override type Expr = ExprRef
  override type Func = FuncRef
  override type Glob = GlobalRef

  override def addFunction(
      name: Str,
      params: Type,
      results: Type,
      vars: Seq[Type],
      body: Expr
  ): Func =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addFunction($name, ${gen
          .fmtType(params)}, ${gen.fmtType(results)}, ${vars
          .map(gen.fmtType)
          .mkString("[", ", ", "]")}, ${body.toJSRepr})"
      new Func(freshId)

  override def removeFunction(name: String): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.removeFunction($name)"

  override def addFunctionImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      params: Type,
      results: Type
  ): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.addFunctionImport($internalName, $externalModuleName, $externalBaseName, ${gen
        .fmtType(params)}, ${gen.fmtType(results)})"
  override def addTableImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.addTableImport($internalName, $externalModuleName, $externalBaseName)"
  override def addMemoryImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.addMemoryImport($internalName, $externalModuleName, $externalBaseName)"
  override def addGlobalImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      globalType: Type
  ): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.addGlobalImport($internalName, $externalModuleName, $externalBaseName, ${gen
        .fmtType(globalType)})"

  override def addFunctionExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addFunctionExport($internalName, $externalName)"
      new Exprt(freshId)
  override def addTableExport(internalName: Str, externalName: Str): Exprt =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addTableExport($internalName, $externalName)"
      new Exprt(freshId)
  override def addMemoryExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addMemoryExport($internalName, $externalName)"
      new Exprt(freshId)
  override def addGlobalExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addGlobalExport($internalName, $externalName)"
      new Exprt(freshId)

  override def addGlobal(
      name: Str,
      ty: Type,
      mutable: Bool,
      value: Expr
  ): Glob =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addGlobal($name, ${gen
          .fmtType(ty)}, ${if mutable then 1 else 0}, ${value.toJSRepr})"
      new Glob(freshId)
  override def removeGlobal(name: Str): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.removeGlobal($name)"

  override def setMemory(
      initial: Int,
      maximum: Int,
      exportName: Opt[Str],
      segments: Seq[MemorySegment[Expr]],
      shared: Bool
  ): Unit =
    gen.db +=\\
      doc"${this.toJSRepr}.setMemory(${initial.toString}, ${maximum.toString}, ${exportName.orNull}, ${segments
          .map(s => s"{offset: ${s.offset.toJSRepr}, data: new Uint8Array(${s.data.mkString("[", ", ", "]")}), passive: ${s.passive}")
          .mkString("[", ", ", "]")}, ${shared.toString})"

  override def setStart(start: Func): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.setStart(${start.toJSRepr})"

  override def block(
      label: Opt[Str],
      children: Seq[Expr],
      resultType: Opt[Type]
  ): Expr =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.block(${label.orNull}, ${children
            .map(_.toJSRepr)
            .mkDocument(pre = "[", ", ", post = "]")}${resultType
            .map(resTy => s", ${gen.fmtType(resTy)}")
            .getOrElse("")})"
      new Expr(freshId)

  override def nop(): Expr =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.nop()"
      new Expr(freshId)

  override def ret(value: Opt[Expr]): Expr =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.return(${value.map(_.toJSRepr).getOrElse("")})"
      new Expr(freshId)

  override def unreachable(): Expr =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.unreachable()"
      new Expr(freshId)

  override def drop(value: Expr): Expr =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.drop(${value.toJSRepr})"
      new Expr(freshId)

  override def i32 = new I32:
    override def const(value: Int): Expr =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.i32.const(${value.toString})"
        new Expr(freshId)

    override def add(left: Expr, right: Expr): Expr =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.i32.add(${left.toJSRepr}, ${right.toJSRepr})"
        new Expr(freshId)
  end i32

  override def ref: Ref = new Ref:
    override def i31(value: Expr): Expr =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.ref.i31(${value.toJSRepr})"
        new Expr(freshId)
  end ref

  override def i31ref: I31Ref = new I31Ref:
    override def get(i31: Expr, signed: Bool): Expr =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.i31.get_${if signed then "s" else "u"}(${i31.toJSRepr})"
        new Expr(freshId)
  end i31ref

  override def toJSRepr: Document = varId.toJSRepr
end ModRef

/** A reference to an `export` field in Binaryen.
 *
 * @param varId
 *   The identifier of the export in JavaScript code.
 */
case class ExportRef(varId: VarId) extends Export[ExportRef] with ToJSRepr:
  override def toJSRepr: Document = varId.toJSRepr
end ExportRef

/** A reference to an `func` field in Binaryen.
 *
 * @param varId
 *   The identifier of the export in JavaScript code.
 */
case class FuncRef(varId: VarId) extends Function[FuncRef] with ToJSRepr:
  override def toJSRepr: Document = varId.toJSRepr
end FuncRef

/** A reference to an expression in Binaryen.
 *
 * @param varId
 *   The identifier of the export in JavaScript code.
 */
case class ExprRef(varId: VarId) extends Expression[ExprRef] with ToJSRepr:
  override def toJSRepr: Document = varId.toJSRepr
end ExprRef

/** A reference to a global in Binaryen.
 *
 * @param varId
 *   The identifier of the export in JavaScript code.
 */
case class GlobalRef(varId: VarId) extends Global[GlobalRef] with ToJSRepr:
  override def toJSRepr: Document = varId.toJSRepr
end GlobalRef

/** A [[WasmGenerator]] backend that produces Binaryen.js Javascript calls as
 * its output.
 *
 * @param modId
 *   The identifier of which the Binaryen module is loaded and referred to in
 *   the JavaScript code.
 */
class BinaryenJSBackend(private[binaryen] val modId: Str = "binaryen")
    extends WasmGenerator[ModRef]:

  /** A monotonically increasing counter for generating variable names of
   * intermediate Binaryen values.
   */
  private val varCounter = AtomicLong()

  /** The [[DocBuilder]] instance housing all generated JavaScript code. */
  private[binaryen] val db = DocBuilder()

  /** Creates a fresh [[VarId]], executes [[block]], and returns the result of
   * the block.
   *
   * This is used to simplify capturing intermediate Binaryen values.
   */
  def withFreshVarId[T](block: VarId => T): T = block(
    VarId(varCounter.getAndIncrement())
  )

  def fmtType(ty: Type): Str = ty match
    case I32Type    => s"$modId.i32"
    case I31RefType => s"$modId.i31ref"
    case _          => ???

  override def newModule: ModRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = new $modId.Module()"
      ModRef(this, freshId)

end BinaryenJSBackend
