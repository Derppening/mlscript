package hkmc2
package codegen
package wasm
package binaryen

import mlscript.utils.*

import document.*
import shorthands.*

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable

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
    extends Module[TypeRef, ExprRef]
    with ToJSRepr:
  override type Exprt = ExportRef
  override type Func = FuncRef
  override type FuncInfo = FuncInfoRef
  override type Glob = GlobalRef

  override def addFunction(
      name: Str,
      params: TypeRef,
      results: TypeRef,
      vars: Seq[TypeRef],
      body: ExprRef
  ): Func =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addFunction($name, ${params.toJSRepr}, ${results.toJSRepr}, ${vars
          .map(_.toJSRepr)
          .mkString("[", ", ", "]")}, ${body.toJSRepr})"
      new Func(freshId)

  /** Gets a function by name.
    *
    * Generates a JavaScript runtime assertion if the function does not exist.
    */
  override def getFunction(name: Str): Func =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.getFunction($name)"
      gen.db +=\\ doc"""assert(${freshId.toJSRepr}, "Function '$name' not found in module")"""
      new Func(freshId)

  override def removeFunction(name: String): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.removeFunction($name)"

  override def addFunctionImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      params: TypeRef,
      results: TypeRef
  ): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.addFunctionImport($internalName, $externalModuleName, $externalBaseName, ${params.toJSRepr}, ${results.toJSRepr})"
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
      globalType: TypeRef
  ): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.addGlobalImport($internalName, $externalModuleName, $externalBaseName, ${globalType.toJSRepr})"

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
      ty: TypeRef,
      mutable: Bool,
      value: ExprRef
  ): Glob =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addGlobal($name, ${ty.toJSRepr}, ${
          if mutable then 1 else 0
        }, ${value.toJSRepr})"
      new Glob(freshId)
  override def removeGlobal(name: Str): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.removeGlobal($name)"

  override def setMemory(
      initial: Int,
      maximum: Int,
      exportName: Opt[Str],
      segments: Seq[MemorySegment[ExprRef]],
      shared: Bool
  ): Unit =
    gen.db +=\\
      doc"${this.toJSRepr}.setMemory(${initial.toString}, ${maximum.toString}, ${exportName.orNull}, ${segments
          .map(s => s"{offset: ${s.offset.toJSRepr}, data: new Uint8Array(${s.data.mkString("[", ", ", "]")}), passive: ${s.passive}")
          .mkString("[", ", ", "]")}, ${shared.toString})"

  override def setStart(start: Func): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.setStart(${start.toJSRepr})"

  override def getFunctionInfo(ftype: Func): FuncInfo =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.getFunctionInfo(${ftype.toJSRepr})"
      new FuncInfo(freshId)

  override def block(
      label: Opt[Str],
      children: Seq[ExprRef],
      resultType: Opt[TypeRef]
  ): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.block(${label.orNull}, ${children
            .map(_.toJSRepr)
            .mkDocument(pre = "[", ", ", post = "]")}${resultType
            .map(resTy => s", ${resTy.toJSRepr}")
            .getOrElse("")})"
      new ExprRef(freshId)

  override def nop(): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.nop()"
      new ExprRef(freshId)

  override def ret(value: Opt[ExprRef]): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.return(${value.map(_.toJSRepr).getOrElse("")})"
      new ExprRef(freshId)

  override def unreachable(): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.unreachable()"
      new ExprRef(freshId)

  override def drop(value: ExprRef): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.drop(${value.toJSRepr})"
      new ExprRef(freshId)

  override def call(
      name: Str,
      operands: Seq[ExprRef],
      returnType: TypeRef
  ): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.call($name, ${operands
            .map(_.toJSRepr)
            .mkDocument(doc"[", doc", ", doc"]")}, ${returnType.toJSRepr})"
      new ExprRef(freshId)

  override def callRef(
      target: ExprRef,
      operands: Seq[ExprRef],
      params: TypeRef,
      results: TypeRef
  ): ExprRef =
    TODO("Binaryen.js does not support call_ref")

  override def i32 = new I32:
    override def const(value: Int): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.i32.const(${value.toString})"
        new ExprRef(freshId)

    override def add(left: ExprRef, right: ExprRef): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.i32.add(${left.toJSRepr}, ${right.toJSRepr})"
        new ExprRef(freshId)
  end i32

  override def ref: Ref = new Ref:
    override def func(name: Str, ty: TypeRef): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.ref.func($name, ${ty.toJSRepr})"
        new ExprRef(freshId)

    override def i31(value: ExprRef): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.ref.i31(${value.toJSRepr})"
        new ExprRef(freshId)
  end ref

  override def i31ref: I31Ref = new I31Ref:
    override def get(i31: ExprRef, signed: Bool): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.i31.get_${if signed then "s" else "u"}(${i31.toJSRepr})"
        new ExprRef(freshId)
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

/** A reference to a structure containing function information in Binaryen. */
case class FuncInfoRef(varId: VarId)
    extends FunctionInfo[TypeRef]
    with ToJSRepr:
  override def toJSRepr: Document = varId.toJSRepr
end FuncInfoRef

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

/** A reference to a type in Binaryen.
 *
 * @param varId
 *   The identifier of the export in JavaScript code.
 */
case class TypeRef(varId: VarId) extends wasm.Type with ToJSRepr:
  override def toJSRepr: Document = varId.toJSRepr
end TypeRef

/** A [[WasmGenerator]] backend that produces Binaryen.js Javascript calls as
 * its output.
 *
 * @param modId
 *   The identifier of which the Binaryen module is loaded and referred to in
 *   the JavaScript code.
 */
class BinaryenJSBackend(private[binaryen] val modId: Str = "binaryen")
    extends WasmGenerator[TypeRef, ModRef, ExprRef]
    with AutoCloseable:
  type TypeRefs = VarId

  /** A monotonically increasing counter for generating variable names of
   * intermediate Binaryen values.
   */
  private val varCounter = AtomicLong()

  /** The [[DocBuilder]] instance housing all generated JavaScript code. */
  private[binaryen] val db = DocBuilder()

  /** A [[set mutable.HashSet]] of all created modules by the Binaryen backend.
   */
  private val moduleIds = mutable.HashSet[VarId]()

  override lazy val none: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.none"
      TypeRef(freshId)
  override lazy val i32: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.i32"
      TypeRef(freshId)
  override lazy val i64: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.i64"
      TypeRef(freshId)
  override lazy val f32: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.f32"
      TypeRef(freshId)
  override lazy val f64: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.f64"
      TypeRef(freshId)
  override lazy val v128: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.v128"
      TypeRef(freshId)
  override lazy val funcref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.funcref"
      TypeRef(freshId)
  override lazy val externref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.externref"
      TypeRef(freshId)
  override lazy val anyref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.anyref"
      TypeRef(freshId)
  override lazy val eqref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.eqref"
      TypeRef(freshId)
  override lazy val i31ref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.i31ref"
      TypeRef(freshId)
  override lazy val structref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.structref"
      TypeRef(freshId)
  override lazy val stringref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.stringref"
      TypeRef(freshId)
  override lazy val unreachable: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.unreachable"
      TypeRef(freshId)

  /** Creates a possibly multi-valued type from a [[Seq]] of types. */
  def createType(types: Seq[TypeRef]): TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.createType(${types.map(_.toJSRepr).mkDocument("[", ", ", "]")})"
      TypeRef(freshId)
  override def createType(types: TypeRefs): TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.createType(${types.toJSRepr})"
      TypeRef(freshId)
  override def expandType(ty: TypeRef): TypeRefs =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.expandType(${ty.toJSRepr})"
      freshId

  override def getExpressionType(expr: ExprRef): TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.getExpressionType(${expr.toJSRepr})"
      TypeRef(freshId)

  /** Creates a fresh [[VarId]], executes [[block]], and returns the result of
   * the block.
   *
   * This is used to simplify capturing intermediate Binaryen values.
   */
  def withFreshVarId[T](block: VarId => T): T = block(
    VarId(varCounter.getAndIncrement())
  )

  override def newModule: ModRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = new $modId.Module()"
      moduleIds += freshId
      ModRef(this, freshId)

  override def close(): Unit =
    moduleIds.foreach: id =>
      db +=\\ doc"${id.toJSRepr}.drop()"
    moduleIds.clear()

  /** Converts all collected JavScript calls into a [[Document]] for execution.
   */
  def dumpJS: Document = db.toDoc

end BinaryenJSBackend
