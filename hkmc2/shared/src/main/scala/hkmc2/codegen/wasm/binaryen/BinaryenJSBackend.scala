package hkmc2
package codegen
package wasm
package binaryen

import mlscript.utils.*

import document.*
import shorthands.*

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import scala.collection.mutable

/** Trait indicating that a class can be lowered into JavaScript code, */
private trait ToJSRepr:
  /** Lowers this instance into JavaScript code for execution. */
  def toJSRepr: Document
end ToJSRepr

/**
 * Abstraction over a JavaScript variable with a unique number as its
 * identifier.
 *
 * This is used to represent intermediate values when generating JavaScript
 * code.
 */
case class VarId(id: Long) extends ToJSRepr:
  /** Returns the name of the variable, i.e. `_$id`. */
  def toJSRepr: Document = doc"_${id.toString}"
end VarId

/**
 * A reference to a Binaryen module.
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
  type Exprt = ExportRef
  type Func = FuncRef
  type FuncInfo = FuncInfoRef
  type Glob = GlobalRef

  def addFunction(
      name: Str,
      params: TypeRef,
      results: TypeRef,
      vars: Seq[TypeRef],
      body: ExprRef
  ): Func =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addFunction($name, ${params.toJSRepr}, ${results.toJSRepr}, ${vars
          .map(_.toJSRepr)
          .mkString("[", ", ", "]")}, ${body.toJSRepr});"
      new Func(freshId)

  /**
   * Gets a function by name.
   *
   * Generates a JavaScript runtime assertion if the function does not exist.
   */
  def getFunction(name: Str): Func =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.getFunction($name);"
      gen.db +=\\ doc"""assert(${freshId.toJSRepr}, "Function '$name' not found in module");"""
      new Func(freshId)

  def removeFunction(name: String): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.removeFunction($name);"

  def addFunctionImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      params: TypeRef,
      results: TypeRef
  ): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.addFunctionImport($internalName, $externalModuleName, $externalBaseName, ${params.toJSRepr}, ${results.toJSRepr});"
  def addTableImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.addTableImport($internalName, $externalModuleName, $externalBaseName);"
  def addMemoryImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.addMemoryImport($internalName, $externalModuleName, $externalBaseName);"
  def addGlobalImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      globalType: TypeRef
  ): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.addGlobalImport($internalName, $externalModuleName, $externalBaseName, ${globalType.toJSRepr});"

  def addFunctionExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addFunctionExport($internalName, $externalName);"
      new Exprt(freshId)
  def addTableExport(internalName: Str, externalName: Str): Exprt =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addTableExport($internalName, $externalName);"
      new Exprt(freshId)
  def addMemoryExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addMemoryExport($internalName, $externalName);"
      new Exprt(freshId)
  def addGlobalExport(
      internalName: Str,
      externalName: Str
  ): Exprt =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addGlobalExport($internalName, $externalName);"
      new Exprt(freshId)

  def addGlobal(
      name: Str,
      ty: TypeRef,
      mutable: Bool,
      value: ExprRef
  ): Glob =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.addGlobal($name, ${ty.toJSRepr}, ${
          if mutable then 1 else 0
        }, ${value.toJSRepr});"
      new Glob(freshId)
  def removeGlobal(name: Str): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.removeGlobal($name);"

  def setMemory(
      initial: Int,
      maximum: Int,
      exportName: Opt[Str],
      segments: Seq[MemorySegment[ExprRef]],
      shared: Bool
  ): Unit =
    gen.db +=\\
      doc"${this.toJSRepr}.setMemory(${initial.toString}, ${maximum.toString}, ${exportName.orNull}, ${segments
          .map(s =>
            s"{offset: ${s.offset.toJSRepr}, data: new Uint8Array(${s.data.mkString("[", ", ", "]")}), passive: ${s.passive}"
          )
          .mkString("[", ", ", "]")}, ${shared.toString});"

  def setStart(start: Func): Unit =
    gen.db +=\\ doc"${this.toJSRepr}.setStart(${start.toJSRepr});"

  def getFunctionInfo(ftype: Func): FuncInfo =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${this.toJSRepr}.getFunctionInfo(${ftype.toJSRepr});"
      new FuncInfo(freshId)

  def block(
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
            .getOrElse("")});"
      new ExprRef(freshId)

  def `if`(
      condition: ExprRef,
      ifTrue: ExprRef,
      ifFalse: Opt[ExprRef]
  ): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.if(${condition.toJSRepr}, ${ifTrue.toJSRepr}${ifFalse
            .dlof(iff => doc", ${iff.toJSRepr}")(doc"")});"
      new ExprRef(freshId)

  def nop(): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.nop();"
      new ExprRef(freshId)

  def ret(value: Opt[ExprRef]): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.return(${value.map(_.toJSRepr).getOrElse("")});"
      new ExprRef(freshId)

  def unreachable(): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.unreachable();"
      new ExprRef(freshId)

  def drop(value: ExprRef): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.drop(${value.toJSRepr});"
      new ExprRef(freshId)

  def call(
      name: Str,
      operands: Seq[ExprRef],
      returnType: TypeRef
  ): ExprRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\
        doc"${freshId.toJSRepr} = ${this.toJSRepr}.call($name, ${operands
            .map(_.toJSRepr)
            .mkDocument(doc"[", doc", ", doc"]")}, ${returnType.toJSRepr});"
      new ExprRef(freshId)

  def callRef(
      target: ExprRef,
      operands: Seq[ExprRef],
      params: TypeRef,
      results: TypeRef
  ): ExprRef =
    TODO("Binaryen.js does not support call_ref")

  def i32 = new I32:
    def const(value: Int): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.i32.const(${value.toString});"
        new ExprRef(freshId)

    def add(left: ExprRef, right: ExprRef): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.i32.add(${left.toJSRepr}, ${right.toJSRepr});"
        new ExprRef(freshId)
  end i32

  def ref: Ref = new Ref:
    def func(name: Str, ty: TypeRef): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.ref.func($name, ${ty.toJSRepr});"
        new ExprRef(freshId)

    def i31(value: ExprRef): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.ref.i31(${value.toJSRepr});"
        new ExprRef(freshId)

    def cast(value: ExprRef, castType: TypeRef): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.ref.cast(${value.toJSRepr}, ${castType.toJSRepr});"
        new ExprRef(freshId)
  end ref

  def i31ref: I31Ref = new I31Ref:
    def get(i31: ExprRef, signed: Bool): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.i31.get_${
            if signed then "s" else "u"
          }(${i31.toJSRepr});"
        new ExprRef(freshId)
  end i31ref

  def struct: Struct = new Struct:
    def `new`(operands: Seq[ExprRef], ty: TypeRef): ExprRef =
      gen.withFreshVarId: freshId =>
        gen.db +=\\ doc"${freshId.toJSRepr} = ${ModRef.this.toJSRepr}.struct.new(${operands
            .map(_.toJSRepr)
            .mkDocument("[", ", ", "]")}, ${ty.toJSRepr});"
        new ExprRef(freshId)
  end struct

  def toJSRepr: Document = varId.toJSRepr
end ModRef

/**
 * A reference to an `export` field in Binaryen.
 *
 * @param varId
 *   The identifier of the export in JavaScript code.
 */
case class ExportRef(varId: VarId) extends Export[ExportRef] with ToJSRepr:
  def toJSRepr: Document = varId.toJSRepr
end ExportRef

/**
 * A reference to an `func` field in Binaryen.
 *
 * @param varId
 *   The identifier of the export in JavaScript code.
 */
case class FuncRef(varId: VarId) extends Function[FuncRef] with ToJSRepr:
  def toJSRepr: Document = varId.toJSRepr
end FuncRef

/** A reference to a structure containing function information in Binaryen. */
case class FuncInfoRef(varId: VarId)
    extends FunctionInfo[TypeRef]
    with ToJSRepr:
  def toJSRepr: Document = varId.toJSRepr
end FuncInfoRef

/**
 * A reference to an expression in Binaryen.
 *
 * @param varId
 *   The identifier of the export in JavaScript code.
 */
case class ExprRef(varId: VarId) extends Expression[ExprRef] with ToJSRepr:
  def toJSRepr: Document = varId.toJSRepr
end ExprRef

/**
 * A reference to a global in Binaryen.
 *
 * @param varId
 *   The identifier of the export in JavaScript code.
 */
case class GlobalRef(varId: VarId) extends Global[GlobalRef] with ToJSRepr:
  def toJSRepr: Document = varId.toJSRepr
end GlobalRef

/**
 * A reference to a heap type builder in Binaryen.
 *
 * @param gen
 *   The [[BinaryenJSBackend]] instance that generates constructs for this type
 *   builder.
 * @param varId
 *   The identifier of the type builder in JavaScript code.
 */
case class TypeBuilder(gen: BinaryenJSBackend, varId: VarId)
    extends wasm.TypeBuilder[TypeRef, PackedTypeRef]
    with ToJSRepr:
  def setSignatureType(
      index: Int,
      paramTypes: TypeRef,
      resultTypes: TypeRef
  ): Unit =
    gen.db +=\\ doc"${varId.toJSRepr}.setSignatureType($index, ${paramTypes.toJSRepr}, ${resultTypes.toJSRepr});"

  def setStructType(
      index: Int,
      fields: Seq[(TypeRef | PackedTypeRef, Bool)]
  ): Unit =
    def fieldToObj(field: (TypeRef | PackedTypeRef, Bool)): Document =
      field._1 match
        case ty: TypeRef =>
          doc"{ type: ${ty.toJSRepr}, packedType: ${gen.notPacked.toJSRepr}, mutable: ${field._2.toString} }"
        case packedTy: PackedTypeRef =>
          assert(
            packedTy != gen.notPacked,
            "Packed type must not be 'notPacked'"
          )
          doc"{ type: ${gen.i32.toJSRepr}, packedType: ${packedTy.toJSRepr}, mutable: ${field._2.toString} }"

    gen.db +=\\ doc"${varId.toJSRepr}.setStructType($index, ${fields.map(fieldToObj).mkDocument("[", ", ", "]")});"

  def build(): TypeRef =
    gen.withFreshVarId: freshId =>
      gen.db +=\\ doc"${freshId.toJSRepr} = ${varId.toJSRepr}.buildAndDispose();"
      TypeRef(freshId)

  def toJSRepr: Document = varId.toJSRepr
end TypeBuilder

/**
 * A reference to a type in Binaryen.
 *
 * @param varId
 *   The identifier of the type in JavaScript code.
 */
case class TypeRef(varId: VarId) extends wasm.Type with ToJSRepr:
  def toJSRepr: Document = varId.toJSRepr
end TypeRef

/**
 * A reference to a packed type in Binaryen.
 *
 * @param varId
 *   The identifier of the packed type in JavaScript code.
 */
case class PackedTypeRef(varId: VarId) extends wasm.PackedType with ToJSRepr:
  def toJSRepr: Document = varId.toJSRepr
end PackedTypeRef

/**
 * A [[WasmGenerator]] backend that produces Binaryen.js Javascript calls as its
 * output.
 *
 * @param modId
 *   The identifier of which the Binaryen module is loaded and referred to in
 *   the JavaScript code.
 */
class BinaryenJSBackend(private[binaryen] val modId: Str = "binaryen")
    extends WasmGenerator[TypeRef, PackedTypeRef, ModRef, TypeBuilder, ExprRef]
    with AutoCloseable:
  type TypeRefs = VarId

  /**
   * A monotonically increasing counter for generating variable names of
   * intermediate Binaryen values.
   */
  private val varCounter = AtomicLong()

  /** The [[DocBuilder]] instance housing all generated JavaScript code. */
  private[binaryen] val db = DocBuilder()

  /**
   * A [[set mutable.HashSet]] of all created modules by the Binaryen backend.
   */
  private val moduleIds = mutable.HashSet[VarId]()

  lazy val none: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.none"
      TypeRef(freshId)
  lazy val i32: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.i32"
      TypeRef(freshId)
  lazy val i64: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.i64"
      TypeRef(freshId)
  lazy val f32: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.f32"
      TypeRef(freshId)
  lazy val f64: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.f64"
      TypeRef(freshId)
  lazy val v128: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.v128"
      TypeRef(freshId)
  lazy val funcref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.funcref"
      TypeRef(freshId)
  lazy val externref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.externref"
      TypeRef(freshId)
  lazy val anyref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.anyref"
      TypeRef(freshId)
  lazy val eqref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.eqref"
      TypeRef(freshId)
  lazy val i31ref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.i31ref"
      TypeRef(freshId)
  lazy val structref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.structref"
      TypeRef(freshId)
  lazy val stringref: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.stringref"
      TypeRef(freshId)
  lazy val unreachable: TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.unreachable"
      TypeRef(freshId)

  lazy val notPacked: PackedTypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.notPacked"
      PackedTypeRef(freshId)

  lazy val i8: PackedTypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.i8"
      PackedTypeRef(freshId)

  lazy val i16: PackedTypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.i16"
      PackedTypeRef(freshId)

  /** Creates a possibly multi-valued type from a [[Seq]] of types. */
  def createType(types: Seq[TypeRef]): TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.createType(${types.map(_.toJSRepr).mkDocument("[", ", ", "]")});"
      TypeRef(freshId)
  def createType(types: TypeRefs): TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.createType(${types.toJSRepr});"
      TypeRef(freshId)
  def expandType(ty: TypeRef): TypeRefs =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.expandType(${ty.toJSRepr});"
      freshId

  def getExpressionType(expr: ExprRef): TypeRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = $modId.getExpressionType(${expr.toJSRepr});"
      TypeRef(freshId)

  def getExpressionWasmType(
      expr: ExprRef,
      expectsValue: Bool
  ): TypeRef =
    val exprType = getExpressionType(expr)
    withFreshVarId: freshId =>
      db +=\\ doc"""if (${exprType.toJSRepr} == $modId.unreachable) {
        ${freshId.toJSRepr} = ${
          if expectsValue then doc"$modId.anyref" else doc"$modId.none"
        };
      } else {
        ${freshId.toJSRepr} = ${exprType.toJSRepr}
      }"""
      TypeRef(freshId)

  /**
   * Creates a fresh [[VarId]], executes [[block]], and returns the result of
   * the block.
   *
   * This is used to simplify capturing intermediate Binaryen values.
   */
  def withFreshVarId[T](block: VarId => T): T = block(
    VarId(varCounter.getAndIncrement())
  )

  def newModule: ModRef =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = new $modId.Module();"
      moduleIds += freshId
      ModRef(this, freshId)

  def newTypeBuilder(size: Int): TypeBuilder =
    withFreshVarId: freshId =>
      db +=\\ doc"${freshId.toJSRepr} = new $modId.newTypeBuilder($size);"
      TypeBuilder(this, freshId)

  def close(): Unit =
    moduleIds.foreach: id =>
      db +=\\ doc"${id.toJSRepr}.drop();"
    moduleIds.clear()

  /**
   * Converts all collected JavScript calls into a [[Document]] for execution.
   */
  def dumpJS: Document =
    val prelude = (0L until varCounter.get())
      .map(VarId(_).toJSRepr)
      .mkDocument("let ", ", ", ";")
    prelude :\\: db.toDoc

end BinaryenJSBackend
