package hkmc2.codegen
package wasm

import mlscript.utils.*, shorthands.*

import js.CodeBuilder

/** Abstract base class for all Wasm types. */
abstract class Type
private case object NoneType extends Type
private case object I32Type extends Type
private case object I64Type extends Type
private case object F32Type extends Type
private case object F64Type extends Type
private case object V128Type extends Type
private case object FuncRefType extends Type
private case object ExternRefType extends Type
private case object AnyRefType extends Type
private case object EqRefType extends Type
private case object I31RefType extends Type
private case object StructRefType extends Type
private case object StringRefType extends Type
private case object StringView_Wtf8Type extends Type
private case object StringView_Wtf16Type extends Type
private case object StringView_IterType extends Type
private case object UnreachableType extends Type
private case class MultiValueType(types: Seq[Type]) extends Type

/** Abstract class representing a Wasm `export` section. */
abstract class Export[E <: Export[E]]

/** Abstract class representing a Wasm expression, which is composed of zero or
 * more instructions.
 */
abstract class Expression[E <: Expression[E]]

/** Abstract class representing a Wasm function. */
abstract class Function[F <: Function[F]]:
  /** The type representing expressions within the function. */
  type Expr <: Expression[Expr]
end Function

/** Abstract class representing a Wasm `global` section. */
abstract class Global[G <: Global[G]]

/** Represention of a data segment used to initialize Wasm memories. See
 * [[https://webassembly.github.io/gc/core/text/modules.html#data-segments]]
 */
case class MemorySegment[E <: Expression[E]](
    offset: E,
    data: Seq[Byte],
    passive: Bool
)

/** Abstract class representing a Wasm `module`.
 */
abstract class Module:
  /** Abstract handle for `i32`-related instructions. */
  abstract class I32:
    /** Creates an `i32.const` instruction with the given `value`. */
    def const(value: Int): Expr

    /** Creates an `i32.add` instruction with the given values as operands. */
    def add(left: Expr, right: Expr): Expr
  end I32

  /** Abstract handle for `ref`-related instructions. */
  abstract class Ref:
    /** Creates a `ref.func` instruction to a function with the given `name` and
     * return type `ty`.
     */
    def func(name: Str, ty: Type): Expr

    /** Creates a `ref.i31` instruction with the given `value`. */
    def i31(value: Expr): Expr
  end Ref

  /** Abstract handle for `i31.ref`-related instructions. */
  abstract class I31Ref:
    /** Creates an `i31.get_{s,u}` instruction. */
    def get(i31: Expr, signed: Bool): Expr
  end I31Ref

  /** Concrete type representing an `export` section. */
  type Exprt <: Export[Exprt]

  /** Concrete type representing a Wasm expression. */
  type Expr <: Expression[Expr]

  /** Concrete type representing a `func` section. */
  type Func <: Function[Func]

  /** Concrete type representing a `global` section. */
  type Glob <: Global[Glob]

  /** Adds a function to this module. */
  def addFunction(
      name: Str,
      params: Type,
      results: Type,
      vars: Seq[Type],
      body: Expr
  ): Func

  /** Removes the function with the given `name` from this module. */
  def removeFunction(name: Str): Unit

  /** Adds a function `import` section to this module. */
  def addFunctionImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      params: Type,
      results: Type
  ): Unit

  /** Adds a table `import` section to this module. */
  def addTableImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit

  /** Adds a memory `import` section to this module. */
  def addMemoryImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str
  ): Unit

  /** Adds a global `import` section to this module. */
  def addGlobalImport(
      internalName: Str,
      externalModuleName: Str,
      externalBaseName: Str,
      globalType: Type
  ): Unit

  /** Adds a function `export` section to this module. */
  def addFunctionExport(internalName: Str, externalName: Str): Exprt

  /** Adds a table `export` section to this module. */
  def addTableExport(internalName: Str, externalName: Str): Exprt

  /** Adds a memory `export` section to this module. */
  def addMemoryExport(internalName: Str, externalName: Str): Exprt

  /** Adds a global `export` section to this module. */
  def addGlobalExport(internalName: Str, externalName: Str): Exprt

  /** Adds a `global` section defining a global variable to this module. */
  def addGlobal(name: Str, ty: Type, mutable: Bool, value: Expr): Glob

  /** Removes a `global` variable from this module. */
  def removeGlobal(name: Str): Unit

  /** Sets the properties for the default memory. */
  def setMemory(
      initial: Int,
      maximum: Int,
      exportName: Opt[Str],
      segments: Seq[MemorySegment[Expr]],
      shared: Bool = false
  ): Unit

  /** Sets the `start` function for this module. */
  def setStart(start: Func): Unit

  /** Creates a `block` instruction.
   *
   * @param label
   *   The label identifier of the block.
   * @param children
   *   The expression(s) contained in the block.
   * @param resultType
   *   The result type of the block.
   */
  def block(label: Opt[Str], children: Seq[Expr], resultType: Opt[Type]): Expr

  /** Creates a `nop` instruction. */
  def nop(): Expr

  /** Creates a `ret` instruction.
   *
   * @param value
   *   The value to return. If `None`, the function does not return a value.
   */
  def ret(value: Opt[Expr]): Expr

  /** Creates an `unreachable` instruction. */
  def unreachable(): Expr

  /** Creates a `drop` instruction.
   *
   * @param value
   *   The value to discard.
   */
  def drop(value: Expr): Expr

  /** Creates a `call` instruction.
   *
   * @param name
   *   The name of the function to call.
   * @param operands
   *   The arguments to pass to the function.
   * @param returnType
   *   The return type of the function.
   */
  def call(name: Str, operands: Seq[Expr], returnType: Type): Expr

  /** Create a `call_ref` instruction.
   *
   * @param target
   *   The function reference to call.
   * @param operands
   *   The arguments to pass to the function.
   * @param params
   *   The parameter types of the function.
   * @param results
   *   The result types of the function.
   */
  def callRef(
      target: Expr,
      operands: Seq[Expr],
      params: Type,
      results: Type
  ): Expr

  /** Returns a handle to create `i32` instructions. */
  def i32: I32

  /** Returns a handle to create `ref` instructions. */
  def ref: Ref

  /** Returns a handle to create `i31` instructions. */
  def i31ref: I31Ref

end Module

/** Base implementation for generating Wasm.
 *
 * This class should be implemented by all backends that generate Wasm code.
 *
 * @tparam M
 *   The backend-specific handle for Wasm modules.
 * @note
 *   The API of this class is based on the `binaryen.js` API.
 */
abstract class WasmGenerator[M <: Module] extends CodeBuilder:
  /** The none (i.e. `void`) type. */
  final def none: Type = NoneType

  /** The 32-bit integer type. */
  final def i32: Type = I32Type

  /** The 64-bit integer type. */
  final def i64: Type = I64Type

  /** The 32-bit floating point type. */
  final def f32: Type = F32Type

  /** The 64-bit floating point type. */
  final def f64: Type = F64Type

  /** The 128-bit vector type. */
  final def v128: Type = V128Type

  /** The function reference type. */
  final def funcref: Type = FuncRefType

  /** The external (host) reference type. */
  final def externref: Type = ExternRefType

  /** The any (⊤) reference type. */
  final def anyref: Type = AnyRefType

  /** The equal reference type. */
  final def eqref: Type = EqRefType

  /** The i31 reference type. */
  final def i31ref: Type = I31RefType

  /** The structure reference type. */
  final def structref: Type = StructRefType

  /** The string reference type. */
  final def stringref: Type = StringRefType

  final def stringview_wtf8: Type = StringView_Wtf8Type
  final def stringview_wtf16: Type = StringView_Wtf16Type
  final def stringview_iter: Type = StringView_IterType

  /** A special type indicating unreachable code when obtaining information
   * about an expression.
   */
  final def unreachable: Type = UnreachableType

  /** Creates a multi-value type from [[TypeRefs an array of types]].
   */
  def createType(types: Seq[Type]): Type =
    types.size match
      case 0 => NoneType
      case 1 => types.head
      case _ => MultiValueType(types)

  /** Expands a multi-value type to [[TypeRefs an array of types]]. */
  def expandType(ty: Type): Seq[Type] = ty match
    case MultiValueType(types) => types
    case NoneType              => Seq()
    case _                     => Seq(ty)

  /** Creates a new module using this backend. */
  def newModule: M

object WasmGenerator:
  /** Test function for creating a simple module. */
  def mkSimpleModule[M <: Module](gen: WasmGenerator[M]): M =
    val mod = gen.newModule
    locally:
      import mod._
      addFunction(
        "main",
        gen.none,
        gen.anyref,
        Seq(),
        ref.i31(i32.const(0))
      )
      addFunctionExport("main", "main")
    mod
end WasmGenerator
