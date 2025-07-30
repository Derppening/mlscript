package hkmc2.codegen
package wasm

import mlscript.utils.*, shorthands.*

import js.CodeBuilder

/** Abstract class representing a Wasm `export` section. */
abstract class Export[E <: Export[E]]

/**
 * Abstract class representing a Wasm expression, which is composed of zero or
 * more instructions.
 */
abstract class Expression[E <: Expression[E]]

/**
 * Abstract class representing a structure containing information of a function.
 *
 * @tparam T
 *   The type representing Wasm types.
 */
abstract class FunctionInfo[T <: Type]

/** Abstract class representing a Wasm function. */
abstract class Function[F <: Function[F]]:
  /** The type representing expressions within the function. */
  type Expr <: Expression[Expr]
end Function

/** Abstract class representing a Wasm `global` section. */
abstract class Global[G <: Global[G]]

/**
 * Represention of a data segment used to initialize Wasm memories. See
 * [[https://webassembly.github.io/gc/core/text/modules.html#data-segments]]
 */
case class MemorySegment[E <: Expression[E]](
    offset: E,
    data: Seq[Byte],
    passive: Bool
)

/** Abstract class representing a Wasm type. */
abstract class Type

/** Abstract class representing a Wasm packed type. */
abstract class PackedType

/** Abstract class representing a builder that creates heap types. */
abstract class TypeBuilder[T <: Type, PT <: PackedType]:

  /**
   * Sets the type at `index` to be a signature type with the given `paramTypes`
   * and `resultTypes`.
   */
  def setSignatureType(index: Int, paramTypes: T, resultTypes: T): Unit

  /**
   * Sets the type at `index` to be a struct type with the given fields.
   *
   * The tuple of each field should contain the Wasm type or the Wasm packed
   * type, and whether the field is mutable respectively.
   */
  def setStructType(index: Int, fields: Seq[(T | PT, Bool)] = Seq()): Unit

  /** Builds a heap type from this instance. */
  def build(): T
end TypeBuilder

/**
 * Abstract class representing a Wasm `module`.
 *
 * @tparam Type
 *   The backend-specific handle for Wasm types.
 * @tparam Expr
 *   The backend-specific handle for Wasm expressions.
 */
abstract class Module[Type <: wasm.Type, Expr <: Expression[Expr]]:
  /** Abstract handle for `i32`-related instructions. */
  abstract class I32:
    /** Creates an `i32.const` instruction with the given `value`. */
    def const(value: Int): Expr

    /** Creates an `i32.add` instruction with the given values as operands. */
    def add(left: Expr, right: Expr): Expr
  end I32

  /** Abstract handle for `ref`-related instructions. */
  abstract class Ref:
    /**
     * Creates a `ref.func` instruction to a function with the given `name` and
     * return type `ty`.
     */
    def func(name: Str, ty: Type): Expr

    /** Creates a `ref.i31` instruction with the given `value`. */
    def i31(value: Expr): Expr

    /**
     * Creates a `ref.null` instruction, downcasting the value to `castType`.
     */
    def cast(value: Expr, castType: Type): Expr
  end Ref

  /** Abstract handle for `i31.ref`-related instructions. */
  abstract class I31Ref:
    /** Creates an `i31.get_{s,u}` instruction. */
    def get(i31: Expr, signed: Bool): Expr
  end I31Ref

  /** Abstract handle for `struct`-related instructions. */
  abstract class Struct:
    /** Creates a `struct.new` instruction. */
    def `new`(operands: Seq[Expr], ty: Type): Expr
  end Struct

  /** Concrete type representing an `export` section. */
  type Exprt <: Export[Exprt]

  /** Concrete type representing a `func` section. */
  type Func <: Function[Func]

  /**
   * Concrete type representing a structure containing information of a
   * function.
   */
  type FuncInfo <: FunctionInfo[Type]

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

  /**
   * Gets a function by name.
   *
   * Refer to the implementation documentation for the specific handling if the
   * function with the given name is not found.
   */
  def getFunction(name: Str): Func

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

  /** Obtains information about a function. */
  def getFunctionInfo(name: Func): FuncInfo

  /**
   * Creates a `block` instruction.
   *
   * @param label
   *   The label identifier of the block.
   * @param children
   *   The expression(s) contained in the block.
   * @param resultType
   *   The result type of the block.
   */
  def block(
      label: Opt[Str],
      children: Seq[Expr],
      resultType: Opt[Type]
  ): Expr

  /**
   * Creates an `if` instruction.
   */
  def `if`(condition: Expr, ifTrue: Expr, ifFalse: Opt[Expr]): Expr

  /** Creates a `nop` instruction. */
  def nop(): Expr

  /**
   * Creates a `ret` instruction.
   *
   * @param value
   *   The value to return. If `None`, the function does not return a value.
   */
  def ret(value: Opt[Expr]): Expr

  /** Creates an `unreachable` instruction. */
  def unreachable(): Expr

  /**
   * Creates a `drop` instruction.
   *
   * @param value
   *   The value to discard.
   */
  def drop(value: Expr): Expr

  /**
   * Creates a `call` instruction.
   *
   * @param name
   *   The name of the function to call.
   * @param operands
   *   The arguments to pass to the function.
   * @param returnType
   *   The return type of the function.
   */
  def call(name: Str, operands: Seq[Expr], returnType: Type): Expr

  /**
   * Create a `call_ref` instruction.
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

  /** Returns a handle to create `struct` instructions. */
  def struct: Struct

end Module

/**
 * Base implementation for generating Wasm.
 *
 * This class should be implemented by all backends that generate Wasm code.
 *
 * @tparam T
 *   The backend-specific handle for Wasm types.
 * @tparam M
 *   The backend-specific handle for Wasm modules.
 * @tparam TB
 *   The backend-specific handle for heap type builders.
 * @tparam E
 *   The backend-specific handle for Wasm expressions.
 * @note
 *   The API of this class is based on the `binaryen.js` API.
 */
abstract class WasmGenerator[T <: Type, PT <: PackedType, M <: Module[
  T,
  E
], TB <: TypeBuilder[T, PT], E <: Expression[E]]
    extends CodeBuilder:

  /** Type alias for representing multiple Wasm types. */
  type TypeRefs

  /** The none (i.e. `void`) type. */
  lazy val none: T

  /** The 32-bit integer type. */
  lazy val i32: T

  /** The 64-bit integer type. */
  lazy val i64: T

  /** The 32-bit floating point type. */
  lazy val f32: T

  /** The 64-bit floating point type. */
  lazy val f64: T

  /** The 128-bit vector type. */
  lazy val v128: T

  /** The function reference type. */
  lazy val funcref: T

  /** The external (host) reference type. */
  lazy val externref: T

  /** The any (⊤) reference type. */
  lazy val anyref: T

  /** The equal reference type. */
  lazy val eqref: T

  /** The i31 reference type. */
  lazy val i31ref: T

  /** The structure reference type. */
  lazy val structref: T

  /**
   * A special type indicating unreachable code when obtaining information about
   * an expression.
   */
  lazy val unreachable: T

  /** A special packed type indicating that a type is not packed. */
  lazy val notPacked: PT

  /** The 8-bit integer packed type. */
  lazy val i8: PT

  /** The 16-bit integer packed type. */
  lazy val i16: PT

  /**
   * Creates a multi-value type from [[TypeRefs an array of types]].
   */
  def createType(types: TypeRefs): T

  /** Expands a multi-value type to [[TypeRefs an array of types]]. */
  def expandType(ty: T): TypeRefs

  /** Returns the type of this expression `expr`. */
  def getExpressionType(expr: E): T

  /**
   * Returns the type of this expression `expr`, lowering internal types into
   * Wasm types where necessary.
   *
   * @param expectsValue
   *   Whether this expression is in a context where a value is expected to be
   *   generated.
   */
  def getExpressionWasmType(expr: E, expectsValue: Bool): T

  /** Creates a new module using this backend. */
  def newModule: M

  /**
   * Creates a new type builder using this backend for generating heap types.
   *
   * @param size
   *   The initial size of the type builder.
   */
  def newTypeBuilder(size: Int = 0): TB

object WasmGenerator:
  /** Test function for creating a simple module. */
  def mkSimpleModule[T <: Type, PT <: PackedType, M <: Module[
    T,
    E
  ], TB <: TypeBuilder[T, PT], E <: Expression[E]](
      gen: WasmGenerator[T, PT, M, TB, E]
  ): M =
    val mod = gen.newModule
    locally:
      import mod.*
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
