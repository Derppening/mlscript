package hkmc2
package codegen.wasm
package text

import mlscript.utils.*, shorthands.*

import document.*

/** Trait indicating a WAT representation is available. */
trait ToWat:

  /** Converts this object into a WAT representation. */
  def toWat: Document
end ToWat

/** Abstract base class for all Wasm types. */
abstract class WasmType extends Type, ToWat:
  def toSeq: Seq[WasmType] = this match
    case MultiValueType(types) => types
    case NoneType => Seq()
    case ty => Seq(ty)
end WasmType

private case object NoneType extends WasmType:
  def toWat: Document = throw UnsupportedOperationException(
    s"${toString} is a compiler-internal type and cannot be converted to WAT"
  )
end NoneType
private case object I32Type extends WasmType:
  def toWat: Document = doc"i32"
end I32Type
private case object I64Type extends WasmType:
  def toWat: Document = doc"i64"
end I64Type
private case object F32Type extends WasmType:
  def toWat: Document = doc"f32"
end F32Type
private case object F64Type extends WasmType:
  def toWat: Document = doc"f64"
end F64Type
private case object V128Type extends WasmType:
  def toWat: Document = doc"v128"
end V128Type
private case object UnreachableType extends WasmType:
  def toWat: Document = throw UnsupportedOperationException(
    s"${toString} is a compiler-internal type and cannot be converted to WAT"
  )
end UnreachableType
private case class MultiValueType(types: Seq[WasmType]) extends WasmType:
  def toWat: Document = throw UnsupportedOperationException(
    s"${toString} is a compiler-internal type and cannot be converted to WAT"
  )
end MultiValueType

type NumType = I32Type.type | I64Type.type | F32Type.type | F64Type.type
type VecType = V128Type.type

/** Enumeration of all Wasm packed types. */
enum WasmPackedType extends PackedType, ToWat:
  case NotPacked
  case I8
  case I16

  def toWat: Document = this match
    case WasmPackedType.I8 => doc"i8"
    case WasmPackedType.I16 => doc"i16"
    case _ => throw IllegalArgumentException(
        "WasmPackedType.NotPacked cannot be converted to Wat"
      )
  end toWat
end WasmPackedType

object RefType:
  def anyref: RefType = RefType(HeapType.Any, nullable = true)
  def i31ref: RefType = RefType(HeapType.I31, nullable = true)
  def funcref: RefType = RefType(HeapType.Func, nullable = true)

/** Wasm type representing a reference to a [[HeapType]]. */
case class RefType(heapType: HeapType, nullable: Bool) extends WasmType:
  def toWat: Document =
    doc"(ref${if nullable then " null" else ""} ${heapType.toWat})"
end RefType

object HeapType:
  case object Func extends HeapType:
    def toWat: Document = doc"func"
  end Func
  case object Ext extends HeapType:
    def toWat: Document = doc"extern"
  end Ext
  case object Any extends HeapType:
    def toWat: Document = doc"any"
  end Any
  case object Eq extends HeapType:
    def toWat: Document = doc"eq"
  end Eq
  case object I31 extends HeapType:
    def toWat: Document = doc"i31"
  end I31
  case object Struct extends HeapType:
    def toWat: Document = doc"struct"
  end Struct
  case object Array extends HeapType:
    def toWat: Document = doc"array"
  end Array
  case object None extends HeapType:
    def toWat: Document = doc"none"
  end None
  case object NoExt extends HeapType:
    def toWat: Document = doc"noextern"
  end NoExt
  case object NoFunc extends HeapType:
    def toWat: Document = doc"nofunc"
  end NoFunc

/** Abstract base class for all Wasm heap types. */
abstract class HeapType extends ToWat

type ValType = NumType | VecType | RefType

case class Param(id: Opt[Str], valtype: ValType) extends ToWat:
  def toWat: Document =
    doc"(param${id.fold(doc"")(id => doc" $id")} ${valtype.toWat})"
end Param

case class Result(valtype: ValType) extends ToWat:
  def toWat: Document = doc"(result ${valtype.toWat})"
end Result

object SignatureType:
  def apply(params: WasmType, results: WasmType): SignatureType =
    new SignatureType(
      params = params.toSeq.map(p => Param(N, p.asInstanceOf[ValType])),
      results = results.toSeq.map(r => Result(r.asInstanceOf[ValType]))
    )

/** A type representing a function signature. */
case class SignatureType(params: Seq[Param], results: Seq[Result])
    extends HeapType,
      ToWat:

  def signatureToWat: Document =
    (params.map(_.toWat) ++ results.map(_.toWat)).mkDocument(doc" ")

  def toWat: Document =
    doc"(func${signatureToWat.optionUnless(_.isEmpty).dlof(sig => doc" $sig")(doc"")})"
end SignatureType

object Field:
  /** Creates a field from a [[WasmType]]. */
  def apply(ty: WasmType, mutable: Bool, id: Opt[Str]) =
    new Field(ty, WasmPackedType.NotPacked, mutable, id)

  /** Creates a field from a [[WasmPackedType]]. */
  def apply(packedType: WasmPackedType, mutable: Bool, id: Opt[Str]) =
    assert(
      packedType != WasmPackedType.NotPacked,
      "Packed type must not be 'notPacked'"
    )
    new Field(I32Type, packedType, mutable, id)

/** A type represening a struct field. */
case class Field(
    ty: WasmType,
    packedType: WasmPackedType,
    mutable: Bool,
    id: Opt[Str]
) extends ToWat:
  def toWat: Document =
    val tyWat = if packedType != WasmPackedType.NotPacked then packedType.toWat
    else ty.toWat
    doc"(field ${id.dlof(id => doc"$$$id ")(doc"")}${
        if mutable then doc"(mut ${tyWat})" else tyWat
      })"
end Field

/** A type representing a structure type. */
case class StructType(fields: Seq[Field]) extends HeapType:
  def toWat: Document =
    doc"(struct${fields.map(
        _.toWat
      ).mkDocument(doc" ").optionUnless(_.isEmpty).dlof(f => doc" $f")(doc"")})"
end StructType

/** A composite type. */
type CompType = StructType | SignatureType

abstract class TypeRef extends HeapType, ToWat

case class TypeIdx(idx: Long) extends TypeRef:
  def toWat: Document = doc"${idx.toString}"
end TypeIdx

/**
 * A type that is referenced by its name.
 *
 * This is used for types that are defined in the module's `type` section.
 *
 * @param id
 *   The identifier of the type.
 */
case class TypeId(id: Str) extends TypeRef:
  def toWat: Document = doc"$$$id"
end TypeId

/**
 * A functionthat is referenced by its name.
 *
 * This is used for types that are defined in the module's `func` section.
 *
 * @param id
 *   The identifier of the function.
 */
case class FuncRef(id: Str) extends HeapType, ToWat:
  def toWat: Document = doc"$$$id"
end FuncRef

/**
 * An abstraction over a generic WebAssembly instructions.
 */
abstract sealed class Instruction extends ToWat:
  /** The mnemonic of the instruction, e.g. "i32.add". */
  val mnemonic: String

  /**
   * The arguments to the instruction. Note that this only includes arguments
   * that are directly part of the instruction, not the stack arguments.
   *
   * For example, for `i32.add` this would be empty, but for `i32.const 42`,
   * this would be `Seq(42)`.
   */
  val instrargs: Seq[Any]

  /** The result type of this expression. */
  val exprType: WasmType
end Instruction

/** A WebAssembly stack instruction. */
case class StackInstr(
    val mnemonic: Str,
    val instrargs: Seq[Any],
    val exprType: WasmType
) extends Instruction:
  def toWat: Document = doc"$mnemonic${instrargs
      .optionIf(_.nonEmpty)
      .dlof(_.map(_.toString).mkDocument(doc" ", doc" ", doc""))(doc"")}"
end StackInstr

object FoldedInstr:
  /**
   * Instruction mnemonics that do not (yet) support lowering from folded
   * instructions to stack instructions.
   */
  val unsupportedToStackMnemonics = Set("if", "then", "else")

/**
 * A WebAssembly folded instruction.
 *
 * @param stackargs
 *   The stack arguments of the instruction.
 */
case class FoldedInstr(
    val mnemonic: Str,
    val instrargs: Seq[Any],
    stackargs: Seq[Expr],
    val exprType: WasmType
) extends Instruction:
  /** Converts this folded instruction into a sequence of stack instructions. */
  def toStack: Ls[StackInstr] =
    if FoldedInstr.unsupportedToStackMnemonics contains mnemonic then
      TODO(
        s"Lowering of `${mnemonic}` to stack instruction not implemented"
      )

    stackargs
      .flatMap: arg =>
        arg match
          case stackInstrs: Ls[StackInstr] => stackInstrs
          case foldedInstr: Opt[FoldedInstr] =>
            foldedInstr.map(_.toStack).getOrElse(Ls())
      .toList :+ StackInstr(mnemonic, instrargs, exprType)

  def toWat: Document = doc"($mnemonic${instrargs
      .optionIf(_.nonEmpty)
      .dlof(_.map(_.toString).mkDocument(doc" ", doc" ", doc""))(doc"")}${stackargs
      .optionIf(_.nonEmpty)
      .dlof(stackarg =>
        doc" #{  # ${stackarg
            .map(sarg =>
              doc"${sarg match
                  case stackInstr: Ls[StackInstr] =>
                    stackInstr.map(_.toWat).mkDocument(" # ")
                  case S(foldedInstr) => foldedInstr.toWat
                  case N => doc""
                }"
            )
            .mkDocument(doc" # ")} #} "
      )(doc"")})"
end FoldedInstr

/**
 * A WebAssembly expression, comprised of zero of more instructions that
 * generate a result value.
 */
type Expr = Opt[FoldedInstr] | Ls[StackInstr]

/**
 * A module type.
 *
 * @param defn
 *   The definition of the `type` declaration.
 * @param doc
 *   The content of the module type.
 */
case class ModType(val defn: CompType, val doc: Document) extends ToWat:
  def toWat: Document = doc
end ModType

/**
 * A module function.
 *
 * @param typeId
 *   The identifier of the function type in the module's `type` section.
 * @param paramTypes
 *   The parameter type(s) of this function, without the `(param)` construct.
 * @param resultTypes
 *   The result type(s) of this function, without the `(result)` construct.
 * @param doc
 *   The content of the module function.
 */
case class ModFunc(
    val typeId: TypeId,
    val paramTypes: WasmType,
    val resultTypes: WasmType,
    val doc: Document
) extends ToWat:
  def toWat: Document = doc
end ModFunc

/**
 * A WebAssembly module definition.
 *
 * @param id
 *   The identifier of the module, if any.
 * @param ty
 *   The types defined in the module.
 * @param im
 *   The imports defined in the module.
 * @param fn
 *   The functions defined in the module.
 * @param ta
 *   The tables defined in the module.
 * @param me
 *   The memories defined in the module.
 * @param gl
 *   The globals defined in the module.
 * @param ex
 *   The exports defined in the module.
 * @param st
 *   The start function of the module, if any.
 * @param el
 *   The element segments defined in the module.
 * @param da
 *   The data segments defined in the module.
 */
case class Module(
    id: Opt[Str] = N,
    ty: Seq[Str -> ModType] = Seq(),
    im: Seq[Str -> Document] = Seq(),
    fn: Seq[Str -> ModFunc] = Seq(),
    ta: Seq[Str -> Document] = Seq(),
    me: Seq[Str -> Document] = Seq(),
    gl: Seq[Str -> Document] = Seq(),
    ex: Seq[Str -> Document] = Seq(),
    st: Opt[Str] = N,
    el: Seq[Str -> Document] = Seq(),
    da: Seq[Str -> Document] = Seq()
) extends ToWat:
  def toWat: Document =
    doc"(module${id.dlof(id => doc" $id")(doc"")} #{  # ${Seq(
        ty.map(_._2.toWat),
        im.map(_._2),
        fn.map(_._2.toWat),
        ta.map(_._2),
        me.map(_._2),
        gl.map(_._2),
        ex.map(_._2),
        st.map(st => doc"(start $$$st)").toSeq,
        el.map(_._2),
        da.map(_._2)
      ).flatten.mkDocument(Document.forceBreak)} #} )"
end Module
