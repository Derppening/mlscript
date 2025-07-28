package hkmc2
package codegen.wasm
package text

import mlscript.utils.*, shorthands.*

import document.*

/** Abstract base class for all Wasm types. */
abstract class WasmType extends Type
private case object NoneType extends WasmType
private case object I32Type extends WasmType
private case object I64Type extends WasmType
private case object F32Type extends WasmType
private case object F64Type extends WasmType
private case object V128Type extends WasmType
private case object UnreachableType extends WasmType
private case class MultiValueType(types: Seq[WasmType]) extends WasmType

/** Enumeration of all Wasm packed types. */
enum WasmPackedType extends PackedType:
  case NotPacked
  case I8
  case I16
end WasmPackedType

/** Wasm type representing a reference to a [[HeapType]]. */
case class RefType(heapType: HeapType, nullable: Bool) extends WasmType

object HeapType:
  case object Func extends HeapType
  case object Ext extends HeapType
  case object Any extends HeapType
  case object Eq extends HeapType
  case object I31 extends HeapType
  case object Struct extends HeapType
  case object Array extends HeapType
  case object String extends HeapType
  case object None extends HeapType
  case object NoExt extends HeapType
  case object NoFunc extends HeapType

/** Abstract base class for all Wasm heap types. */
abstract class HeapType

/** A type representing a function signature. */
case class SignatureType(params: WasmType, results: WasmType) extends HeapType

object Field:
  /** Creates a field from a [[WasmType]]. */
  def apply(ty: WasmType, mutable: Bool) =
    new Field(ty, WasmPackedType.NotPacked, mutable)

  /** Creates a field from a [[WasmPackedType]]. */
  def apply(packedType: WasmPackedType, mutable: Bool) =
    assert(
      packedType != WasmPackedType.NotPacked,
      "Packed type must not be 'notPacked'"
    )
    new Field(I32Type, packedType, mutable)

/** A type represening a struct field. */
case class Field(ty: WasmType, packedType: WasmPackedType, mutable: Bool)

/** A type representing a structure type. */
case class StructType(fields: Seq[Field]) extends HeapType

/**
 * An abstraction over a generic WebAssembly instructions.
 */
abstract sealed class Instruction:
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

  /** Formats this instruction into a [[Document]]. */
  def fmtDoc: Document
end Instruction

/** A WebAssembly stack instruction. */
case class StackInstr(
    val mnemonic: Str,
    val instrargs: Seq[Any],
    val exprType: WasmType
) extends Instruction:
  def fmtDoc: Document = doc"$mnemonic${instrargs
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

  def fmtDoc: Document = doc"($mnemonic${instrargs
      .optionIf(_.nonEmpty)
      .dlof(_.map(_.toString).mkDocument(doc" ", doc" ", doc""))(doc"")}${stackargs
      .optionIf(_.nonEmpty)
      .dlof(stackarg =>
        doc" #{  # ${stackarg
            .map(sarg =>
              doc"${sarg match
                  case stackInstr: Ls[StackInstr] =>
                    stackInstr.map(_.fmtDoc).mkDocument(" # ")
                  case S(foldedInstr) => foldedInstr.fmtDoc
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
    val typeId: Str,
    val paramTypes: WasmType,
    val resultTypes: WasmType,
    val doc: Document
)

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
    ty: Seq[Str -> Document] = Seq(),
    im: Seq[Str -> Document] = Seq(),
    fn: Seq[Str -> ModFunc] = Seq(),
    ta: Seq[Str -> Document] = Seq(),
    me: Seq[Str -> Document] = Seq(),
    gl: Seq[Str -> Document] = Seq(),
    ex: Seq[Str -> Document] = Seq(),
    st: Opt[Str] = N,
    el: Seq[Str -> Document] = Seq(),
    da: Seq[Str -> Document] = Seq()
):
  def emitText: Document =
    doc"(module${id.dlof(id => doc" $id")(doc"")} #{  # ${Seq(
        ty.map(_._2),
        im.map(_._2),
        fn.map(_._2.doc),
        ta.map(_._2),
        me.map(_._2),
        gl.map(_._2),
        ex.map(_._2),
        st.map(st => doc"(start $$$st)").toSeq,
        el.map(_._2),
        da.map(_._2)
      ).flatten.mkDocument(Document.forceBreak)} #} )"
end Module
