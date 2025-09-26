package hkmc2
package codegen
package wasm
package text

import mlscript.utils.*, shorthands.*
import hkmc2.utils.*

import document.*
import document.Document
import semantics.*
import text.Param as WasmParam

import scala.collection.mutable.{ArrayBuffer as ArrayBuf, Map as MutMap}

class FuncInfo(
    val id: Opt[SymIdx],
    val typeIdx: TypeIdx,
    val params: Seq[Local -> Str],
    val nResults: Int,
    val locals: Seq[Local -> Str],
    val body: Expr
) extends ToWat:

  def this(
      sym: BlockMemberSymbol,
      typeIdx: TypeIdx,
      params: Seq[Local -> Str],
      nResults: Int,
      locals: Seq[Local -> Str],
      body: Expr
  ) = this(
    sym.optionIf(_.nameIsMeaningful).map(sym => SymIdx(sym.nme)),
    typeIdx,
    params,
    nResults,
    locals,
    body
  )

  def getSignatureType: SignatureType = SignatureType(
    params = params.map((_, varNme) => WasmParam(S(varNme), RefType.anyref)),
    results = Seq.fill(nResults)(Result(RefType.anyref))
  )

  def toWat: Document =
    doc"""(func ${id.fold(doc"")(_.toWat)} (type ${typeIdx.toWat})${
        getSignatureType.toWat.surroundUnlessEmpty(doc" ")
      } #{ ${
        locals.map: p =>
          doc"(local $$${p._2} ${RefType.anyref.toWat})"
        .toSeq.mkDocument(doc" # ").surroundUnlessEmpty(doc" # ")
      } # ${body.toWat} #} )${
        id.fold(doc""): id =>
          doc""" # (export "${id.id}" (func ${id.toWat})) # (elem declare func ${id.toWat})"""
      }"""
end FuncInfo

class TypeInfo(
    val id: Opt[SymIdx],
    val compType: CompType
) extends ToWat:

  def this(sym: BlockMemberSymbol, compType: CompType) = this(
    sym.optionIf(_.nameIsMeaningful).map(sym => SymIdx(sym.nme)),
    compType
  )

  def toWat: Document =
    doc"(type ${id.fold(doc"")(_.toWat).surroundUnlessEmpty(postfix = doc" ")}${compType.toWat})"
end TypeInfo

object Ctx:
  def empty: Ctx = Ctx(
    types = ArrayBuf.empty,
    namedTypes = MutMap.empty,
    funcs = ArrayBuf.empty,
    namedFuncs = MutMap.empty,
    locals = MutMap() :: Nil
  )

  def ctx(using ctx: Ctx): Ctx = ctx

  extension (ref: CtxIdx | Symbol)
    private def prettyString: Str = ref match
      case idx: CtxIdx => s"type index `${idx.toWat.toString}`"
      case sym: Symbol => s"symbol `${sym.toString}`"

class Ctx(
    types: ArrayBuf[TypeInfo],
    namedTypes: MutMap[Symbol, NumIdx],
    funcs: ArrayBuf[FuncInfo],
    namedFuncs: MutMap[Symbol, NumIdx],
    var locals: Ls[MutMap[Local, NumIdx]]
) extends ToWat:

  import Ctx.prettyString

  def addType(sym: Opt[Symbol], typeInfo: TypeInfo): TypeIdx =
    val numIdx = NumIdx(types.size)
    types += typeInfo
    sym.foreach:
      namedTypes(_) = numIdx
    TypeIdx(typeInfo.id.getOrElse(numIdx))

  def getType(typeref: TypeIdx | Symbol, resolveSymIdx: Bool = false): Opt[TypeIdx] = typeref match
    case TypeIdx(SymIdx(nme)) if resolveSymIdx =>
      namedTypes.find(_._1.nme == nme).map(t => TypeIdx(t._2))
    case typeidx: TypeIdx => S(typeidx)
    case sym: Symbol if resolveSymIdx => namedTypes.get(sym).map(TypeIdx(_))
    case sym: Symbol =>
      getType(sym, resolveSymIdx = true).map: numIdx =>
        getTypeInfo(numIdx).flatMap(_.id).fold(numIdx)(TypeIdx(_))

  def getType_!(typeref: TypeIdx | Symbol, resolveSymIdx: Bool = false): TypeIdx =
    getType(typeref, resolveSymIdx).getOrElse:
      lastWords(s"Missing type definition for ${typeref.prettyString}")

  def getTypeInfo(typeref: TypeIdx | Symbol): Opt[TypeInfo] = typeref match
    case TypeIdx(NumIdx(idx)) => types.unapply(idx.toInt)
    case TypeIdx(SymIdx(nme)) =>
      namedTypes.find(_._1.nme == nme).flatMap(t => getTypeInfo(TypeIdx(t._2)))
    case sym: Symbol => namedTypes.get(sym).flatMap(idx => getTypeInfo(TypeIdx(idx)))

  def getTypeInfo_!(typeref: TypeIdx | Symbol): TypeInfo =
    getTypeInfo(typeref).getOrElse:
      lastWords(s"Missing type definition for ${typeref.prettyString}")

  def addFunc(sym: Opt[Symbol], funcInfo: FuncInfo): FuncIdx =
    val numIdx = NumIdx(funcs.size)
    funcs += funcInfo
    sym.foreach:
      namedFuncs(_) = numIdx
    FuncIdx(funcInfo.id.getOrElse(numIdx))

  def getFunc(funcref: FuncIdx | Symbol, resolveSymIdx: Bool = false): Opt[FuncIdx] = funcref match
    case FuncIdx(SymIdx(nme)) if resolveSymIdx =>
      namedFuncs.find(_._1.nme == nme).map(f => FuncIdx(f._2))
    case funcidx: FuncIdx => S(funcidx)
    case sym: Symbol if resolveSymIdx => namedFuncs.get(sym).map(FuncIdx(_))
    case sym: Symbol =>
      getFunc(sym, resolveSymIdx = true).map: numIdx =>
        getFuncInfo(numIdx).flatMap(_.id).fold(numIdx)(FuncIdx(_))

  def getFunc_!(funcref: FuncIdx | Symbol, resolveSymIdx: Bool = false): FuncIdx =
    getFunc(funcref, resolveSymIdx).getOrElse:
      lastWords(s"Missing function definition for ${funcref.prettyString}")

  def getFuncInfo(funcref: FuncIdx | Symbol): Opt[FuncInfo] = funcref match
    case FuncIdx(NumIdx(idx)) => funcs.unapply(idx.toInt)
    case funcref => getFunc(funcref, resolveSymIdx = true).flatMap(getFuncInfo(_))

  def getFuncInfo_!(funcref: FuncIdx | Symbol): FuncInfo =
    getFuncInfo(funcref).getOrElse:
      lastWords(s"Missing function definition for ${funcref.prettyString}")

  def pushLocal(): Unit = locals = MutMap() :: locals
  def popLocal(): Unit = locals = locals.tail

  def addLocal(sym: Local): LocalIdx =
    val numIdx = NumIdx(locals.head.size)
    locals.head(sym) = numIdx
    LocalIdx(numIdx)

  def addLocals(syms: Seq[Local]): Seq[LocalIdx] =
    syms.map(addLocal)

  def containsLocal(sym: Local): Bool = locals.head.contains(sym)

  def getLocals: Ls[Seq[Local]] = locals.map(_.toSeq.sortBy(_._2.index).map(_._1))

  def toWat: Document =
    doc"""(module #{  # ${(types.toSeq ++ funcs.toSeq).map(_.toWat).mkDocument(doc" # ")}) #} """

end Ctx