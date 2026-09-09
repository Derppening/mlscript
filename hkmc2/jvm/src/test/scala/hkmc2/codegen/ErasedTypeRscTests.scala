package hkmc2
package codegen

import org.scalatest.funsuite.AnyFunSuite

import hkmc2.utils.*, shorthands.*


/** Unit tests for the resource-ness dimension of [[ErasedType]].
  *
  * These pure functions are tested directly rather than through diff-tests because no `.mls` source can produce
  * a resource type yet: there is no syntax for one, so every `rsc` reaching the IR is still `S(false)` or `N`.
  * Every case below is therefore unreachable from a golden.
  */
class ErasedTypeRscTests extends AnyFunSuite:

  private val Rsc: Opt[Bool] = S(true)
  private val NonRsc: Opt[Bool] = S(false)
  private val Undetermined: Opt[Bool] = N

  test("lubRsc keeps a resource-ness both sides agree on"):
    assert(ErasedType.lubRsc(Rsc, Rsc) == Rsc)
    assert(ErasedType.lubRsc(NonRsc, NonRsc) == NonRsc)
    assert(ErasedType.lubRsc(Undetermined, Undetermined) == Undetermined)

  test("lubRsc joins a disagreement to the undetermined top"):
    assert(ErasedType.lubRsc(Rsc, NonRsc) == Undetermined)
    assert(ErasedType.lubRsc(NonRsc, Rsc) == Undetermined)

  test("lubRsc is absorbed by the undetermined top"):
    assert(ErasedType.lubRsc(Undetermined, Rsc) == Undetermined)
    assert(ErasedType.lubRsc(Rsc, Undetermined) == Undetermined)
    assert(ErasedType.lubRsc(Undetermined, NonRsc) == Undetermined)
    assert(ErasedType.lubRsc(NonRsc, Undetermined) == Undetermined)

  test("rscCast needs no cast when the layouts already agree"):
    assert(ErasedType.needsRscCast(Rsc, Rsc) == S(false))
    assert(ErasedType.needsRscCast(NonRsc, NonRsc) == S(false))
    assert(ErasedType.needsRscCast(Undetermined, Undetermined) == S(false))

  test("rscCast widens a known layout into the undetermined one for free"):
    assert(ErasedType.needsRscCast(Rsc, Undetermined) == S(false))
    assert(ErasedType.needsRscCast(NonRsc, Undetermined) == S(false))

  test("rscCast narrows out of the undetermined layout with a runtime test"):
    assert(ErasedType.needsRscCast(Undetermined, Rsc) == S(true))
    assert(ErasedType.needsRscCast(Undetermined, NonRsc) == S(true))

  test("rscCast rejects a coercion between the two layouts"):
    assert(ErasedType.needsRscCast(Rsc, NonRsc) == N)
    assert(ErasedType.needsRscCast(NonRsc, Rsc) == N)

  test("the canonical types that cannot carry a resource-ness of their own pick the right constant"):
    // * An unboxed primitive has no object header to hold a refcount, so it asserts the collected layout.
    assert(ErasedType.Primitive(PrimitiveType.Int32).rsc == NonRsc)
    // * An incompatibility has no representation at all, so it asserts nothing either way.
    val incompatible = ErasedType.Incompatible(ErasedType.Unknown(N), ErasedType.Unknown(Rsc))
    assert(incompatible.rsc == Undetermined)

end ErasedTypeRscTests
