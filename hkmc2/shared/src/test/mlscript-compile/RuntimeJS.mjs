import Predef from "./Predef.mjs";

const RuntimeJS = {
  bitand(lhs, rhs) {
    return lhs & rhs;
  },
  bitnot(v) {
    return ~v;
  },
  bitor(lhs, rhs) {
    return lhs | rhs;
  },
  shl(v, sh) {
    return v << sh;
  },
  try_catch(computation, onError) {
    try { return computation() }
    catch (error) { return onError(error) }
  }
}

export default RuntimeJS;

