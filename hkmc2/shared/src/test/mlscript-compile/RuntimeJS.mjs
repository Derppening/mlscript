import Predef from "./Predef.mjs";

const RuntimeJS = {
  bitor(lhs, rhs) {
    return lhs | rhs;
  },
  try_catch(computation, onError) {
    try { return computation() }
    catch (error) { return onError(error) }
  }
}

export default RuntimeJS;

