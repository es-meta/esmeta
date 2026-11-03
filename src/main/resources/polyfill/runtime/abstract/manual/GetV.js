var AO__ToObject = require("./ToObject");

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-getv
function AO__GetV(V, P) {
  return V[P];
  // 1. Let _O_ be ? ToObject(_V_).
  var O = AO__ToObject(V);
  // 1. Return ? <emu-meta effects="user-code">_O_.[[Get]]</emu-meta>(_P_, _V_).
  //
}

module.exports = AO__GetV;
