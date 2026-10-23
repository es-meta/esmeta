// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-hasproperty
function AO__HasProperty(O, P) {
  "use strict";

  // 1. Return ? O.[[HasProperty]](P).
  return P in O;
}

module.exports = AO__HasProperty;
