// 7.3 Operations on Objects
// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-get-o-p
function AO__Get(O, P) {
  "use strict";

  // 1. Return ? O.[[Get]](P, O).
  return O[P];
}

module.exports = AO__Get;
