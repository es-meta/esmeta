var NUM__sameValueZero = require("./number/sameValueZero");
var AO__SameType = require("./SameType");

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-samevaluezero
function AO__SameValueZero(x, y) {
  "use strict";
  // 1. If SameType(x, y) is false, return false.
  if (AO__SameType(x, y) === false) return false;
  // 2. If x is a Number, then
  if (typeof x === "number") {
    // a. Return Number::sameValueZero(x, y).
    return NUM__sameValueZero(x, y);
  }
  // 3. Return SameValueNonNumber(x, y).
  return x === y;
}

module.exports = AO__SameValueZero;
