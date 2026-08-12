// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-isstrictlyequal
function AO__IsStrictlyEqual(x, y) {
  return x === y;
  // 1. If SameType(x, y) is false, return false.
  // 2. If x is a Number, then
  //    a. Return Number::equal(x, y).
  // 3. Return SameValueNonNumber(x, y).
}

module.exports = AO__IsStrictlyEqual;
