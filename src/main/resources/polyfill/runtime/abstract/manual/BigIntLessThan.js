// https://tc39.es/ecma262/multipage/ecmascript-data-types-and-values.html#sec-numeric-types-bigint-lessThan
function BigInt__lessThan(x, y) {
  // 1. If x < y, return true; otherwise return false. BigInts carry no NaN, so
  //    the comparison is total.
  return x < y;
}

module.exports = BigInt__lessThan;
