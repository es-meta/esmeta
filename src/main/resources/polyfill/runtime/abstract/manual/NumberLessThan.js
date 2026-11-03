// https://tc39.es/ecma262/multipage/ecmascript-data-types-and-values.html#sec-numeric-types-number-lessThan
function Number__lessThan(x, y) {
  // 1. If x is NaN, return undefined.
  // 2. If y is NaN, return undefined.
  if (x !== x || y !== y) return undefined;
  // 3. If x is y, return false.
  // 4. If x is +0 and y is -0, return false.
  // 5. If x is -0 and y is +0, return false.
  // 6-9. Otherwise order by mathematical value, with the infinities at the ends.
  return x < y;
}

module.exports = Number__lessThan;
