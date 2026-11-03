// https://tc39.es/ecma262/multipage/ecmascript-data-types-and-values.html#sec-numeric-types-number-sameValueZero
function NUM__sameValueZero(x, y) {
  // 1. If x is NaN and y is NaN, return true.
  if (isNaN(x) && isNaN(y)) return true;
  // 2. If x is +0𝔽 and y is -0𝔽, return true.
  if (x === 0 && y === -0) return true;
  // 3. If x is -0𝔽 and y is +0𝔽, return true.
  if (x === -0 && y === 0) return true;
  // 4. If x is y, return true.
  if (x === y) return true;
  // 5. Return false.
  return false;
}

module.exports = NUM__sameValueZero;
