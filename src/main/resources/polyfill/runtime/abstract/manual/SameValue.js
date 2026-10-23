function AO__SameValue(x, y) {
  return Object.is(x, y);
  /*
  // 1. If SameType(_x_, _y_) is *false*, return *false*.
  // if (AO__SameType(x, y) === false) return false; // function type에 대해 검사하지 못함
  // 1. If _x_ is a Number, then
  if (typeof x === "number") {
    // 1. Return Number::sameValue(_x_, _y_).
    return NUM__sameValue(x, y);
  }
  // 1. Return SameValueNonNumber(_x_, _y_).
  return AO__SameValueNonNumber(x, y);
  */
}

module.exports = AO__SameValue;
