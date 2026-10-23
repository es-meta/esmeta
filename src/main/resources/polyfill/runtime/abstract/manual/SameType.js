// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-sametype
function AO__SameType(x, y) {
  // 1. If x is undefined and y is undefined, return true.
  if (x === undefined && y === undefined) return true;
  // 2. If x is null and y is null, return true.
  if (x === null && y === null) return true;
  // 3. If x is a Boolean and y is a Boolean, return true.
  if (typeof x === "boolean" && typeof y === "boolean") return true;
  // 4. If x is a Number and y is a Number, return true.
  if (typeof x === "number" && typeof y === "number") return true;
  // 5. If x is a BigInt and y is a BigInt, return true.
  if (typeof x === "bigint" && typeof y === "bigint") return true;
  // 6. If x is a Symbol and y is a Symbol, return true.
  if (typeof x === "symbol" && typeof y === "symbol") return true;
  // 7. If x is a String and y is a String, return true.
  if (typeof x === "string" && typeof y === "string") return true;
  // 8. If x is an Object and y is an Object, return true.
  if (typeof x === "object" && typeof y === "object") return true;
  // 9. Return false.
  return false;
}

module.exports = AO__SameType;
