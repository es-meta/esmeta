// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-stringtobigint
function AO__StringToBigInt(str) {
  // A host without BigInt cannot represent the result at all.
  if (typeof BigInt === "undefined") return undefined;
  // 1-3. BigInt(_str_) parses the StringIntegerLiteral grammar the spec calls
  //      for, but throws where the spec returns undefined.
  try {
    return BigInt(str);
  } catch (e) {
    return undefined;
  }
}

module.exports = AO__StringToBigInt;
