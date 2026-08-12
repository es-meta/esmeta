// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-stringtonumber
function AO__StringToNumber(str) {
  // 1-3. Number(_str_) parses the StringNumericLiteral grammar the spec calls
  //      for, and yields NaN when the string does not match it.
  return Number(str);
}

module.exports = AO__StringToNumber;
