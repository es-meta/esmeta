var AO__UTF16SurrogatePairToCodePoint = require("./UTF16SurrogatePairToCodePoint");

// https://tc39.es/ecma262/multipage/ecmascript-language-source-code.html#sec-codepointat
function AO__CodePointAt(string, position) {
  // 1. Let size be the length of string.
  var size = string.length;
  // 2. Assert: position ≥ 0 and position < size.
  // 3. Let first be the code unit at index position within string.
  var first = string.charCodeAt(position);
  // 4. Let cp be the code point whose numeric value is the numeric value of first.
  var cp = first;

  // leading: 0xD800 to 0xDBFF
  // trailing: 0xDC00 to 0xDFFF
  // 5. If first is neither a leading surrogate nor a trailing surrogate, then
  if (first < 0xd800 || first > 0xdfff) {
    // a. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: false }.
    return { CodePoint: cp, CodeUnitCount: 1, IsUnpairedSurrogate: false };
  }
  // 6. If first is a trailing surrogate or position + 1 = size, then
  if ((first >= 0xdc00 && first <= 0xdfff) || position + 1 === size) {
    // a. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: true }.
    return { CodePoint: cp, CodeUnitCount: 1, IsUnpairedSurrogate: true };
  }

  // 7. Let second be the code unit at index position + 1 within string.
  var second = string.charCodeAt(position + 1);
  // 8. If second is not a trailing surrogate, then
  if (second < 0xdc00 || second > 0xdfff) {
    // a. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 1, [[IsUnpairedSurrogate]]: true }.
    return { CodePoint: cp, CodeUnitCount: 1, IsUnpairedSurrogate: true };
  }
  // 9. Set cp to UTF16SurrogatePairToCodePoint(first, second).
  cp = AO__UTF16SurrogatePairToCodePoint(first, second);
  // 10. Return the Record { [[CodePoint]]: cp, [[CodeUnitCount]]: 2, [[IsUnpairedSurrogate]]: false }.
  return { CodePoint: cp, CodeUnitCount: 2, IsUnpairedSurrogate: false };
}

module.exports = AO__CodePointAt;
