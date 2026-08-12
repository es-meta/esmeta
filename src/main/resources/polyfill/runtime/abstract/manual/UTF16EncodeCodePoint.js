// 11.1 Source Text
// https://tc39.es/ecma262/multipage/ecmascript-language-source-code.html#sec-utf16encodecodepoint
function AO__UTF16EncodeCodePoint(cp) {
  // 1. Assert: 0 ≤ cp ≤ 0x10FFFF.
  // 2. If cp ≤ 0xFFFF, return the String value consisting of the code unit whose numeric value is cp.
  if (cp <= 0xffff) return String.fromCharCode(cp);
  // 3. Let cu1 be the code unit whose numeric value is Math.floor((cp - 0x10000) / 0x400) + 0xD800.
  var cu1 = Math.floor((cp - 0x10000) / 0x400) + 0xd800;
  // 4. Let cu2 be the code unit whose numeric value is ((cp - 0x10000) modulo 0x400) + 0xDC00.
  var cu2 = ((cp - 0x10000) % 0x400) + 0xdc00;
  // 5. Return the string-concatenation of cu1 and cu2.
  return String.fromCharCode(cu1) + String.fromCharCode(cu2);
}

module.exports = AO__UTF16EncodeCodePoint;
