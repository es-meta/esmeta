// https://tc39.es/ecma262/multipage/ecmascript-language-source-code.html#sec-utf16decodesurrogatepair
function AO__UTF16SurrogatePairToCodePoint(lead, trail) {
  return (lead - 0xd800) * 0x400 + (trail - 0xdc00) + 0x10000;
}

module.exports = AO__UTF16SurrogatePairToCodePoint;
