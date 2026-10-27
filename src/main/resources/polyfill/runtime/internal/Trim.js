var lineTerminatorsStr = "\u000A\u000D\u2028\u2029";
var whiteSpacesStr =
  "\u0009\u000A\u000B\u000C\u000D\u0020\u00A0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u2028\u2029\u202F\u205F\u3000\uFEFF";
var whitespaces = lineTerminatorsStr + whiteSpacesStr;
function IsWhitespace(C) {
  for (var _i = 0; _i < whitespaces.length; _i++) {
    if (whitespaces[_i] === C) return true;
  }
  return false;
}

function RemoveLeadingWhitespace(S) {
  for (var _i = 0; _i < S.length; _i++) {
    if (!IsWhitespace(S[_i])) break;
  }
  return S.substring(_i);
}

function RemoveTrailingWhitespace(S) {
  for (var _i = S.length - 1; _i >= 0; _i--) {
    if (!IsWhitespace(S[_i])) break;
  }
  return S.substring(0, _i + 1);
}

function Trim(S, start, end) {
  if (start) S = RemoveLeadingWhitespace(S);
  if (end) S = RemoveTrailingWhitespace(S);
  return S;
}

module.exports = Trim;
