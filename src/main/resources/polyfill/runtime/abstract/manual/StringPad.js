// Workaround of substring operation
function SubString(S, start, end) {
  return "".substring.call(S, start, end);
}

// Workaround of repeat n times / repeated concatenation operation
function RepeatedString(str, n, maxlen) {
  var result = "";
  for (; n > 0; (n >>>= 1) && (str += str)) if (n & 1) result += str;
  return SubString(result, 0, maxlen);
}

// https://tc39.es/ecma262/multipage/text-processing.html#sec-stringpad
function AO__StringPad(S, maxLength, fillString, placement) {
  // 1. Let stringLength be the length of S.
  var stringLength = S.length;
  // 2. If maxLength ≤ stringLength, return S.
  if (maxLength <= stringLength) return S;
  // 3. If fillString is the empty String, return S.
  if (fillString === "") return S;
  // 4. Let fillLen be maxLength - stringLength.
  var fillLen = maxLength - stringLength;
  // 5. Let truncatedStringFiller be the String value consisting of repeated concatenations of fillString truncated to length fillLen.
  var truncatedStringFiller = RepeatedString(
    fillString,
    Math.ceil(fillLen / fillString.length),
    fillLen,
  );
  // 6. If placement is start, return the string-concatenation of truncatedStringFiller and S.
  if (placement === "start") return truncatedStringFiller + S;
  // 7. Else, return the string-concatenation of S and truncatedStringFiller.
  else return S + truncatedStringFiller;
}

module.exports = AO__StringPad;
