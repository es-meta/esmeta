var AO__Get = require("./Get");
var AO__ToString = require("./ToString");

// https://tc39.es/ecma262/multipage/text-processing.html#sec-getsubstitution
//
// ESMeta cannot compile this operation because the specification writes its
// dispatch as "_templateRemainder_ starts with *"$$"*", a phrasing the spec
// language parser does not yet cover.
function AO__GetSubstitution(
  matched,
  str,
  position,
  captures,
  namedCaptures,
  replacementTemplate,
) {
  // 1. Let _stringLength_ be the length of _str_.
  var stringLength = str.length;
  // 3. Let _result_ be the empty String.
  var result = "";
  // 4. Let _templateRemainder_ be _replacementTemplate_.
  var templateRemainder = replacementTemplate;
  // 5. Repeat, while _templateRemainder_ is not the empty String,
  while (templateRemainder !== "") {
    var ref;
    var refReplacement;
    if (startsWith(templateRemainder, "$$")) {
      ref = "$$";
      refReplacement = "$";
    } else if (startsWith(templateRemainder, "$`")) {
      ref = "$`";
      refReplacement = str.slice(0, position);
    } else if (startsWith(templateRemainder, "$&")) {
      ref = "$&";
      refReplacement = matched;
    } else if (startsWith(templateRemainder, "$'")) {
      ref = "$'";
      var tailPos = position + matched.length;
      refReplacement = str.slice(tailPos < stringLength ? tailPos : stringLength);
    } else if ((ref = matchCaptureIndex(templateRemainder, captures)) !== null) {
      // _ref_ is "$" followed by one or two decimal digits naming a capture.
      var index = Number(ref.slice(1));
      var capture = captures[index - 1];
      refReplacement = capture === undefined ? "" : capture;
    } else if (startsWith(templateRemainder, "$<")) {
      var gtPos = templateRemainder.indexOf(">");
      if (namedCaptures === undefined || gtPos === -1) {
        ref = "$<";
        refReplacement = "$<";
      } else {
        ref = templateRemainder.slice(0, gtPos + 1);
        var groupName = templateRemainder.slice(2, gtPos);
        var capture = AO__Get(namedCaptures, groupName);
        refReplacement = capture === undefined ? "" : AO__ToString(capture);
      }
    } else {
      // No substitution applies, so the next code unit is copied verbatim.
      ref = templateRemainder.slice(0, 1);
      refReplacement = ref;
    }
    // 5.n. Append _refReplacement_ and drop _ref_ from the remainder.
    result = result + refReplacement;
    templateRemainder = templateRemainder.slice(ref.length);
  }
  // 6. Return _result_.
  return result;
}

function startsWith(str, prefix) {
  return str.slice(0, prefix.length) === prefix;
}

// "$" followed by the longest run of one or two decimal digits that names an
// existing capture; null when the template does not start with such a run.
function matchCaptureIndex(templateRemainder, captures) {
  if (templateRemainder.slice(0, 1) !== "$") return null;
  var digits = "";
  var i = 1;
  while (i < templateRemainder.length && digits.length < 2) {
    var ch = templateRemainder.charAt(i);
    if (ch < "0" || ch > "9") break;
    digits = digits + ch;
    i = i + 1;
  }
  while (digits.length > 0) {
    var index = Number(digits);
    if (index >= 1 && index <= captures.length) return "$" + digits;
    digits = digits.slice(0, digits.length - 1);
  }
  return null;
}

module.exports = AO__GetSubstitution;
