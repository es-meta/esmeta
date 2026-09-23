// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-toboolean
//
// Hand-written because the specification's step 3 is normative-optional and
// guarded by "if the host is a web browser or otherwise supports
// [[IsHTMLDDA]]", a condition the metalanguage cannot express. A non-browser
// host omits that step, which is exactly what `Boolean` already does.
function AO__ToBoolean(argument) {
  "use strict";

  return Boolean(argument);
}

module.exports = AO__ToBoolean;
