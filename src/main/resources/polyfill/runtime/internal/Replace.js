var AO__SameValue = require("../abstract/manual/SameValue");

// Replaces the first element of `base` that is SameValue to `oldElem`
// (`indexOf` uses `===`, which never finds NaN).
function OP__Replace(base, oldElem, newElem) {
  for (var i = 0; i < base.length; i++) {
    if (AO__SameValue(base[i], oldElem)) {
      base[i] = newElem;
      break;
    }
  }
  return base;
}

module.exports = OP__Replace;
