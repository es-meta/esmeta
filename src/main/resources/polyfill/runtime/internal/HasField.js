// Whether a record or an object has the field or internal slot. Fields and
// slots are own properties, so an object inheriting from one (e.g.,
// `Object.create(new Map())`) does not have them.
var hasOwn = Object.prototype.hasOwnProperty;

function IN__HasField(O, field) {
  return hasOwn.call(O, field);
}

module.exports = IN__HasField;
