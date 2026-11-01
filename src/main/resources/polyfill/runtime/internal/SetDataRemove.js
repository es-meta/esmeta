var ENUM__empty = require("./Enum")("empty");
var AO__SameValue = require("../abstract/manual/SameValue");

module.exports = function (setData, value) {
  for (var i = 0; i < setData.length; i++) {
    if (AO__SameValue(value, setData[i])) {
      setData[i] = ENUM__empty;
      return;
    }
  }
}
