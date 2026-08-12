const AO__SameValue = require("../abstract/manual/SameValue");

module.exports = function (setData, value) {
  "use strict";
  for (var _x0 = 0; _x0 < setData.length; _x0++) {
    var e = setData[_x0];
    if (!(e === "empty") && AO__SameValue(e, value)) {
      return true;
    }
  }

  return false;
};
