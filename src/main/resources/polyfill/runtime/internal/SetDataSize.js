module.exports = function (setData) {
  "use strict";
  var count = 0;
  for (var _x0 = 0; _x0 < setData.length; _x0++) {
    var e = setData[_x0];
    if (!(e === "empty")) {
      count = count + 1;
    }
  }

  return count;
};
