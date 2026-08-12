var getFastKey = require("./getFastkey");

module.exports = function (setData, value) {
  var key = getFastKey(value);

  if (key === "FROZEN") {
    var frozenList = setData.frozen;
    for (var i = 0; i < frozenList.length; i++) {
      if (frozenList[i] === value) return true;
    }
    return false;
  }

  return !!setData.index[key];
};
