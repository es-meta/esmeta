var getFastKey = require("./getFastkey");

module.exports = function (mapData, key) {
  var fastKey = getFastKey(key);

  if (fastKey === "FROZEN") {
    var frozenList = mapData.frozen;
    for (var i = 0; i < frozenList.length; i++) {
      if (frozenList[i].Key === key) return true;
    }
    return false;
  }

  return !!mapData.index[fastKey];
};