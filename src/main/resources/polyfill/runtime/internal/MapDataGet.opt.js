var getFastKey = require("./getFastkey");

module.exports = function (mapData, key) {
  var fastKey = getFastKey(key);

  if (fastKey === "FROZEN") {
    var frozenList = mapData.frozen;
    for (var i = 0; i < frozenList.length; i++) {
      if (frozenList[i].Key === key) return frozenList[i].Value;
    }
    return undefined;
  }

  var node = mapData.index[fastKey];
  if (node) return node.Value;
  
  return undefined;
};