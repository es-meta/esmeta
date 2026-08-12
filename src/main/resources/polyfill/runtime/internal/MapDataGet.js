const AO__SameValue = require("../abstract/manual/SameValue");

module.exports = function (mapData, key) {
  for (var i = 0; i < mapData.length; i++) {
    if (AO__SameValue(key, mapData[i]["Key"])) {
      return mapData[i]["Value"];
    }
  }
}