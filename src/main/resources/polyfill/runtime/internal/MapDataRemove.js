var ENUM__empty = require("./Enum")("empty");
module.exports = function (mapData, key) {
  for (var i = 0; i < mapData.length; i++) {
    if (key === mapData[i]["Key"]) {
      mapData[i]["Key"] = ENUM__empty;
      mapData[i]["Value"] = ENUM__empty;
      return;
    }
  }
}