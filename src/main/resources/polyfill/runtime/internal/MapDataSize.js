var ENUM__empty = require("./Enum")("empty");
module.exports = function (mapData) {
  var count = 0;
  for (var i = 0; i < mapData.length; i++) {
    if (mapData[i]["Key"] !== ENUM__empty) {
      count = count + 1;
    }
  }
  return count;
}
