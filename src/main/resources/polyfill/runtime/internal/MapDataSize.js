module.exports = function (mapData) {
  var count = 0;
  for (var i = 0; i < mapData.length; i++) {
    if (mapData[i]["Key"] !== "empty") {
      count = count + 1;
    }
  }
  return count;
}
