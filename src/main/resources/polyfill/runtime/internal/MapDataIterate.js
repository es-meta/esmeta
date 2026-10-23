module.exports = function (mapData, callback) {
  var numEntries = mapData.length;
  var index = 0;
  while (index < numEntries) {
    if (mapData[index]["Key"] !== "empty") callback(mapData[index]);
    index = index + 1;
    numEntries = mapData.length;
  }
}
