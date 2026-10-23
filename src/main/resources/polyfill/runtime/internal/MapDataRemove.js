module.exports = function (mapData, key) {
  for (var i = 0; i < mapData.length; i++) {
    if (key === mapData[i]["Key"]) {
      mapData[i]["Key"] = "empty";
      mapData[i]["Value"] = "empty";
      return;
    }
  }
}