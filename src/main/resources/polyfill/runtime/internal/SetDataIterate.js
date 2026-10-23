module.exports = function (setData, callback) {
  var thisSize = setData.length;
  var index = 0;
  while (index < thisSize) {
    var result = setData[index] === "empty" ? undefined : callback(setData[index]);
    if (result && result["Type"] === "early-return") {
      return result;
    }
    index = index + 1;
    thisSize = setData.length;
  }
}
