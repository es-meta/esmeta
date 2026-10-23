module.exports = function (setData, value) {
  for (var i = 0; i < setData.length; i++) {
    if (value === setData[i]) {
      setData[i] = "empty";
      return;
    }
  }
}