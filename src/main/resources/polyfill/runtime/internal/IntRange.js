function IntRange(
  from,
  isFromInclusive,
  to,
  isToInclusive,
  isAscending,
) {
  var range = [];
  if (isAscending) {
    var start = isFromInclusive ? from : from + 1;
    var end = isToInclusive ? to : to - 1;
    for (var i = start; i <= end; i++) {
      range.push(i);
    }
  } else {
    var start = isToInclusive ? to : to - 1;
    var end = isFromInclusive ? from : from + 1;
    for (var i = start; i >= end; i--) {
      range.push(i);
    }
  }
  return range;
}

module.exports = IntRange;