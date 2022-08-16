var m = new Map([[1,2], [2,3]])
var ksum = 0, vsum = 0;
for (let x of m) {
  let [k, v] = x;
  ksum += k;
  vsum += v;
}
