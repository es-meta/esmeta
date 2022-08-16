var a = { a: 1, b: 2, c: 3 };

var sum = 0;
for (let i in a) {
    sum += a[i];
}
