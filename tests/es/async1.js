var x = 0;
var z = 1;
async function f() { return 2; }
f().then((y) => { x += y;} );
z = x;
