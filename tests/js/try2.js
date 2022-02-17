var x = true;
var y = false;
try {
  a;
  x = false;
} catch(e) {
  y = e instanceof ReferenceError;
}
