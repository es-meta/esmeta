var callCount = 0;
// Stores a reference `ref` for case evaluation
async function* ref(a, b,) {
  callCount = callCount + 1;
}

ref(42, 39, 1).next().then(() => {
}).then(() => {});

