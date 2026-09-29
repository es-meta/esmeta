Iterator.from.call("", { [Symbol.iterator]: () => ({ get next() { throw 0; } }) });
