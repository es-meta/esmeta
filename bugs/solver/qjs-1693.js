Iterator.from.call(-1n, { [Symbol.iterator]: () => ({ get next() { throw 0; } }) });
