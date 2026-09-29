Object.getPrototypeOf(Uint8Array).prototype.set.call((() => { const buffer = new ArrayBuffer(8); const value = new Uint8ClampedArray(buffer); buffer.transfer(); return value; })(), NaN, -1);
