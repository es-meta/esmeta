Object.getPrototypeOf(Uint8Array).prototype.set.call((() => { const buffer = new ArrayBuffer(8); const value = new Float64Array(buffer); buffer.transfer(); return value; })(), true, -1);
