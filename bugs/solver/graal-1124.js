Object.getPrototypeOf(Uint8Array).prototype.set.call((() => { const buffer = new ArrayBuffer(8); const value = new Float32Array(buffer); buffer.transfer(); return value; })(), Function, 2 ** 31);
