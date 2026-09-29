Object.freeze.call(undefined, (() => { const buffer = new ArrayBuffer(8); const value = new Uint32Array(buffer); buffer.transfer(); return value; })());
