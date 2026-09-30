var $logs = [];
var $logState = {
  results: [],
  threw: false,
  error: undefined,
  active: true,
  stop() { $logState.active = false; }
};
var $L = (() => {
  const proxies = new WeakMap();
  const L = (target, id) => {
    if (target === null || (typeof target !== 'object' && typeof target !== 'function'))
      return target;
    if (proxies.has(target)) return proxies.get(target);
    const proxy = new Proxy(target, {
      get(...args) {
        if ($logState.active) $logs.push(id + ':get' + ':' + typeof args[1] + ':' + String(args[1]));
        return Reflect.get(...args);
      },
      set(...args) {
        if ($logState.active) $logs.push(id + ':set' + ':' + typeof args[1] + ':' + String(args[1]));
        return Reflect.set(...args);
      },
      has(...args) {
        if ($logState.active) $logs.push(id + ':has' + ':' + typeof args[1] + ':' + String(args[1]));
        return Reflect.has(...args);
      },
      deleteProperty(...args) {
        if ($logState.active) $logs.push(id + ':deleteProperty' + ':' + typeof args[1] + ':' + String(args[1]));
        return Reflect.deleteProperty(...args);
      },
      defineProperty(...args) {
        if ($logState.active) $logs.push(id + ':defineProperty' + ':' + typeof args[1] + ':' + String(args[1]));
        return Reflect.defineProperty(...args);
      },
      getOwnPropertyDescriptor(...args) {
        if ($logState.active) $logs.push(id + ':getOwnPropertyDescriptor' + ':' + typeof args[1] + ':' + String(args[1]));
        return Reflect.getOwnPropertyDescriptor(...args);
      },
      ownKeys(...args) {
        if ($logState.active) $logs.push(id + ':ownKeys');
        return Reflect.ownKeys(...args);
      },
      getPrototypeOf(...args) {
        if ($logState.active) $logs.push(id + ':getPrototypeOf');
        return Reflect.getPrototypeOf(...args);
      },
      setPrototypeOf(...args) {
        if ($logState.active) $logs.push(id + ':setPrototypeOf');
        return Reflect.setPrototypeOf(...args);
      },
      isExtensible(...args) {
        if ($logState.active) $logs.push(id + ':isExtensible');
        return Reflect.isExtensible(...args);
      },
      preventExtensions(...args) {
        if ($logState.active) $logs.push(id + ':preventExtensions');
        return Reflect.preventExtensions(...args);
      },
      apply(...args) {
        if ($logState.active) $logs.push(id + ':apply');
        return Reflect.apply(...args);
      },
      construct(...args) {
        if ($logState.active) $logs.push(id + ':construct');
        return Reflect.construct(...args);
      }
    });
    proxies.set(target, proxy);
    proxies.set(proxy, proxy);
    return proxy;
  };
  return L;
})();
