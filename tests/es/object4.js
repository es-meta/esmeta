var obj = { "-0": 42, "0": 42, "1": 42, "x": 44 };
var keys = Reflect.ownKeys(obj);
print(keys);
print(keys.length);
print(keys[0]);
print(keys[1]);
print(keys[2]);
print(keys[3]);
