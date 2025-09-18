import zlib from "node:zlib";
import v8 from "node:v8";
import fs from "node:fs";
import { createHash } from "node:crypto";

export const saveDataToFile = filename => data => () => {
  fs.writeFileSync(filename, zlib.gzipSync(v8.serialize(data), { level: 2 }));
};
export const loadDataFromFile = filename => () => {
  return v8.deserialize(zlib.gunzipSync(fs.readFileSync(filename)));
};
export const hashSHA512 = data => {
  const hasher = createHash('sha512');
  hasher.update(new TextEncoder().encode(data));
  return hasher.digest('hex');
};
