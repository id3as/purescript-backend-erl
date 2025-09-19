import zlib from "node:zlib";
import v8 from "node:v8";
import fs from "node:fs/promises";
import { subtle } from "node:crypto";
import { promisify } from "node:util";

export const _saveDataToFile = (filename, data, success, error) => {
  Promise.resolve()
    .then(() => v8.serialize(data))
    .catch(error)
    .then(bytes => promisify(zlib.gzip)(bytes, { level: 2 }))
    .catch(error)
    .then(buf => fs.writeFile(filename, buf))
    .then(success, error);
};
export const _loadDataFromFile = (filename, success, error) => {
  fs.readFile(filename)
    .catch(error)
    .then(buf => promisify(zlib.gunzip)(buf))
    .catch(error)
    .then(bytes => v8.deserialize(bytes))
    .then(success, error);
};
export const _hashSHA512 = (data, success, error) => {
  subtle.digest("SHA-512", new TextEncoder().encode(data))
    .then(digest => Array.from(new Uint8Array(digest), d => d.toString(16).padStart(2, '0')).join(''))
    .then(success, error);
};
