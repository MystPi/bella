import { readFileSync, writeFileSync, mkdirSync, existsSync } from 'node:fs';
import { Ok, Error, toList } from './gleam.mjs';

export function readFile(path) {
  try {
    return new Ok(readFileSync(path, 'utf-8'));
  } catch (e) {
    return new Error(e.message);
  }
}

export function writeFile(path, content) {
  try {
    writeFileSync(path, content);
    return new Ok(path);
  } catch (e) {
    return new Error(e.message);
  }
}

export function createDirectory(path) {
  try {
    mkdirSync(path, { recursive: true });
    return new Ok(path);
  } catch (e) {
    return new Error(e.message);
  }
}

export function fileExists(path) {
  return existsSync(path);
}

export function getArgs() {
  return toList(globalThis.Deno?.args ?? process.argv.slice(2));
}
