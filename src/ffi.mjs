import { Ok, Error, toList } from './gleam.mjs';

export function readFile(path) {
  try {
    return new Ok(Deno.readTextFileSync(path));
  } catch (e) {
    return new Error(e.message);
  }
}

export function writeFile(path, content) {
  try {
    Deno.writeTextFileSync(path, content);
    return new Ok(path);
  } catch (e) {
    return new Error(e.message);
  }
}

export function createDirectory(path) {
  try {
    Deno.mkdirSync(path, { recursive: true });
    return new Ok(path);
  } catch (e) {
    return new Error(e.message);
  }
}

export function getArgs() {
  return toList(Deno.args);
}
