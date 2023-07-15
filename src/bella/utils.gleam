@external(javascript, "../ffi.ts", "readFile")
pub fn read_file(a: String) -> Result(String, String)

@external(javascript, "../ffi.ts", "writeFile")
pub fn write_file(a: String, b: String) -> Result(String, String)

@external(javascript, "../ffi.ts", "createDirectory")
pub fn create_directory(a: String) -> Result(String, String)

@external(javascript, "../ffi.ts", "getArgs")
pub fn get_args() -> List(String)

@external(javascript, "../ffi.ts", "unescape")
pub fn unescape(a: String) -> String

@external(javascript, "../ffi.ts", "stringify")
pub fn stringify(a: String) -> String
