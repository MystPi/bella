pub external fn read_file(String) -> Result(String, String) =
  "../ffi.ts" "readFile"

pub external fn write_file(String, String) -> Result(String, String) =
  "../ffi.ts" "writeFile"

pub external fn create_directory(String) -> Result(String, String) =
  "../ffi.ts" "createDirectory"

pub external fn get_args() -> List(String) =
  "../ffi.ts" "getArgs"

pub external fn unescape(String) -> String =
  "../ffi.ts" "unescape"

pub external fn stringify(String) -> String =
  "../ffi.ts" "stringify"
