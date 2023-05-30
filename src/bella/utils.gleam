pub external fn read_file(String) -> Result(String, String) =
  "../ffi.mjs" "readFile"

pub external fn write_file(String, String) -> Result(String, String) =
  "../ffi.mjs" "writeFile"

pub external fn create_directory(String) -> Result(String, String) =
  "../ffi.mjs" "createDirectory"

pub external fn get_args() -> List(String) =
  "../ffi.mjs" "getArgs"
