import gleam/dynamic
import gleam/json

pub type Project {
  Project(name: String, version: String)
}

pub fn init(project_name: String) -> String {
  json.object([
    #("name", json.string(project_name)),
    #("version", json.string("0.1.0")),
  ])
  |> json.to_string
}

pub fn decode(json: String) -> Result(Project, json.DecodeError) {
  let project_decoder =
    dynamic.decode2(
      Project,
      dynamic.field("name", of: dynamic.string),
      dynamic.field("version", of: dynamic.string),
    )

  json.decode(json, project_decoder)
}
