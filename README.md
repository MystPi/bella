# Bella programming language

![Bella language logo](assets/Bella-language.png)

Bella is a simple functional programming language, written in [Gleam](https://gleam.run). Still very much a work-in-progress. 🚧

## Using Bella

You will need:

- Gleam
- Node.js/Deno _(This is Gleam's target runtime)_
- This repo

Then you can try out the [examples](/examples/):

```sh
# Using Node.js
gleam run ./examples/hello_world.bella

# Using Deno
gleam run --runtime deno ./examples/hello_world.bella
```

Creating a Bella executable requires Deno.
```sh
# Build
gleam clean && gleam build

# Compile
deno compile --unstable --allow-all ./build/dev/javascript/bella/main.mjs

# Run
./bella ./examples/countdown.bella
```