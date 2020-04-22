# Claire

Claire is a very experimental implementation of a Clojure-like programming language in [Rust](https://www.rust-lang.org/).

## Motivation

I love [Clojure](https://clojure.org/) for its simplicity, expressivity and pragmatic approach to immutability. However I dislike having to install heavy tools such as the [JVM](https://en.wikipedia.org/wiki/Java_virtual_machine) or managing [Leiningen](https://leiningen.org/) configurations.

I want to be able to spread Clojure magic everywhere 🌟 and not have to worry about installing the tools.

Rust already gives me this portability and I would love to be able to use both languages together easily. So I decided to start working on a Clojure-like implementation in Rust.

This project is also an opportunity for me to learn how to build a compiler or interpreter. As such it is very experimental and I don't know how it's gonna turn out. 🔮

## Build status

Currently no garantees, except that it should compile. 

Look at the [Test](#test) and [How to use](#how-to-use) sections for instructions. 📝

## Code style

[Rustfmt](https://github.com/rust-lang/rustfmt), without any custom configuration, is run over the code before every commit.

## Screenshot

No screenshot, yet ;)

## Test

In order to run the tests, go to the project root directory and run the Rust defacto build tool, Cargo, with its test flag:

```bash
    cd path/to/the/project/claire
    cargo test
```

## How to use

Currently there is not much to use. But you can still run the code by getting into the project root directory and running the build tool:

```bash
    cd path/to/the/project/claire
    cargo run
```

## Contribute

I am not actively looking for contributors yet, but if you want to help or ask a question don't hesitate to open an issue and come say hi. I'll appreciate it for sure :)

## Credits

Thanks to everyone who made Rust such a great programming language to work with 🎉 and to the Clojure community for making me aware of the exciting things Clojure has to offer! 🎇

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.
Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in Claire by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
