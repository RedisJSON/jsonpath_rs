[![license](https://img.shields.io/github/license/RedisJSON/jsonpath_rs.svg)](https://github.com/RedisJSON/jsonpath_rs/blob/master/LICENSE)
[![GitHub issues](https://img.shields.io/github/release/RedisJSON/jsonpath_rs.svg)](https://github.com/RedisJSON/jsonpath_rs/releases/latest)
[![Rust](https://github.com/RedisJSON/jsonpath_rs/actions/workflows/rust.yml/badge.svg)](https://github.com/RedisJSON/jsonpath_rs/actions/workflows/rust.yml)
[![crates.io](https://img.shields.io/crates/v/jsonpath_rs.svg)](https://crates.io/crates/jsonpath_rs)

# jsonpath_rs
[![Forum](https://img.shields.io/badge/Forum-RedisJSON-blue)](https://forum.redislabs.com/c/modules/redisjson)
[![Discord](https://img.shields.io/discord/697882427875393627?style=flat-square)](https://discord.gg/QUkjSsk)

A JSONPath library for rust. The idea behind this library is that it can operate on any json representation as long as it implements the [`SelectValue`](src/select_value.rs) triat. The library has an implementation for [serde_json value](https://docs.serde.rs/serde_json/value/enum.Value.html) and [ivalue](https://docs.rs/tch/0.1.1/tch/enum.IValue.html).

### Getting Started
Add the following to your cargo.toml

```rust
[dependencies]
jsonpath_rs = { git = "https://github.com/RedisJSON/jsonpath_rs.git", branch = "master" }
```

Usage example:

```rust
extern crate jsonpath_rs
#[macro_use] extern crate serde_json;

fn main() {
    let mut query = jsonpath_rs::compile("$..friends[0]");
    let path = jsonpath_rs::create(&query)

    let json_obj = json!({
        "school": {
            "friends": [
                {"name": "foo1", "age": 20},
                {"name": "foo2", "age": 20}
            ]
        },
        "friends": [
            {"name": "foo3", "age": 30},
            {"name": "foo4"}
        ]
    });

    let json = path.calc(&json_obj);

    assert_eq!(json, vec![
        &json!({"name": "foo3", "age": 30}),
        &json!({"name": "foo1", "age": 20})
    ]);
}
```

### Tests
`jsonpath_rs` pass **Almost** all the tests on https://github.com/freestrings/jsonpath, to run the tests:

```rust
cargo test
```
