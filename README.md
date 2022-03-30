# JsonPathCalculator
Json path calculator library for rust. The idea behind this library is that it can operate on any json representation as long as it implements the [`SelectValue`](src/select_value.rs) triat. The library has an implementation for [serde_json value](https://docs.serde.rs/serde_json/value/enum.Value.html) and [ivalue](https://docs.rs/tch/0.1.1/tch/enum.IValue.html).

### Getting Started
Add the following to your cargo.toml

```rust
[dependencies]
jsonpath_calculator = { git = "https://github.com/RedisJSON/JsonPathCalculator.git", branch = "master" }
```

Usage example:

```rust
extern crate jsonpath_calculator
#[macro_use] extern crate serde_json;

fn main() {
    let mut query = jsonpath_calculator::compile("$..friends[0]");
    let calculator = jsonpath_calculator::create(&query)

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

    let json = calculator.calc(&json_obj);

    assert_eq!(json, vec![
        &json!({"name": "foo3", "age": 30}),
        &json!({"name": "foo1", "age": 20})
    ]);
}
```

### Tests
`jsonpath_calculator` pass **Almost** all the tests on https://github.com/freestrings/jsonpath, to run the tests:

```rust
cargo test
```
