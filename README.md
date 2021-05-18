# derive_enum_methods

## derive_enum_methods

This is a fork of [derive_is_enum_variant](https://github.com/fitzgen/derive_is_enum_variant) by @fitzgen that can additionally derive `as_*` and `unsafe as_*_unchecked` methods.
### `derive_enum_methods`


#### Usage

Add `derive_enum_methods` to your crate's `Cargo.toml`:

```toml
[dependencies]
derive_enum_methods = { git = "https://github.com/ves-lang/derive_enum_methods" }
```

And then add `#[derive(is_enum_variant, as_enum_variant, enum_variant_unchecked)]` to your `enum` definitions:

```rust
#[macro_use]
extern crate derive_enum_methods;

struct Doggo;
struct Kitten;

// This derive ...
#[derive(is_enum_variant, as_enum_variant, enum_variant_unchecked)]
pub enum Pet {
    Doggo(Doggo),
    Kitten(Kitten),
}

// will generate the code below:
impl Pet {
    fn is_doggo(&self) -> bool {
        matches!(self, Self::Doggo { .. })
    }
    fn is_kitten(&self) -> bool {
        matches!(self, Self::Kitten { .. })
    }
}
impl Pet {
    fn as_doggo(&self) -> Option<&Doggo> {
        if Self::Doggo(__self_0) = self {
            Some(__self_0)
        } else {
            None
        }
    }
    fn as_kitten(&self) -> Option<&Kitten> {
        if Self::Kitten(__self_0) = self {
            Some(__self_0)
        } else {
            None
        }
    }
}
impl Pet {
    unsafe fn as_doggo_unchecked(&self) -> &Doggo {
        match self {
            Self::Doggo(__self_0) => __self_0,
            _ => {
                if cfg!(debug_assertions) {
                    unreachable!()
                } else {
                    core::hint::unreachable_unchecked()
                }
            }
        }
    }
    unsafe fn as_kitten_unchecked(&self) -> &Kitten {
        match self {
            Self::Kitten(__self_0) => __self_0,
            _ => {
                if cfg!(debug_assertions) {
                    unreachable!()
                } else {
                    core::hint::unreachable_unchecked()
                }
            }
        }
    }
}
```

##### Customizing Predicate Names

By default, the predicates are named `is_snake_case_of_variant_name`. You can
use any name you want instead with `#[is_enum_variant(name = "..")]` (same for the other two attributes):

```rust
use derive_enum_methods::is_enum_variant;

#[derive(is_enum_variant)]
pub enum Pet {
    #[is_enum_variant(name = "isDoggo")]
    Doggo,
    Kitten,
}

let pet = Pet::Doggo;
assert!(pet.isDoggo());
```

##### Skipping Predicates for Certain Variants

If you don't want to generate a predicate for a certain variant, you can use
`#[is_enum_variant(skip)]` (same for the other two attributes):

```rust
#[derive(is_enum_variant)]
pub enum Errors {
    Io(::std::io::Error),

    #[doc(hidden)]
    #[is_enum_variant(skip)]
    __NonExhaustive,
}

```

#### License

Licensed under either of

  * Apache License, Version 2.0 ([`LICENSE-APACHE`](./LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
  * MIT license ([`LICENSE-MIT`](./LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

#### Contribution

See [CONTRIBUTING.md](./CONTRIBUTING.md) for hacking!

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.


License: Apache-2.0/MIT
