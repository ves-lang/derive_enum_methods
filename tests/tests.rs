// This lets us ensure that the generated methods get doc comments.
#![deny(missing_docs)]
#![deny(unreachable_patterns)]

/// Tests for `#[derive(is_enum_variant)]`.

#[macro_use]
extern crate derive_enum_methods;

use std::env;
use std::fs::File;
use std::io::Read;
use std::process::Command;

#[test]
fn cargo_readme_up_to_date() {
    /// A wrapper that exposes the given object's [`Display`] impl as [`Debug`].
    #[derive(Clone, PartialEq)]
    pub struct DisplayAsDebugWrapper<T>(T);

    impl<T> std::fmt::Debug for DisplayAsDebugWrapper<T>
    where
        T: std::fmt::Display,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl<T> std::ops::Deref for DisplayAsDebugWrapper<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    if env::var("CI").is_ok() {
        return;
    }

    let expected = Command::new("cargo")
        .arg("readme")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .expect("should run `cargo readme` OK")
        .stdout;
    let expected = String::from_utf8_lossy(&expected);

    let actual = {
        let mut file = File::open(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))
            .expect("should open README.md file");
        let mut s = String::new();
        file.read_to_string(&mut s)
            .expect("should read contents of file to string");
        s
    };

    if actual != expected {
        println!();
        println!("+++ expected README.md");
        println!("--- actual README.md");
        pretty_assertions::assert_eq!(
            DisplayAsDebugWrapper(&*expected),
            DisplayAsDebugWrapper(&*actual),
            "Run `cargo readme > README.md` to update README.md"
        );
    }
}

/// A kind of pet.
#[derive(is_enum_variant)]
pub enum Pet {
    /// A dog.
    Doggo,
    /// A cat.
    Kitteh,
    /// A flying squirrel.
    FlyingSquirrel,
}

#[test]
fn basic_enum_predicates() {
    let doggo = Pet::Doggo;
    assert!(doggo.is_doggo());
    assert!(!doggo.is_kitteh());
    assert!(!doggo.is_flying_squirrel());

    let kitteh = Pet::Kitteh;
    assert!(!kitteh.is_doggo());
    assert!(kitteh.is_kitteh());
    assert!(!kitteh.is_flying_squirrel());

    let squirrel = Pet::FlyingSquirrel;
    assert!(!squirrel.is_doggo());
    assert!(!squirrel.is_kitteh());
    assert!(squirrel.is_flying_squirrel());
}

/// Different kinds of enum variants.
#[derive(is_enum_variant, as_enum_variant, enum_variant_unchecked)]
pub enum VariantKinds {
    Single(i32),
    Struct { x: usize, y: usize },
    Tuple(usize, usize),
    Unit,
}

#[test]
fn variant_kinds() {
    assert!(VariantKinds::Single(5).is_single());
    assert!(VariantKinds::Struct { x: 1, y: 2 }.is_struct());
    assert!(VariantKinds::Tuple(1, 2).is_tuple());
    assert!(VariantKinds::Unit.is_unit());

    assert_eq!(
        VariantKinds::Struct { x: 1, y: 2 }.as_struct(),
        Some((&1, &2))
    );
    assert_eq!(VariantKinds::Tuple(1, 2).as_tuple(), Some((&1, &2)));
    assert_eq!(VariantKinds::Single(2).as_single(), Some(&2i32));

    unsafe {
        assert_eq!(
            VariantKinds::Struct { x: 1, y: 2 }.as_struct_unchecked(),
            (&1, &2)
        );
        assert_eq!(VariantKinds::Tuple(1, 2).as_tuple_unchecked(), (&1, &2));
        assert_eq!(VariantKinds::Single(2).as_single_unchecked(), &2i32);
    }
}

#[should_panic]
#[test]
fn panics_on_invalid_kind() {
    unsafe {
        VariantKinds::Unit.as_struct_unchecked();
    }
}

/// Various funky case names.
#[allow(non_camel_case_types)]
#[derive(is_enum_variant, as_enum_variant)]
pub enum Funky {
    /// doc
    CAPS,
    /// doc
    SHOUTING_SNAKE,
    /// doc
    snake_case,
    /// doc
    littleCamel,
    /// doc
    WithACRONYM,
}

#[test]
fn funky_variant_names() {
    assert!(Funky::CAPS.is_caps());
    assert!(Funky::SHOUTING_SNAKE.is_shouting_snake());
    assert!(Funky::snake_case.is_snake_case());
    assert!(Funky::littleCamel.is_little_camel());
    assert!(Funky::WithACRONYM.is_with_acronym());
}

/// Test providing custom predicate names.
#[derive(is_enum_variant, as_enum_variant)]
pub enum CustomNames {
    #[is_enum_variant(name = "i_dont_know_why_you_say")]
    #[as_enum_variant(name = "i_dont_know_why_you_say_val")]
    Goodbye(()),
    #[is_enum_variant(name = "i_say")]
    #[as_enum_variant(name = "i_say_val")]
    Hello(()),
}

#[test]
fn custom_predicate_names() {
    assert!(CustomNames::Goodbye(()).i_dont_know_why_you_say());
    assert!(CustomNames::Hello(()).i_say());
    assert_eq!(
        CustomNames::Goodbye(()).i_dont_know_why_you_say_val(),
        Some(&())
    );
    assert_eq!(CustomNames::Hello(()).i_say_val(), Some(&()));
}

/// This doesn't get a predicate for every variant
#[derive(is_enum_variant)]
pub enum Skip {
    #[is_enum_variant(skip)]
    NoPredicate,
    YesPredicate,
}

#[test]
fn skip_variants() {
    assert!(Skip::YesPredicate.is_yes_predicate());
}

/// Because there is only one variant, the generated match's `_` pattern is
/// unreachable. This better not create a compilation error due to our
/// `deny(unreachable_patterns)`.
#[derive(is_enum_variant)]
pub enum GeneratedCodeHasNoWarnings {
    OnlyOneVariant,
}
