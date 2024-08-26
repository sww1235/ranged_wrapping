# Arbitrary Wrapping

Arbitrary Wrapping is an extension of the Wrapping type in std rust. It takes in two additional parameters that allows specifying the maximum and minimum wrapping range of the type.

It does not fully support all operations yet, but the goal would be to support all operations supported by Wrapping.

Most code is directly copied out of the [main rust repo](https://github.com/rust-lang/rust/blob/master/library/core/src/num/wrapping.rs) and then modified to accomplish the goal of this library.
