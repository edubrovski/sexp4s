# What is sexp4s 

This is a toy project for learning shapeless and fastparse.

S-expressions can be used as a serialization format (instead of JSON, for example),
but keep in mind that this was created purely for fun and learning. One should never use S-expressions in production for anything.

sexp4s provides:
- methods for parsing string input to AST and pretty printing ASTs
- typeclasses for converting Scala classes to S-expressions and vice versa
- automatic derivation of these typeclasses

S-expressions used by sexp4s are not really S-expressions, they are much simpler.
TODO: describe format

# TODO: document usage

