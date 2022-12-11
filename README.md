# Chipmunk2D

Raw bindings to the [Chipmunk2D](https://chipmunk-physics.net/) C library (version 7.0.3), specifically the most recent git version at
https://github.com/slembcke/Chipmunk2D/commit/7d10641155864bcf0e7f4c7cf1f0327ec7c1d90d
(includes Apple-specific fixes added right after 7.0.3).

This package reexports the
[Foreign.Storable.Offset](https://hackage.haskell.org/package/storable-offset-0.1.0.0/docs/Foreign-Storable-Offset.html)
module and implements the `Offset` instance for all the datatypes.
Alas Hackage currently does not show this (as per [haddock#563](https://github.com/haskell/haddock/issues/563)).

Caveats of this library:
- `DuplicateRecordFields` (since GHC 9.2 also `NoFieldSelectors`) are turned on in all modules with datatype
  definitions, record field names are the same as in the C library.
  You should use `GHC.Records.getField` or record dot syntax to access datatype fields;

- `data` and `type` record fields are replaced with `data_` and `type_` respectively.
  `GHC.Records.HasField` and `Foreign.Storable.Offset.Offset` instances are defined
  over both variants;

## Maintenance
Some bindings are bound to have errors in them, feel free to report them through Github.
