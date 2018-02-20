# stylish-cabal formatting conventions

To the best of my knowledge, there's no authoritative source for best practices regarding
formatting Cabal files. `stylish-cabal` tries to make formatting decisions that are easily
readable, easy to edit, and aesthetically pleasing.

## General formatting

Cabal files are organized into a PackageDescription of top-level declarations and then a
number of blocks describing package components (libraries, executables, etc.).

In general, the value halves of a list of key-value declarations should be vertically
aligned:

```
name:        stylish-cabal
version:     0.2.0.0
synopsis:    Format Cabal files
description: A tool for nicely formatting your Cabal file.
```

Attribute values are generally either scalars (i.e. a string) or lists. List values can be
space-separated, with or without commas, separated by one of commas or linebreaks, or
require commas regardless of whitespace.

`stylish-cabal` only specifies commas when required by the Cabal grammar[1]; i.e. for the
`build-depends` field.

[1] The `tested-with` field is the single exception; I personally find it more easily
scannable when the listed compiler versions are on as few lines as possible.

```
extra-source-files: ChangeLog.md
                    tests/example.cabal
                    "tests/some filename containing spaces.txt"
ghc-options:        -Wall -Werror -rtsopts -with-rtsopts=-N
tested-with:        GHC == 8.0.2, GHC == 8.2.2, GHC == 8.4.1
```

## Special fields

### Description

`description` is formatted specially depending on its length and the page width.

If the value of the `description` field, aligned with other values in the block, fits in
the page width, it's placed on the same line. Otherwise, a linebreak is inserted and the
description is wrapped to the page width over multiple lines, indented by $INDENT.

```
description:   A tool for nicely formatting your Cabal file.
```

vs.

```
description:
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam sagittis neque magna, eu
  accumsan velit convallis at. In at odio eu risus facilisis vehicula vel non elit.
  Vestibulum volutpat id tellus ac malesuada.
```

### Dependencies fields

`build-depends`, `tool-depends`, `pkgconfig-depends`, and `reexported-modules`
also require special formatting.

As with `description`, if the formatted contents of the field value fit on one line,
they're placed on that line with no special indentation. Otherwise they're broken out into
one line per list element, with leading commas "outdented" 2 spaces so that the name of
each list element aligns with the first.

```
build-depends:      base       == 4.*
                  , attoparsec
                  , bar        ^>= 3.4
                  , foo        >= 1.2.3 && < 1.4
                  , somelib    ^>= 1.2.3.4
reexported-modules: base:Numeric as MyNumericModule
                  , base:System.IO.Unsafe as SuperSafeIOModule
pkgconfig-depends:  cairo >= 1.0, gtk+-2.0 >= 2.10
```

Version constraints (where applicable) are aligned to the longest dependency name in the
list, regardless of whether it has a version constraint or not.

*Side note: the `as` clause in `reexported-modules` isn't aligned the same way version
constraints are. should it be?*

### Mixins

`mixins` is one of the most complex fields in a Cabal file and requires special
formatting.
