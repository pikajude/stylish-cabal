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
formatting. If you're unfamiliar with mixins (as I was when I started this project), [this
blog post](http://blog.ezyang.com/2017/01/try-backpack-cabal-packages/) is a helpful
resource.

Each mixin has a name, an optional "provided renaming" list and an optional "required
renaming" list, of the form:

```
mixins: mixin-name (Module as Renamed.Module) requires (hiding)? (Module as Renamed.Module)
```

If the full mixins list fits on a single line, as seen above, it will be rendered that way
with no special indentation.

If it doesn't, the mixins list will expand as follows:

```
mixins: example-mixin2  (Mixin as Foo.Mixin2
                       , Mixina as Foo.Mixina2
                       , Mixinb as Foo.Mixinb2
                       , Mixinc as Foo.Mixinc2
                       )
      , example-mixin3 (Mixin as Bar.Mixin)
              requires (Mixin4, Mixin5)
      , example-mixin4 (Mixin as Qux.Mixin)
              requires hiding (Mixin6, Mixin7)
      , example-mixin5
      , last-mixin     (Mixin as Foo.Mixin) requires (Bar as Foo.Bar)
```

In short:

* Module renaming lists are formatted like tuples, with leading commas.
* If a *required renaming* list is given, the *provided renaming* and *required renaming*
  lists will be vertically aligned. (No extra spacing is added for "hiding" as it makes
  mixin lists too unwieldy.) If the mixin name is shorter than the word "requires", it's
  left-justified to match; otherwise "requires" will be right-justified.

The sharp-eyed reader notices the visually inconsistent formatting of the first renaming
group:

```
mixins: example-mixin2  (Mixin as Foo.Mixin2
                       , Mixina as Foo.Mixina2
```

...because Cabal's parser (incorrectly) can't parse this due to the leading space:

```
mixins: example-mixin2 ( Mixin as Foo.Mixin2
                       , Mixina as Foo.Mixina2
```

Since new Cabal versions require backwards-compatible parsing (even the bugs), this isn't
something that we can fix for the forseeable future.

## Field organization

Field ordering was chosen arbitrarily based on what "felt right", which IMO is the most
effective way to create formatting conventions :)

If `stylish-cabal` produces field orderings that are confusing for you to read or seem
counterintuitive, file a bug on the [Github issue tracker](https://github.com/pikajude/stylish-cabal/issues).

## Other misc. considerations

Starting with `cabal-version: 2.1` the Cabal spec requires that `cabal-version` be placed
at the top of the file. For previous versions, it will be placed at the bottom of the
initial list of top-level declarations.
