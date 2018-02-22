# stylish-cabal [![stylish-cabal](https://img.shields.io/hackage/v/stylish-cabal.svg)](https://hackage.haskell.org/package/stylish-cabal) [![stylish-cabal](https://travis-ci.org/pikajude/stylish-cabal.svg)](https://travis-ci.org/pikajude/stylish-cabal)

Automatic formatting for Cabal files.

## Examples

[example.cabal](tests/example.cabal) and [example-out.cabal](tests/example-out.cabal)
demonstrate what `stylish-cabal` does.

## Usage

```
# prints a highlighted and formatted version of myproject.cabal to stdout.
$ stylish-cabal myproject.cabal

# reformats myproject.cabal in-place.
$ stylish-cabal -i myproject.cabal

# pipe usage
$ cat myproject.cabal | stylish-cabal > myproject-formatted.cabal
```

Note that:

* `stylish-cabal` cannot preserve comments or unrecognized declarations.

* `stylish-cabal` cannot parse and will not produce a .cabal file using pre-1.2 flat
  (non-section) Cabal syntax.

* `stylish-cabal` will not attempt to format a Cabal file if warnings are emitted during
  parsing. Fix warnings before using it.
