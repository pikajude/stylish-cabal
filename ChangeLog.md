# Revision history for stylish-cabal

## 0.1.0.0  -- 2018-02-19

* Initial release.

## 0.2.0.0 -- 2018-02-20

0.1.0.0 tarball was broken due to a missing test file.

* Better haddocks for StylishCabal module so it can be imported and used independently of
  the CLI tool (like hindent, hlint, and so on).
* Better readme

## 0.2.1.0 -- 2018-02-27

* Removed support for GHC 8.4. Using a Cabal version older than the built-in version is
  officially not recommended.
* Certain version range expressions no longer incorrectly collapsed to ">="
* `stylish-cabal` executable now checks the correct Handle when determining if it should colorize (previously always checked stdout)
* Code block formatting in descriptions is now preserved (and will not be soft-broken to fit the width limit.)
* haskell-suite in the "tested-with" field can now be rendered
* Roundtrip test now checks an entire `GenericPackageDescription`, rather than just the `packageDescription` field

## 0.3.0.0 -- 2018-03-02

* Description rendering has been rewritten. Rather than naive reformatting, it now uses
  the proper Haddock parser.
* Show1/Eq1 instances for Result

## 0.4.0.0 -- 2018-03-08

* Cabal 2.2 and GHC 8.4 support
