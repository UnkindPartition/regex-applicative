Changes
=======

0.3.4
-----

* Let the user provide a custom `uncons` function (add
    `find{First,Longest,Shortest}PrefixWithUncons`)
* Add `Filtrable` and `Monoid` instances for `RE`

0.3.3.1
-------

Make a release to refresh the haddocks on hackage
(see <https://github.com/feuerbach/regex-applicative/issues/35>).

0.3.3
-----

Add `replace`

0.3.2.1
-------

* Use strict left fold in decimal/hexadecimal
* Include a missing test module in the sdist tarball

0.3.2
-----

Add `msym`

0.3.1
-----

Add `comap`

0.3.0.3
-------

* Fix the test suite
* Fix build with GHC 7.9

0.3.0.2
-------

Fix the test suite

0.3.0.1
-------

Port the test suite to tasty

0.3
---
* Add a new module, `Text.Regex.Applicative.Common`, which contains some
  commonly used regexps (by Aleksey Khudyakov)
* Improve the test suite

0.2.1
-----
* Add the `withMatched` function
* Make matching functions a bit more lax
* Fix a bug in the `empty` method

0.2
---
* Infix matching functions
* Improved documentation
* Improved performance
* Improved portability

0.1.5
-----
* Expose Object interface
* Allow matching prefixes rather than the whole string
* Add non-greedy repetitions

0.1.4
-----
* Completely rewrite the engine. Now it's faster and runs in constant space.
* Add 'string' function and 'IsString' instance.

0.1.3
-----
* Fix a .cabal-file issue introduced in 0.1.2
* Change the fixity of =~

0.1.2
-----
* Relax the constraint on the containers version

0.1.1
---
* Fix a bug in 'reFoldl' and 'many'
* "Lazy" infinite regexes are no longer supported

0.1
---
* Initial release
