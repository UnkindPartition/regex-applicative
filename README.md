regex-applicative
=================

*regex-applicative* is aimed to be an efficient and easy to use parsing combinator
library for Haskell based on regular expressions.

Perl programmers often use regular expressions for parsing, even if it is not
an appropriate tool for the job, because Perl has so good support for regexps.

The opposite seems to be valid about Haskell programmers -- they use parsing
combinators (which recognize context-free or even context-sensitive grammars),
even when the language is actually regular!

Hopefully, this library will improve the situation.

Installation
------------
Install this library using `cabal-install` tool:

    cabal update
    cabal install regex-applicative

Documentation
-------------
The [API reference][haddock] is available from Hackage.

To get started, see some [examples][examples] on the wiki.

Other resources
---------------

* [This package on Hackage][hackage]
* [Issue tracker][issues]
* [Repository][github]


[examples]: https://github.com/feuerbach/regex-applicative/wiki/Examples
[haddock]: http://hackage.haskell.org/packages/archive/regex-applicative/latest/doc/html/Text-Regex-Applicative.html
[hackage]: http://hackage.haskell.org/package/regex-applicative
[issues]: https://github.com/feuerbach/regex-applicative/issues
[github]: https://github.com/feuerbach/regex-applicative
