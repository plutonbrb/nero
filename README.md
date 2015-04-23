# Nero

> A [`Lens`][lens]-based HTTP toolkit.

:warning: The following is a declaration of intentions.  Expect wild
changes in the `API` until the `1.0.0` release.

* **Not a framework**: it may be considered an *anti-framework*,
  *micro-framework*, or just a "library", in the sense that it provides a
  set of utilities for building *custom* web applications instead of
  creating applications from user provided code following certain
  structure.

  <!-- In reality this is more a distinction in intention than in actual
  code-->

* **Pay for what you eat**: instead of coming with *everything and the
  kitchen sink*, it provides the bare minimum to write applications
  with minimum implicit behavior. At the same time, it offers diverse
  paths to *grow with you* as applications become more complex.

  <!-- No monad transformers until they are needed.-->

* **Unopinonated**: there is no preferred routing method, HTML templating
  library, session management, web server or database adapter. It comes with
  some defaults to alleviate the [paradox of
  choice](https://en.wikipedia.org/wiki/The_Paradox_of_Choice), but most
  components are expected to be easily swapped in and out either with
  plain 3rd party [Haskell] libraries or by writing thin adapters around
  them.

  <!-- Is pluggable right here? Sounds out of fashion -->

* **Power of [Haskell] and [`Lens`][lens]**: the [`Lens`][lens]-based API
  enables a style familiar to imperative programmers [`Lens`][lens] while
  being purely functional under the hood. Veteran *Haskellers* can take
  advantage of the powerful lens combinators.

[![Hackage Version](https://img.shields.io/hackage/v/nero.svg)](https://hackage.haskell.org/package/nero) [![Build Status](https://img.shields.io/travis/plutonbrb/nero.svg)](https://travis-ci.org/plutonbrb/nero)

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Nero.Prelude
import Nero (Request, Response, _GET, prefixed, ok)
import Nero.Warp (serve) -- from `nero-warp`

app :: Request -> Maybe Response
app request = request ^? _GET . prefixed "/hello/" <&> \name ->
    ok $ "<h1>Hello " <> name <> "</h1>"

main :: IO ()
main = serve 8080 app
```

You can check more examples at https://github.com/plutonbrb/nero-examples.

[Haskell]: https://www.haskell.org/
[lens]: https://lens.github.io/
