# Nero

> An experimental Haskell toolkit for [`Lens`][lens-home]-based web
> application development.

:warning: The following is for now a declaration of intentions only.
Expect wild changes in the `API` in the near future.

* **Not a framework**: it could be considered an *anti-framework*,
  *micro-framework*, or just a "library", in the sense that it provides a
  set of utilities to build *custom* web applications instead of being a
  *framework* that creates web applications from user provided code.

  <!-- In reality this is more a distinction in intention than in actual
  code-->

* **Pay for what you eat**: instead of coming with *everything and the
  kitchen sink*, it provides the bare minimum to write applications
  without almost any implicit behavior. At the same time, it offers
  diverse paths to *grow with you* as applications become more complex.

  <!-- No monad transformers until they are needed.-->

* **Unopinonated**: there is no preferred routing method, HTML templating
  library, session management, web server or database adapter. It comes with
  some defaults to alleviate the [paradox of
  choice](https://en.wikipedia.org/wiki/The_Paradox_of_Choice), but most
  components are expected to be easily swapped in and out with plain 3rd
  party Haskell libraries writing thin adapters if at all needed.

  <!-- Is pluggable right here? Sounds out of fashion -->

* **Power of Haskell and Lens**: the [`Lens`][lens-home]-based API enables styles
  familiar to imperative programmers [`Lens`][lens-home] while being purely
  functional under the hood. Haskell veterans can take advantage of the
  powerful lens combinators.

[![Hackage Version](https://img.shields.io/hackage/v/nero.svg)](https://hackage.haskell.org/package/nero) [![Build Status](https://img.shields.io/travis/plutonbrb/nero.svg)](https://travis-ci.org/plutonbrb/nero)

## Example

```haskell
import Nero

app :: Request -> Maybe Response
app = request ^? _GET . match . prefixed "/hello/" . target <&> \name ->
    ok $ "<h1>Hello " <> name <> "</h1>"

main :: IO ()
main = serve 8080 app
```

:warning: `serve` is not implemented yet.

Check more examples with its corresponding tests in the [examples directory](
https://github.com/plutonbrb/nero/tree/master/examples).

[lens-home]: [https://lens.github.io/]
