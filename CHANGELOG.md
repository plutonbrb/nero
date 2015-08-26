# Change Log
All notable changes to this project will be documented in this file. This file
follows the formatting recommendations from [Keep a
CHANGELOG](http://keepachangelog.com/). This project adheres to [Semantic
Versioning](http://semver.org/).

## [Unreleased][unreleased]
### Added
- `stack` support.
- `nest` and `reroute`.
- Basic tests.

### Changed
- `HasBody` is now a `Lens` instead of a `Getter`.
- Restructured how multimaps are stored internally. This affects the way to
  create them, parsing and rendering,  is slightly  different.

### Removed
- Examples moved to its own package.

## [0.3.1][0.3.1] - 2015-4-16
### Fixed
- Make examples private modules so they can be built.

### Added
- Lower bounds versions for dependencies.

## [0.3][0.3] - 2015-4-12
This release includes changes driven by the initial implementation of
[`nero-wai`](https://github.com/plutonbrb/nero-wai).
### Changed
- `GET` and `POST` are now types on their own instead of constructors for
  `Request`. `Request` is now sum type wrapper for the types `GET` and `POST`
- `Payloaded` is now a `Lens'` and has been renamed to `HasPayload`.
  `Request` is not an instance of `HasPayload` anymore, instead use `payloaded`.
- Rename `server` -> `application` in `Server` type class.

### Added
- `split` for combining `match` and `sep`.
- `notFound` response.
- `payloaded` `Prism'` for `Request` `Payload`s.
- `HasBody` instance for `Request`.
- `Prism'` for `Response`s with different status.
- `null`, `fromList` for `MultiMap`.
- `Binary` module including `Renderable` and `Parseable` classes.
- `Renderable` instance for `Url`.
- Single `Response` instance for `Server`.

## [0.2] - 2015-4-5
### Changed
- Replace basic routing (monoidal matching) with lens routing.
- Limit exports for central module `Nero`.

### Added
- Custom `Prelude` (`Nero.Prelude`) extended with frequently used imports.

## [0.1.1] - 2015-3-30
### Fixed
- Support for ghc-7.6.3 and ghc-7.10.1.

## [0.1] - 2015-3-30
### Added
- Basic routing.
- HTTP parameters handling for both *query string* and form encoded `POST`s.
- Trailing slash redirection.

[unreleased]: https://github.com/plutonbrb/nero/compare/v0.3.1...HEAD
[0.3.1]: https://github.com/plutonbrb/nero/compare/v0.3...v0.3.1
[0.3]: https://github.com/plutonbrb/nero/compare/v0.2...v0.3
[0.2]: https://github.com/plutonbrb/nero/compare/v0.1.1...v0.2
[0.1.1]: https://github.com/plutonbrb/nero/compare/v0.1...v0.1.1
[0.1]: https://github.com/plutonbrb/nero/compare/a2c3f720...v0.1
