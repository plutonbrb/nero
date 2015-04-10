# Change Log
All notable changes to this project will be documented in this file. This file
follows the formatting recommendations from [Keep a
CHANGELOG](http://keepachangelog.com/). This project adheres to [Semantic
Versioning](http://semver.org/).

## [Unreleased][unreleased]
### Changed
- `GET` and `POST` are now types on their own instead of constructors for
  `Request`. `Request` is now sum type wrapper for the types `GET` and `POST`
- `Payloaded` is now a `Lens'` and has been renamed to `HasPayload`.
  `Request` is not an instance of `HasPayload` anymore, instead use `payloaded`.
### Added
- `split` for combinining `match` and `sep`.
- `notFound` response.
- `payloaded` `Prism'` for `Request` `Payload`s.
- Smart constructors for `GET` and `POST`.
- `HasBody` instance for `Request`.
- `Binary` module.

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

[unreleased]: https://github.com/jdnavarro/nero/compare/v0.2...HEAD
[0.2]: https://github.com/jdnavarro/nero/compare/v0.1.1...v0.2
[0.1.1]: https://github.com/jdnavarro/nero/compare/v0.1...v0.1.1
[0.1]: https://github.com/jdnavarro/nero/compare/a2c3f720...v0.1
