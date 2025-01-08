# Change Log

All notable changes to this package are documented in this file. This project adheres to [Haskell PVP](https://pvp.haskell.org/) versioning.

## Unreleased

- Allow spaces around the JSONPath query
- Always return `Vector Value` and rename `runJSPQuery` to `query`
- Replace hyphen with underscore in `member-name-shorthand`
- Implement Filter Selector

## 0.2.0.0

- Remove dependency on `protolude`
- Fix parsing bug with Wildcard Selector
- Implement Descendant Segment
- Fix allowed characters in the `member-name-shorthand`
- Add `QuasiQuoter` for compile-time syntax checking

## 0.1.0.0

- Initial Release
