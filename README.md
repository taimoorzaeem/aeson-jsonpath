# aeson-jsonpath

![ci-badge](https://github.com/taimoorzaeem/aeson-jsonpath/actions/workflows/build.yml/badge.svg?event=push) [![hackage-docs](https://img.shields.io/badge/hackage-v0.2.0.0-blue)](https://hackage.haskell.org/package/aeson-jsonpath)

Run [RFC 9535](https://www.rfc-editor.org/rfc/rfc9535) compliant JSONPath queries on [Data.Aeson](https://hackage.haskell.org/package/aeson).

## Roadmap

- [ ] Selectors
  - [x] Name Selector
  - [x] Index Selector
  - [x] Slice Selector
  - [x] Wildcard Selector
  - [ ] Filter Selector
- [x] Segments
  - [x] Child Segment
  - [x] Descendant Segment
- [ ] Function Extensions
- [ ] Setting Values (Non-RFC)

## Why use this?

- Provides a clean interface (single function call) to run [RFC 9535](https://www.rfc-editor.org/rfc/rfc9535) compliant JSONPath queries on [Data.Aeson](https://hackage.haskell.org/package/aeson) objects. 
- The parser is written in an extendable way and hence it will be able to provide newer features in our future releases possibly without any breaking changes.

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.Aeson          (Value (..))
import Data.Aeson.JSONPath (runJSPQuery, jsonPath)

jsonDoc :: Value -- aeson value

-- usage example
runJSPQuery [jsonPath|$|] jsonDoc
runJSPQuery [jsonPath|$.*|] jsonDoc
runJSPQuery [jsonPath|$..*|] jsonDoc
runJSPQuery [jsonPath|$.store|] jsonDoc
runJSPQuery [jsonPath|$.store.books|] jsonDoc
runJSPQuery [jsonPath|$.store.books[0]|] jsonDoc
runJSPQuery [jsonPath|$.store.books[-4]|] jsonDoc
runJSPQuery [jsonPath|$.store.books[1:4:-1]|] jsonDoc
runJSPQuery [jsonPath|$.store.books[1,2:5,7]|] jsonDoc
```

## Development

Please report any bugs you encounter by opening an issue.
