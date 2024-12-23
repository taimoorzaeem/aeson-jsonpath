# aeson-jsonpath

![ci-badge](https://github.com/taimoorzaeem/aeson-jsonpath/actions/workflows/haskell.yml/badge.svg?event=push) ![hackage-docs](https://img.shields.io/badge/hackage-v0.1.0.0-blue)

Run [RFC 9535](https://www.rfc-editor.org/rfc/rfc9535) compliant JSONPath queries on [Data.Aeson](https://hackage.haskell.org/package/aeson).

## Roadmap

- [ ] Selectors
  - [x] Name Selector
  - [x] Index Selector
  - [x] Slice Selector
  - [x] Wildcard Selector
  - [ ] Filter Selector
- [ ] Segments
  - [x] Child Segment
  - [ ] Descendant Segment
- [ ] Function Extensions

## Why use this?

- Provides a clean interface (single function call) to run [RFC 9535](https://www.rfc-editor.org/rfc/rfc9535) compliant JSONPath queries on [Data.Aeson](https://hackage.haskell.org/package/aeson) objects. 
- The parser is written in an extendable way and hence it will be able to provide newer features in our future releases possibly without any breaking changes.

```haskell
import Data.Aeson          (Value (..))
import Data.Aeson.JSONPath (runJSPQuery)

jsonDoc :: Value -- aeson value

-- currently supported queries are:
runJSPQuery "$" jsonDoc
runJSPQuery "$.*" jsonDoc -- supports wildcard operator
runJSPQuery "$.store" jsonDoc -- supports member name shorthand
runJSPQuery "$.store.books" jsonDoc
runJSPQuery "$.store.books[0]" jsonDoc
runJSPQuery "$.store.books[-4]" jsonDoc -- supports negative index
runJSPQuery "$.store.books[1:4:-1]" jsonDoc -- suports slicing with negative step
runJSPQuery "$.store.books[1,2:5,7]" jsonDoc -- supports multiple selectors
```

## Development

Please report any bugs you encounter. Contributions are welcomed.
