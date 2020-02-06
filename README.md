# purescript-refinery

[<img src="./assets/logo.svg" width="140">](https://commons.wikimedia.org/wiki/File:Refinery.svg)

## Usage

### Simple

```haskell
type T
  = Refined Pos Int

refine 3 :: Either _ T
-- ok

refine (-3) :: Either _ T
-- Refinement error for value `-3`:
--
-- must be greater than 0

```

### Nested

```haskell
type H
  = Refined
      ( (Pos && Odd) || (Not Pos && Even) || Eq D8
      )
      Int

refine 3 :: Either _ H
-- ok

refine 6 :: Either _ H
-- Refinement error for value `6`:
--
-- At least one of this must hold:
--  - [ ] All of this must hold:
--      - [x] must be greater than 0
--      - [ ] must be odd
--  - [ ] All of this must hold:
--      - [ ] NOT
--          - [x] must be greater than 0
--      - [x] must be even
--  - [ ] must be equal 8


```

### Extending

```haskell
instance validateCustom :: Validate Custom Int where
  validate _ i =
    { result: i == 1234
    , evalTree: Satisfy $ "must be 1234"
    }

type G
  = Refined Custom Int

refine 1234 :: Either _ G
-- ok

refine 123 :: Either _ G
-- Refinement error for value `123`:
--
-- must be 1234
```

## Features

- Baked in logical predicates. (`And`, `Or`, `Xor`, `Not`)
- Provides a set of commonly used predicates for
  - Numbers (E.g `Lt`, `Gt`, `Pos`)
- Predicates are easily extensible
- Error messages provide detailed information about the predicate evaluation tree.

## Future

- More predicates for
  - Foldable
  - Strings

## Prior work

- [Purescript: refined](https://github.com/danieljharvey/purescript-refined)
- [Haskell: refined](http://hackage.haskell.org/package/refined)
