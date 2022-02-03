# elm-review-forestry

An exploratory rule for generating code. When enough context is available, it will attempt to replace specific `Debug.todo` with appropriate code. E.g. pattern matching a `Maybe` or `Result` into their branches.

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to generate code.

## Provided rules

- [`PruneTodo`](https://package.elm-lang.org/packages/wolfadex/elm-review-forestry/1.0.0/PruneTodo) - Attempts to replace instances of `Debug.todo ":prune"` with applicable code..

## Configuration

```elm
module ReviewConfig exposing (config)

import PruneTodo
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ PruneTodo.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template wolfadex/elm-review-forestry/example
```

---

Very much inspired by Haskell's [Wingman](https://haskellwingman.dev/) and [Program Synthesis in Idris](https://www.youtube.com/watch?v=brjFqXkUQv0).
