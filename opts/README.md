This directory consists of a script `./run.sh` and some Erlang examples to test how Erlang optimizes code.

Each example file gets compiled with and without optimizations, and you can inspect the diffs at each stage.
Additionally, `./$suite/no_$example.erl` and `./$suite/yes_$example.erl` will be compared against each other too.

So if you want to see if two ways of writing Erlang are equivalent, you should look at `./$suite/opt/$example.S.diff` for the yes/no examples.
If you just want to understand what optimizations are performed on a particular Erlang file, inspect `./$suite/$example.*.diff`.

The Erlang compilation pipeline goes through these stages:

- source (`.erl`)
- core (`.core`/`.copt`)
- [SSA](https://en.wikipedia.org/wiki/Static_single-assignment_form) (`.ssa`)
- BEAM (`.S`)
- JITted (not inspectable, but does not do much optimization AFAIK).

I believe SSA is where most of the optimizations happen, especially type-based optimizations, though I am skeptical that those apply much to idiomatic PS code.

If you want to understand more about these files and the optimizations, read the Erlang blog: https://www.erlang.org/blog/a-brief-beam-primer/.

## Summaries

- `./iife`: Nullary [IIFE](https://en.wikipedia.org/wiki/Immediately_invoked_function_expression) gets beta-reduced in core.
  (Need to try more with non-nullary functions.)
- `./matching`: Nested pattern matching: somewhat optimized, but not fully, AFAICT.
  (A larger test case would help make this apparent.)
- `./repeat`: Redundant accessors: somewhat optimized but generally disappointing.
- `./top-level`: Does a top-level almost-literal reference get inlined? Nope.

  This is mainly a problem for `purescript-unicode` and I will probably have to redo that library if we want to use it seriously in Erlang.
- `./misc`: shrug

In short, a lot of the code patterns that are natural in PureScript(/`backend-optimizer`) do not get optimized as much as I would like.
