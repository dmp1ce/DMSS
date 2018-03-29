# DMSS
Dead Man's Switch System

**This library is not production-ready and should not be used for purposes
where cryptographic failure may endanger anyone's life, liberty or pursuit of
happiness. Neither stability nor quality of either API or implementation is
guaranteed at the moment.**

See [DESIGN.md](DESIGN.md) for now.

## Development

For active development use the `dev` application to watch for file changes and running test continually.

Build `dev`.

`stack build :dev`

To watch a subset of tests.

`stack exec -- dev test -w -p "my test filter"`

Or run all the tests.

`stack exec -- dev test`

See `--help` for details.

`stack exec -- dev --help`
