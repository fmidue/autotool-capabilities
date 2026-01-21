# autotool-capabilities

## Configuration

You may limit the maximum bit width by adding a limit (here 5) to your stack
command like `stack build --ghc-options="-DMAX_BIT_WIDTH=5"` or by amending your
`stack.yaml` like:

``` yaml
ghc-options:
  autotool-capabilities: -DMAX_BIT_WIDTH=5
```

This configuration is then used in order to reject configurations that do not
adhere to this limit.

## Compatibility

On Windows, you may have to use `SAT4J` instead of `MiniSat`.
In order to do so, you can set or change the appropriate flag for
`autotool-capabilities` under `flags` in the relevant `stack.yaml` to `true`.
Thus the section might look like this:

``` yaml
flags:
  autotool-capabilities:
    alloy-use-sat4j: true
```

Or provide it as an argument to each call of `stack`, e.g. `stack build --flag autotool-capabilities:alloy-use-sat4j`.
