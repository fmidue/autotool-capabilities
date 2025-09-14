# autotool-capabilities

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
