# autotool-capabilities

## Compatibility

On Windows, you may have to use `SAT4J` instead of `MiniSat`.
In order to do so you can change the provided flag in `stack.yaml` or `stack-*.yaml` to:

``` yaml
    alloy-use-sat4j: true
```

Or provide it as argument to each call of `stack`, e.g. `stack build --flag autotool-capabilities:alloy-use-sat4j`.
