# autotool-capabilities

autotool-capabilities is a Haskell library providing monadic interfaces (capabilities) for
working with external tools like Alloy (formal modeling), PlantUML (diagramming), Graphviz
(graph visualization), and LaTeX SVG generation. The project consists of two packages: the
core capabilities library and IO instances for those capabilities.

Always reference these instructions first and fallback to search or bash commands only when
you encounter unexpected information that does not match the info here.

## ⚠️ CRITICAL ENVIRONMENT LIMITATIONS

**NETWORK RESTRICTIONS**: This development environment has severe network limitations that
prevent downloading Haskell dependencies from Hackage, Stackage, or GitHub. Stack builds
WILL FAIL with connection timeouts.

**WORKAROUNDS AVAILABLE**: Use the validation commands below for development work. For actual
building/testing, note the limitations and document what needs to be done in a proper
environment.

## Working Effectively

### System Dependencies (WORKING - Validated)

Install required system packages:

```bash
# Takes 2-3 minutes. NEVER CANCEL.
sudo apt-get update && sudo apt-get install -y graphviz
```

Verify installations:

```bash
dot -V                    # Should show graphviz version 2.43.0
java -version            # Should show OpenJDK 17 (required for Alloy/PlantUML)
stack --version          # Should show Stack 3.7.1
hlint --version          # Install HLint if needed (see below)
```

### HLint Installation and Linting (WORKING - Validated)

Install HLint manually (takes ~30 seconds):

```bash
curl -L https://github.com/ndmitchell/hlint/releases/download/v3.8/hlint-3.8-x86_64-linux.tar.gz | \
tar xz && sudo mv hlint-3.8/hlint /usr/local/bin/hlint
```

Run HLint validation (takes <1 second):

```bash
hlint autotool-capabilities/src/ autotool-capabilities/test/ \
autotool-capabilities-io-instances/src/ autotool-capabilities-io-instances/test/
```

### Build Process (NETWORK LIMITED - Document Only)

**⚠️ WARNING**: The following commands WILL FAIL in this environment due to network
restrictions, but document the correct process for a normal environment:

```bash
# In a normal environment with network access:
# Install dependencies - takes 15-30 minutes. NEVER CANCEL. Set timeout to 45+ minutes.
stack --no-terminal --install-ghc build --test --bench --only-dependencies

# Build and test - takes 5-15 minutes. NEVER CANCEL. Set timeout to 30+ minutes.
stack --no-terminal test --coverage --bench --no-run-benchmarks --haddock \
--no-haddock-deps
```

**Expected behavior in normal environment:**

- Dependency installation: 15-30 minutes (downloads GHC 9.4.8, all Haskell packages)
- Full build and test: 5-15 minutes
- Both commands require network access to Hackage and Stackage

### Project Structure Validation (WORKING - Validated)

Check project structure and consistency:

```bash
# Verify package.yaml → .cabal consistency (takes <1 second)
stack build --dry-run --no-install-ghc

# List project structure
ls -la autotool-capabilities*/
find . -name "*.hs" | head -10
```

### Alternative Development Commands (WORKING - Validated)

Since full builds don't work, use these for code analysis:

```bash
# Check Haskell syntax (requires manual verification)
find . -name "*.hs" -exec ghc -fno-code {} \; 2>&1 | head -20

# Examine dependencies and project info
cat autotool-capabilities*/package.yaml
cat stack.yaml
```

## Repository Structure

### Main Packages

- `autotool-capabilities/` - Core capability type classes and interfaces
- `autotool-capabilities-io-instances/` - IO instances implementing the capabilities

### Key Capabilities

- **Alloy**: Formal modeling and analysis (via call-alloy)
- **PlantUML**: UML diagram generation (via call-plantuml)
- **Graphviz**: Graph visualization (via graphviz system package)
- **LaTeX SVG**: Mathematical typesetting to SVG
- **Cache**: Caching mechanisms for expensive operations
- **WriteFile**: File writing capabilities

### Configuration

- Uses Stack with LTS 21.25 (GHC 9.4.8)
- Hpack for package.yaml → .cabal generation
- Git dependency on output-blocks library
- Conditional compilation flag: `alloy-use-sat4j` (for Windows SAT4J solver)

## Validation and CI Requirements

### Linting (WORKING)

Always run before committing:

```bash
hlint autotool-capabilities/src/ autotool-capabilities/test/ \
autotool-capabilities-io-instances/src/ autotool-capabilities-io-instances/test/
```

### CI Workflows (.github/workflows/)

The project has these automated checks:

- **haskell.yml**: Full build and test (requires network)
- **hlint.yml**: Haskell linting (use local hlint)
- **linter.yml**: Super-linter for general code quality
- **spelling.yml**: Spell checking with specialized dictionaries
- **consistency.yml**: Verifies .cabal files match package.yaml

### Consistency Check (WORKING)

Verify package.yaml and .cabal files are in sync:

```bash
# Save current .cabal files
find . -name '*.cabal' -exec cp {} {}.backup \;

# Regenerate (will fail but creates files)
stack build --dry-run --no-install-ghc || true

# Compare (should show no differences)
find . -name '*.cabal' -exec diff -u {}.backup {} \;
```

## Common Development Tasks

### Adding New Capabilities

1. Define type class in `autotool-capabilities/src/Capabilities/`
2. Add IO instance in `autotool-capabilities-io-instances/src/Capabilities/*/IO.hs`
3. Update exposed-modules in respective package.yaml files
4. Run hlint to check code quality
5. **Cannot test build in this environment** - document changes for later validation

### External Tool Dependencies

- **Graphviz**: System package (dot command) - INSTALLED ✅
- **Java**: Required for Alloy and PlantUML - AVAILABLE ✅
- **LaTeX**: Required for LaTeX SVG capability - NOT INSTALLED ❌
- **Alloy Analyzer**: JAR file downloaded by call-alloy package
- **PlantUML**: JAR file managed by call-plantuml package

### Windows Compatibility Note

On Windows, use SAT4J instead of MiniSat by setting flag in stack.yaml:

```yaml
flags:
  autotool-capabilities:
    alloy-use-sat4j: true
```

## Troubleshooting

### Network Timeout Issues

If you see "ConnectionTimeout" errors:

- This is expected in this development environment
- Commands will fail when trying to download dependencies
- Use the validation commands above that work locally
- Document what needs to be done in a normal environment

### Missing Dependencies

Install missing system packages:

```bash
# For LaTeX capabilities (if needed)
sudo apt-get install -y texlive-latex-base texlive-latex-extra

# For additional diagram tools
sudo apt-get install -y plantuml
```

### Build Timing Expectations (Normal Environment)

- **Dependency installation**: 15-30 minutes (first time only)
- **Incremental builds**: 1-5 minutes
- **Full rebuild**: 5-15 minutes
- **Test execution**: 1-3 minutes (currently no-op but placeholder for future)
- **HLint**: <1 second
- **Documentation generation**: 2-5 minutes

## Development Workflow

1. **Setup**: Install system dependencies (graphviz, hlint)
2. **Analysis**: Use hlint for code quality checks
3. **Structure**: Verify package.yaml consistency
4. **Changes**: Make minimal, focused modifications
5. **Validation**: Run working validation commands
6. **Documentation**: Note any build/test requirements for normal environment
7. **CI**: Ensure changes will pass all automated workflows

**Remember**: This environment cannot build or test the actual functionality, but you can
perform static analysis, code review, and structural validation.
