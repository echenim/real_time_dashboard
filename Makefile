.PHONY: all build deps release run clean test dialyzer shell docs

# Default target
all: deps build

# Fetch dependencies
deps:
\trebar3 get-deps

# Compile the project
build:
\trebar3 compile

# Create a release
release:
\trebar3 release

# Run the release in console mode
run: release
\t_build/default/rel/presence_dashboard/bin/presence_dashboard console

# Run development shell
shell:
\trebar3 shell

# Clean build artifacts
clean:
\trebar3 clean
\trm -rf _build log logs

# Run all tests
test:
\trebar3 do eunit, ct

# Run unit tests only
unit:
\trebar3 eunit

# Run common tests only  
ct:
\trebar3 ct

# Run dialyzer for type checking
dialyzer:
\trebar3 dialyzer

# Generate documentation
docs:
\trebar3 ex_doc

# Development helpers
dev: deps build shell

# Production release
prod:
\trebar3 as prod release

# Check code quality
check: build test dialyzer

# Format check (requires erlfmt)
fmt-check:
\trebar3 fmt --check

# Format code
fmt:
\trebar3 fmt
