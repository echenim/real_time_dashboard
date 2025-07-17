.PHONY: all build deps release run clean test dialyzer shell docs

# Default target
all: deps build

# Fetch dependencies
deps:
	rebar3 get-deps

# Compile the project
build:
	rebar3 compile

# Create a release
release:
	rebar3 release

# Run the release in console mode
run: release
	_build/default/rel/presence_dashboard/bin/presence_dashboard console

# Run development shell
shell:
	rebar3 shell

# Clean build artifacts
clean:
	rebar3 clean
	rm -rf _build log logs

# Run all tests
test:
	rebar3 do eunit, ct

# Run unit tests only
unit:
	rebar3 eunit

# Run common tests only  
ct:
	rebar3 ct

# Run dialyzer for type checking
dialyzer:
	rebar3 dialyzer

# Generate documentation
docs:
	rebar3 ex_doc

# Development helpers
dev: deps build shell

# Production release
prod:
	rebar3 as prod release

# Check code quality
check: build test dialyzer

# Format check (requires erlfmt)
fmt-check:
	rebar3 fmt --check

# Format code
fmt:
	rebar3 fmt
