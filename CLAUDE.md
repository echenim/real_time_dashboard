# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Erlang/OTP-based real-time user presence dashboard designed for high-performance, low-latency tracking of user online/offline status. The project is currently in the scaffolding phase with comprehensive documentation but no implemented source code yet.

## Build and Development Commands

```bash
# Build the project
make build        # or: rebar3 compile

# Run in development
rebar3 shell      # Interactive Erlang shell with project loaded

# Create a release
make release      # or: rebar3 release

# Run the release
make run          # or: _build/default/rel/presence_dashboard/bin/presence_dashboard console

# Clean build artifacts
make clean        # or: rebar3 clean

# Install dependencies
rebar3 get-deps
```

## Architecture

The system follows OTP design principles with five core applications under `/apps/`:

1. **presence_core**: Core presence tracking logic, supervision trees, and state management
2. **presence_api**: Cowboy HTTP/WebSocket handlers for client connections
3. **presence_store**: Data layer abstraction (ETS, Redis, or PostgreSQL)
4. **presence_adapter_xmpp**: Optional MongooseIM/XMPP integration
5. **presence_admin**: Admin interface with metrics and management

### Key Design Patterns
- One GenServer per user for presence state
- WebSocket connections for real-time updates
- ETS for in-memory storage with optional persistence
- Telemetry/Prometheus for observability

## Development Workflow

When implementing features in this codebase:

1. First analyze the problem and read relevant documentation files
2. Create a plan in `tasks/todo.md` with checkable items
3. Get plan verification before implementation
4. Work through todo items systematically
5. Keep changes simple and minimal
6. Add a review section to todo.md with summary

## Configuration

- Runtime config: `/config/sys.config`
- VM arguments: `/config/vm.args`
- Build config: `/rebar.config`

## Testing

The project uses:
- `eunit` for unit tests
- `common_test` for integration tests
- `propEr` for property-based testing

Run tests with: `rebar3 eunit` or `rebar3 ct`

## Important Notes

- The project structure is fully documented but implementation code doesn't exist yet
- Follow OTP principles when implementing GenServers and supervision trees
- Use Cowboy for all HTTP/WebSocket handling
- Maintain fault-tolerance through proper supervision strategies
- All client connections should use WebSocket for real-time updates
- Authentication uses JWT/OAuth2 patterns