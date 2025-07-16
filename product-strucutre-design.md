
# 📦 Project Structure – Real-Time User Presence Dashboard

This is the production-grade project structure for the `real_time_dashboard` system, built using Erlang/OTP and Cowboy, with optional MongooseIM integration.

---

## 🔧 Directory Tree Overview

```plaintext
real_time_dashboard/
├── apps/
│   ├── presence_core/             # Core logic for tracking presence
│   ├── presence_api/              # Cowboy HTTP/WebSocket handlers
│   ├── presence_adapter_xmpp/     # Optional MongooseIM integration
│   ├── presence_store/            # ETS, Redis or DB logic
│   ├── presence_admin/            # Admin interface logic
│
├── config/
│   ├── sys.config                 # Runtime config (e.g., DB, JWT secret)
│   ├── vm.args                    # Erlang VM args (node name, cookies)
│
├── rel/
│   └── real_time_presence_dashboard/  # Release metadata for rebar3
│       └── files/
│           ├── sys.config
│           └── vm.args
│
├── rebar.config                   # Dependencies and build config
├── rebar.lock                     # Locked versions
├── README.md                      # Project documentation
└── Makefile                       # (Optional) for Docker, CI, build shortcuts
```

---

## 🧠 apps/presence_core/

Handles OTP logic and application supervision:

- Tracks user session state
- Manages worker lifecycles
- Publishes and routes presence updates

```plaintext
presence_core/
├── src/
│   ├── presence_app.erl             # OTP application entry
│   ├── presence_sup.erl             # Top-level supervisor
│   ├── presence_worker_sup.erl      # DynamicSupervisor (one per user/group)
│   ├── presence_worker.erl          # GenServer for tracking single session
│   ├── presence_router.erl          # Pub/Sub router (ETS/gproc/pg)
│   └── presence_metrics.erl         # Telemetry hooks for observability
├── include/
│   └── presence.hrl                 # Common macros/records
├── test/
│   └── presence_worker_tests.erl    # Unit tests
```

---

## 🌐 apps/presence_api/

Exposes REST and WebSocket endpoints via Cowboy:

- Provides external interface for clients
- Maintains real-time bi-directional connection

```plaintext
presence_api/
├── src/
│   ├── presence_api_app.erl
│   ├── presence_api_sup.erl
│   ├── presence_http_router.erl     # Cowboy routing rules
│   ├── presence_ws_handler.erl      # Cowboy WebSocket handler
│   └── presence_http_handler.erl    # REST endpoints for querying status
├── test/
│   └── websocket_auth_tests.erl     # WebSocket auth and handshake tests
```

---

## ✉️ apps/presence_adapter_xmpp/

Optional integration with MongooseIM:

- Listens to XMPP presence events
- Bridges XMPP stanzas to internal state

```plaintext
presence_adapter_xmpp/
├── src/
│   ├── presence_xmpp_app.erl
│   ├── presence_xmpp_sup.erl
│   ├── presence_xmpp_listener.erl     # GenServer subscribing to MongooseIM hooks
│   └── presence_xmpp_mapper.erl       # Translates XMPP stanza to internal format
```

---

## 💾 apps/presence_store/

Manages volatile and persistent presence data:

- Provides interfaces for ETS, Redis, and PostgreSQL
- Used for state syncing and historical analytics

```plaintext
presence_store/
├── src/
│   ├── presence_store_app.erl
│   ├── presence_store_sup.erl
│   ├── presence_ets.erl              # In-memory fast-access store
│   ├── presence_redis.erl            # Optional Redis sync
│   └── presence_pg.erl               # Optional PostgreSQL history writer
```

---

## 🛡️ apps/presence_admin/

Admin panel backend logic:

- Provides metrics, filters, and aggregated views
- Can impersonate or inspect user sessions

```plaintext
presence_admin/
├── src/
│   ├── presence_admin_app.erl
│   ├── presence_admin_sup.erl
│   ├── presence_admin_handler.erl     # Admin-specific REST/WebSocket endpoints
│   └── presence_admin_metrics.erl     # Metrics aggregation and analysis
```

---

## ⚙️ config/

Configuration files loaded at runtime and boot time.

```plaintext
config/
├── sys.config     # Environment-specific settings (e.g., DB creds, feature flags)
├── vm.args        # VM-level flags (e.g., node name, cookie, distribution settings)
```

---

## 📦 rel/

Rebar3 release configuration for generating deployable builds.

```plaintext
rel/real_time_presence_dashboard/
└── files/
    ├── sys.config   # Overridden by environment-specific configs
    └── vm.args      # VM settings for the release
```

---

## 📄 Other Root Files

- `rebar.config`: Declares dependencies, compiler settings, release targets
- `rebar.lock`: Auto-generated lock file for dependency versions
- `Makefile`: Optional build automation (rebar3 commands, Docker, etc.)
- `README.md`: Entry point for contributors and documentation

---

Let me know if you'd like a ZIP scaffolding, stub files for each `.erl`, or to auto-generate `rebar3` apps for this layout.
