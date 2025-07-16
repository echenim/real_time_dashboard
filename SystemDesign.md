
# 📡 Real-Time User Presence Dashboard – System Design Document

**Title:** Real-Time User Presence Dashboard  
**Lead Architect:** [Your Name]  
**Date:** July 16, 2025  
**Tech Stack:** Erlang/OTP, Cowboy, MongooseIM, PostgreSQL/ETS, Redis (optional), WebSocket  

---

## 🔭 1. Purpose

The **Real-Time User Presence Dashboard** is a backend and frontend system designed to:

- Show real-time online/offline status of users.
- Allow authenticated WebSocket subscriptions.
- Track and update presence data with low latency.
- Optionally integrate with **MongooseIM** to reflect XMPP presence.

---

## 🎯 2. Core Features

| Feature | Description |
|--------|-------------|
| 🟢 Real-Time Presence Tracking | Tracks user online/offline status across multiple devices or services. |
| 🛰️ WebSocket-based Dashboard | Live feed via Cowboy WebSocket handler—no polling, ultra-low latency. |
| 🔐 Authenticated Sessions | Secure token-based or JWT authentication to manage session presence. |
| 🧠 OTP Supervision Trees | Robust supervision model ensures each component self-heals on failure. |
| 🔄 Multi-Node Distribution | Presence state replicates across Erlang nodes using `pg2` / `gproc`. |
| 🗃️ Optional Persistent Store | Presence history can be persisted in PostgreSQL or Redis. |
| ✉️ XMPP Integration (Optional) | With MongooseIM, mirrors XMPP presence signals to the dashboard. |
| 📊 Presence Analytics | Time spent online, device types, and location patterns. |
| ⚙️ Admin Panel | Admins can view aggregated data, filter by group, or impersonate sessions. |

---

## 🏗️ 3. High-Level Architecture

```
               +---------------------+
               |     Admin Panel     |
               +---------+-----------+
                         |
                         | REST/WebSocket
                         v
+-----------+    +--------------------+    +----------------+
| Web Client| -> | Cowboy HTTP Server | -> | Presence Engine|
+-----------+    +--------------------+    +----------------+
                         |                          |
                         | uses OTP behaviours      | subscribes
                         v                          v
                   +-------------+           +---------------+
                   | WebSocket   |<--------->| User Registry |
                   | Handler     |           |   (ETS/Redis) |
                   +-------------+           +---------------+
                                                  |
                                                  |
                                        +--------------------+
                                        | MongooseIM Adapter |
                                        +--------------------+
                                                  |
                                           +--------------+
                                           | MongooseIM DB|
                                           +--------------+
```

---

## ⚙️ 4. Component Breakdown

### A. Cowboy HTTP/WebSocket Server

- Uses `cowboy_router` to direct HTTP and WS requests.
- WebSocket handlers use Erlang processes per user session.
- Each connection is supervised using a `simple_one_for_one` supervisor.

### B. Presence Engine (Core)

- **Tracks**: user sessions, online/offline states.
- **Stores**: current presence in ETS; long-term in Redis/PostgreSQL.
- **Pushes**: updates to subscribed sockets.
- **Design**: OTP GenServer per user or group (sharded).

### C. User Registry

- Built using ETS for low-latency access.
- Optionally uses `gproc` or `pg` for distributed presence sync.
- Could be backed up by Redis for cross-node recovery.

### D. MongooseIM Adapter (Optional)

- Erlang GenServer that subscribes to MongooseIM events.
- Translates XMPP stanzas into internal presence updates.
- Leverages `mod_presence` hooks or database polling.

### E. Admin Panel

- Built in Phoenix/React/LiveView for UI.
- Shows:
  - Total online users
  - Last seen timestamps
  - Session breakdown
  - Filters by group, role, geography

---

## 🧱 5. Supervision Tree Design

```
real_time_presence_app
├── cowboy_listener_sup
├── presence_registry_sup
│   └── presence_worker_sup (DynamicSupervisor)
│       └── presence_worker (gen_server)
├── mongoose_adapter_sup (optional)
├── presence_store_sup
│   ├── ets_manager (gen_server)
│   └── db_writer (gen_server)
```

---

## 🌐 6. Scalability & Distribution

- BEAM distribution handles intra-node comms.
- ETS tables use read-concurrency and are partitioned if needed.
- Nodes can scale horizontally (shared-nothing) with presence state replicated using:
  - `pg` or `gproc` for clustering
  - Redis for cross-region cluster sync (optional)

---

## 🔐 7. Security

- JWT or OAuth2 token validation before WebSocket upgrade.
- Rate limiting and origin checks at Cowboy level.
- Encrypted traffic over TLS.

---

## 📈 8. Observability & Monitoring

- Use `telemetry` + `prometheus.erl` to expose metrics:
  - Active users
  - WebSocket connection health
  - Latency of updates
  - XMPP adapter failures
- Admin metrics exposed via `/metrics` endpoint.

---

## 🧪 9. Testing Strategy

- `eunit` and `common_test` for unit/integration tests.
- Property-based testing with `propEr` for crash-resilience and consistency.
- Simulation of network partitions and presence conflicts in test harness.

---

## 🛠️ 10. Deployment Considerations

- Deploy with `rebar3` releases.
- Systemd + Docker container support.
- Blue-green or hot-code upgrades supported via `release_handler`.

---

## 🔮 11. Future Enhancements

- Presence replay: visualizing who was online over time.
- Geo-awareness: display region of connection (via IP).
- Alerting: Slack/webhook when certain users or roles go online.
- Typing indicators, activity pulses.
