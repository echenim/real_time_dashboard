
# 📡 Real-Time User Presence Dashboard

## 📝 Project Brief

The **Real-Time User Presence Dashboard** is a production-grade, Erlang/OTP-based system designed to deliver low-latency, real-time user presence information across multiple clients and services. This project uses the Cowboy HTTP/WebSocket framework to provide a resilient and scalable interface for presence tracking and optionally integrates with **MongooseIM** for XMPP messaging platforms.

The system is intended for high-performance communication platforms, collaborative tools, real-time games, or enterprise dashboards requiring up-to-the-millisecond user presence status.

---

## 🎯 Objectives

- ✅ Track user online/offline status in real-time
- ✅ Deliver WebSocket updates to clients with sub-100ms latency
- ✅ Support secure, authenticated connections
- ✅ Maintain resilient distributed state using Erlang OTP
- ✅ Provide optional integration with MongooseIM/XMPP
- ✅ Enable flexible data storage using ETS, Redis, or PostgreSQL

---

## 🧱 Architecture Overview

- **Erlang/OTP**: Backbone for concurrency, fault-tolerance, supervision
- **Cowboy**: Handles HTTP and WebSocket connections
- **ETS/Redis/PostgreSQL**: In-memory and persistent storage options
- **MongooseIM Adapter**: Translates XMPP presence into dashboard state
- **Admin API**: Aggregated views, session tracking, impersonation tools

---

## 🧩 Key Components

| App                     | Purpose                                                             |
|-------------------------|---------------------------------------------------------------------|
| `presence_core`         | Presence logic, state management, supervision tree                 |
| `presence_api`          | WebSocket + REST handlers using Cowboy                             |
| `presence_store`        | Data layer abstraction (ETS, Redis, PostgreSQL)                    |
| `presence_adapter_xmpp` | MongooseIM integration (optional)                                  |
| `presence_admin`        | Admin endpoints, metrics, and management interfaces                |

---

## 🔒 Security

- JWT/OAuth2-based authentication for session control
- Rate limiting and origin protection on WebSocket upgrades
- TLS encrypted communication

---

## 📈 Observability

- Telemetry-based metrics exported via Prometheus-compatible endpoints
- Real-time and historical analytics of user activity
- Alerts on session flapping or suspicious activity (planned)

---

## 🚀 Getting Started

1. **Install Dependencies**  

   ```bash
   rebar3 get-deps
   ```

2. **Build Project**  

   ```bash
   rebar3 compile
   ```

3. **Run Locally**  

   ```bash
   rebar3 shell
   ```

4. **Release & Deploy**  

   ```bash
   rebar3 release
   _build/default/rel/real_time_presence_dashboard/bin/real_time_presence_dashboard console
   ```

---

## 📁 Project Structure

See [`real_time_dashboard_project_structure.md`](./real_time_dashboard_project_structure.md)

---

## 🧪 Testing

- Unit tests via `eunit`
- Integration tests via `common_test`
- Property-based testing with `propEr`

---

## 🔮 Roadmap

- [ ] Live dashboard with Phoenix/LiveView or React
- [ ] Geo-distributed presence sync via Redis
- [ ] Typing indicators and last-seen analytics
- [ ] UI/UX for admin tools

---

## 🤝 Contributing

Contributions, bug reports, and feature requests are welcome!

1. Fork the repo
2. Create a branch: `feature/my-feature`
3. Submit a PR

---

## 📄 License

MIT License © 2025 [Your Name / Your Organization]
