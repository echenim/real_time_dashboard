# Real-Time User Presence Dashboard

The Real-Time User Presence Dashboard is an interactive application designed to display real-time user presence information. Built with Erlang/OTP and leveraging the Cowboy framework for handling HTTP and WebSocket connections, this dashboard is perfect for applications needing to show user status dynamically. Integration with MongooseIM for XMPP messaging capabilities is also considered, making it a robust solution for real-time communication platforms.

## Features

- **Real-Time Updates**: Display user presence status (online, offline, busy) in real-time.
- **Erlang/OTP Foundation**: Benefits from the high concurrency, fault tolerance, and distribution capabilities of Erlang/OTP.
- **WebSocket Support**: Utilizes WebSocket for live, bidirectional communication between the client and server.
- **Scalable Architecture**: Designed to handle a growing number of users and connections efficiently.

## Getting Started

### Prerequisites

- Erlang/OTP 24 or later
- Rebar3

### Installation

1. **Clone the repository**

    ```sh
    git clone https://github.com/echenim/realtime_dashboard.git
    cd realtime_dashboard
    ```

2. **Compile the application**

    ```sh
    rebar3 compile
    ```

3. **Run the application**

    ```sh
    rebar3 shell
    ```

    The dashboard is now accessible at `http://localhost:8080`.

### Configuration

Modify settings in `config/sys.config` for customization, including:

- HTTP and WebSocket server configurations.
- MongooseIM integration settings, if applicable.

## Usage

Open a web browser and navigate to `http://localhost:8080` to view the dashboard. The page will update in real-time as users' presence statuses change.

## Development

### Adding New Features

- To add more real-time functionalities, extend the WebSocket handler in `src/ws_handler.erl`.
- For UI enhancements, modify `priv/static/index.html` and associated CSS/JavaScript files.

### Testing

Run automated tests with Rebar3:

```sh
rebar3 eunit
