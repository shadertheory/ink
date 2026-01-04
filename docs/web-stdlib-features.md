# web stdlib feature spec (ink)

this spec defines non-core features for a web-focused standard library, built entirely in ink.

## 1) networking modules

### net.tcp
- `dial(host: string, port: int) -> result<tcp_stream, net_error>`
- `listen(host: string, port: int) -> result<tcp_listener, net_error>`
- `accept(listener) -> result<tcp_stream, net_error>`
- `tcp_stream` implements `stream` concept.

### net.udp
- `bind(host: string, port: int) -> result<udp_socket, net_error>`
- `send_to(socket, addr, data)`
- `recv_from(socket, buf)`

### net.dns
- `resolve(host: string) -> result<list<ip_addr>, dns_error>`
- cache with ttl.

### net.quic (phase 2)
- stream multiplexing api compatible with `stream`.

## 2) tls

### tls.client
- `connect(host: string, port: int) -> result<tls_stream, tls_error>`
- `wrap(stream: stream, host: string) -> result<tls_stream, tls_error>`
- alpn support for http/2, http/3.

### tls.server (phase 2)
- `accept(listener, cert, key) -> result<tls_stream, tls_error>`

## 3) http/1.1

### client
- `request(method, url, headers, body_stream) -> result<response, http_error>`
- `response` body is a `stream` with backpressure.
- automatic redirect policy (configurable).
- keep-alive pool.

### server
- `serve(listener, handler) -> result<void, http_error>`
- `handler(req) -> result<res, http_error>`

### parser/serializer
- incremental parsing with `buf`.
- chunked transfer support.
- header normalization with original casing preserved.

## 4) router

### route definitions
- `route("/users/{id:int}") -> handler`
- compile-time validation for conflicting routes.

### extraction
- `params.id` typed by route pattern.
- `query` parser with typed conversions.

## 5) json + schema

### serde traits
- `concept serialize<T>` and `concept deserialize<T>`.
- derive macros or codegen hooks later.

### validation
- `schema<T>` with runtime validation of user input.

## 6) middleware pipeline

### api
- `compose([m1, m2, ...], handler) -> handler`
- middleware can mutate `request`/`response` or short-circuit.

### built-ins
- logging
- timing
- rate limit (token bucket)
- cors

## 7) streaming + backpressure

### response streaming
- `res.body` is a stream.
- `res.write(chunk)` respects backpressure.
- `res.flush()` available.

## 8) observability

### tracing
- request id propagation.
- span api with structured fields.

### metrics
- counters, gauges, histograms.
- export to logs or text endpoint.

## 9) static file server

- `static(dir, cache_control)`
- range requests, etag.

## 10) registry client (package manager)

### api
- `registry.fetch(registry_name, package, version) -> result<path, reg_error>`
- cache layout: `.quill/registry/<name>/<package>/<version>`
- checksum verification (pluggable).

