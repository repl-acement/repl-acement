[![CI-Lint](https://github.com/repl-acement/repl-acement/actions/workflows/lint.yml/badge.svg)](https://github.com/repl-acement/repl-acement/actions/workflows/lint.yml) [![CI-Test](https://github.com/repl-acement/repl-acement/actions/workflows/ci.yml/badge.svg)](https://github.com/repl-acement/repl-acement/actions/workflows/ci.yml)

# REPL-acement

Goal is to accept changes to the data we call code, persist the changes and offer a variety of feedback.

The server part exposes a shared PREPL, accessible via a web socket.

A limited set of commands are supported on the socket:

- [ ] save
- [ ] eval
- [ ] xform - see [AOP](AOP.md) document

## Usage

```bash
shadow-cljs watch repl-ui # UI
clojure -M:clj:server     # Server
```

The web server will be exposed on port `56665` by default

Optionally you can provide a port number:

```bash
$ clojure -M:clj:server 8888
```

Optionally you can provide the port number via an environment variable `PORT`:

```bash
$ PORT=8998 clojure -M:clj:server 
```

In all cases, a PREPL socket server will be started locally.
 
## Planned features

### Local code emission:

For all or some subset of the code

- [ ] export to file system
- [ ] export to produce a JAR
- [ ] export to produce a GRAALVM image


