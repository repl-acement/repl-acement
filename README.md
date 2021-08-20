[![CI-Lint](https://github.com/repl-acement/repl-acement/actions/workflows/lint.yml/badge.svg)](https://github.com/repl-acement/repl-acement/actions/workflows/lint.yml) [![CI-Test](https://github.com/repl-acement/repl-acement/actions/workflows/ci.yml/badge.svg)](https://github.com/repl-acement/repl-acement/actions/workflows/ci.yml)

# REPL-acement

A Clojure server exposing a shared PREPL, accessible via a web socket.

It exposes:
- PREPL output
- keystrokes
- `clj-kondo` analysis data

## Usage

```bash
$ clojure -M:clj:server
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

- [ ] JVM PREPL
- [ ] nodeJS PREPL

## Planned examples 
- [ ] distribution
- [ ] hosting
- [ ] networking
