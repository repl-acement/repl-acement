[![CI-Lint](https://github.com/repl-acement/repl-acement/actions/workflows/lint.yml/badge.svg)](https://github.com/repl-acement/repl-acement/actions/workflows/lint.yml) [![CI-Test](https://github.com/repl-acement/repl-acement/actions/workflows/ci.yml/badge.svg)](https://github.com/repl-acement/repl-acement/actions/workflows/ci.yml)

# REPL-acement

Goal is to accept changes to the data we call code, persist the changes and offer a variety of feedback.

The server part exposes a shared PREPL, accessible via a web socket.

A limited set of commands will be supported on the socket:

- [ ] save
- [ ] eval
- [ ] xform - see [AOP](AOP.md) document

## Usage

```bash
shadow-cljs watch repl-ui # UI
clojure -M:clj:server     # Server
```