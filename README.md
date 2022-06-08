[![CI-Lint](https://github.com/repl-acement/repl-acement/actions/workflows/lint.yml/badge.svg)](https://github.com/repl-acement/repl-acement/actions/workflows/lint.yml) [![CI-Test](https://github.com/repl-acement/repl-acement/actions/workflows/ci.yml/badge.svg)](https://github.com/repl-acement/repl-acement/actions/workflows/ci.yml)

### Event flow

It is organized to match the life-cycle:
- `event` to set the whole view of a specific form-id
- `fn` to write the parts CMs to set the parts view of the whole
- `event` to transact changes (keystrokes) on the whole form
- `fn` to ripple out change to appropriate parts
- `event` to transact changes (keystrokes) on any of the form parts
- `fn` to reflect back the part change to whole

# Immediate TODO

- [ ] Make data persistent and recursively hashing
- [ ] Use `events/defn` as a model for working with single and multi-arity parts. 
- [ ] Look to simplify. It seems complex atm but after doing `events/ns` I'm hoping to find some patterns that can be shared.

### Code mirror 
- [ ] side-effect refactoring for supported forms

### Shared spec namespaces
- [ ] use consistently throughout the code
- [ ] have all common data in the shared ns?? (thinking yes)

### Persist editing changes
- [ ] event to persist changes at user behest
- [ ] event to persist changes when the form under inspection is changed

### Set warnings if not conformed
- [ ] event to set that warnings exist
- [ ] use a 'humane' lib to expose the spec warnings
- [ ] persist warnings with changes


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