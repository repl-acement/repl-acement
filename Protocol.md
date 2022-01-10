# Data Persistence Protocol

## Goals
The protocol goals are:
- UI components should have a stable understanding of how to save and retrieve project data.
- UI components can be extended or replaced providing they conform to the protocol 
- the protocol can be implemented using any type of persistence store that can accept conformant input and produce conforming results
- the protocol can be implemented with or without a network between the UI and the persistent medium.
- the persistent medium can be varied so that testing and lightweight experimentation are simple.

## Objectives

Specific protocol objectives are:
- Don't lose user data. All keystrokes and other edits are always saved.
- Respect user privacy. All actions can be performed anonymously. Any integrations that require authentication are outside the core protocol.  
- Enforce data signing to improve the software supply chain.
- Low cost data exchange. Despite wanting to exchange rich data, a compact form of request / response is sought to minimize round trips costs. There may be higher cost exchanges outside the core protocol.

## Non-Objectives

Non-objectives of the protocol are:
- Interoperability
  - ClojureScript clients and Clojure servers are mandated.
- CLJ/CLJS legacy compatibility
  - ClojureScript clients must use ClojureScript 1.10.nnn and higher.
  - Clojure servers must be 1.10 or higher.

## Actions

### Save - Client

Web sockets data packet from the client
 
- Key
  - :repl-acement/save
- Value
  - Keys 
    - id
    - type
    - name
    - ns-name
    - data, a map of:
      - text
      - conformed
      - unformed

#### Specs

- sample from the [data spec](repl-shared/replacement/protocol/data.cljc)

```clojure
(s/def ::form-data
  (s/keys :req-un [::text ::conformed ::unformed]))
```

- sample from the [events spec](repl-shared/replacement/protocol/events.cljc)

```clojure
(s/def ::form-save
  (s/keys :req [::data/id ::data/type ::data/name ::data/ns-name ::data/form-data]))
```

- example conforming message

```clojure
#:replacement.protocol.data{:id        #uuid"cfe7bd64-f23b-40b3-bc6a-fdedd26293d8",
                            :type      defn,
                            :var-name  xy,
                            :ns-name   user,
                            :form-data {:text      "(defn xy [x y] (+ x y))",
                                        :conformed {:defn-type defn,
                                                    :defn-args {:fn-name xy,
                                                                :fn-tail [:arity-1
                                                                          {:params {:args [[:local-symbol x]
                                                                                           [:local-symbol y]]},
                                                                           :body   [:body [(+ x y)]]}]}},
                                        :unformed  (defn xy [x y] (+ x y))}}
```

### Save - Server

Web sockets data packet response from the server

- Key
  - :repl-acement/saved
- Value
  - Key
    - form-id (uuid)
  - Value
    - ok? (boolean)

Link to the spec [TBD]

### Sync

#### Form Sync - Client

Web sockets data packet from the client

- Key
  - :repl-acement/sync-form
- Value
  - form-id (uuid)

Link to the spec [TBD]

#### Form Sync - Server

Web sockets data packet response from the server

- Key
    - :repl-acement/synced-form
- Value
    - Key
      - form-id (uuid)
    - Value
      - form.type (symbol)
      - form.text (string)
      - form.conformed-data (map from clojure.core.specs)

Link to the spec [TBD]

#### Classpath Sync - Client

Web sockets data packet from the client

- Key
  - :repl-acement/sync-classpath
- Value
  - app-id (uuid)

Link to the spec [TBD]

#### Classpath Sync - Server

Web sockets data packet response from the server

- Key
  - :repl-acement/synced-classpath
- Value
  - Key
    - app-id (uuid)
  - Value, array of
    - dep-name / dep data (maven / git)
      - namespaces / vars per dep

Link to the spec [TBD]

## UX

UX around form persistence:

When considering form persistence from a user perspective, these are some minimal examples of when the persistence should be conducted to reduce any "surprising" data loss:
- explicitly via a user action.
- when switching function / form.
- before a transformation.

Between saves, the keystrokes should be kept in local storage to ensure that nothing from a coding session is lost during a program crash. 

As an example of how data can be stored, keystrokes for Code Mirror 6+ are: form id + tx from the whole form Code Mirror instance.



