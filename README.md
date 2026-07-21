# hive-emacs

Host-neutral Emacs integration packaged as a `hive-addon`.

## What it does

hive-emacs owns all Emacs-specific code and exposes it through data and injected
function ports:

- **Editor and vessel descriptors** — plain maps a host can adapt to its own protocols
- **emacsclient execution** — bounded `hive-weave` pool, timeout, circuit breaker, daemon death detection
- **Daemon management** — private DataScript store, selection, heartbeat, autoheal, redistribution
- **Elisp helpers** — template system for eval-elisp calls
- **Olympus** — grid layout calculation for multi-buffer arrangements
- **Notifications** — desktop notification integration
- **cljel transpilation** — Clojure-to-Elisp source files for Emacs-side integration

## Architecture

hive-emacs implements the host-neutral `hive-addon.protocol/IAddon` contract.
`hive-addon.mount` discovers its manifest and lets the host drive registration
and lifecycle:

```clojure
;; META-INF/hive-addons/hive-emacs.edn
{:addon/id           "hive.emacs"
 :addon/type         :native
 :addon/init-ns      "hive-emacs.addon"
 :addon/init-fn      "addon-ctor"
 :addon/capabilities #{:tools :mcp-bridge :health-reporting
                       :editor :vessel :terminal}}
```

`addon-ctor` is pure: it returns an uninitialized addon. The mounter owns
`construct → register → initialize`; hive-emacs owns only its internal resources.
It never imports, resolves, or stores a host-core API.

Cross-domain state enters through `:emacs/ports` (or the direct compatibility
keys). Ports are functions such as `:lookup-ling-fn`, `:tasks-for-ling-fn`,
`:emit-fn`, and `:terminal-dispatch-fn`. This is the dependency-inversion seam;
the host supplies adapters during `initialize!`.

## Usage

Add hive-emacs to a host's classpath, then let `hive-addon.mount` discover
`META-INF/hive-addons/hive-emacs.edn`. For example:

```clojure
io.github.hive-agi/hive-emacs
{:git/tag "v0.1.0" :git/sha "94641c5d79ee80b713762569c88ca2b75eda7756"}
```

Any hive-addon-compatible host can discover and initialize it; no hive-mcp core
dependency is required.

## Dependencies

- `org.clojure/clojure` 1.12.1
- `datascript/datascript` 1.7.8 — daemon store
- `io.github.hive-agi/hive-dsl` — Result monad, rescue/guard DSL
- `io.github.hive-agi/hive-addon` — host-neutral IAddon contract
- `io.github.hive-agi/hive-schemas` + `metosin/malli` — canonical contracts and generators
- `io.github.hive-agi/hive-weave` — bounded asynchronous effects
- `babashka/process` — emacsclient subprocess execution
- `com.taoensso/timbre` — logging

## Verification

```bash
clj-kondo --lint src test src-typed test-ansatz
clojure -M:test
clojure -M:typed -e "(require 'typed.clojure 'hive-emacs.typed-scoring) (typed.clojure/check-ns-clj 'hive-emacs.typed-scoring)"
clojure -M:ansatz-test
bash scripts/check-cljel-parity.sh
```

Malli is the source for runtime validation, generated property/contract/mutation
tests, and Typed Clojure aliases. The Ansatz suite proves the saturating capacity
laws over every natural number and checks its executable model against the
production primitive.

## License

Copyright (c) 2024-2026 hive-agi contributors

EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0. See [LICENSE](LICENSE).
