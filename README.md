# hive-emacs

Emacs vessel addon for [hive-mcp](https://github.com/hive-agi/hive-mcp) — implements `IVessel` / `IEditor` for Emacs.

## What it does

hive-emacs extracts all Emacs-specific code from hive-mcp core into a standalone IAddon project:

- **IEditor** via `EmacsclientEditor` — emacsclient subprocess with circuit breaker and daemon death detection
- **Daemon management** — multi-daemon DataScript store, selection, autoheal
- **Elisp helpers** — template system for eval-elisp calls
- **Olympus** — grid layout calculation for multi-buffer arrangements
- **Notifications** — desktop notification integration
- **cljel transpilation** — Clojure-to-Elisp source files for Emacs-side integration

## Architecture

hive-emacs follows the IAddon protocol pattern. It registers itself via classpath scanning:

```clojure
;; META-INF/hive-addons/hive-emacs.edn
{:addon/id           "hive.emacs"
 :addon/type         :native
 :addon/init-ns      "hive-emacs.init"
 :addon/init-fn      "init-as-addon!"
 :addon/capabilities #{:tools :editor :health-reporting}}
```

The init pipeline follows the nil-railway pattern: `resolve-deps → register → init → store`.

## Usage

Add as a git dependency in your `deps.edn`:

```clojure
io.github.hive-agi/hive-emacs
{:git/tag "v0.1.0" :git/sha "..."}
```

hive-mcp's classpath scanner will auto-detect and initialize the addon.

## Dependencies

- `org.clojure/clojure` 1.12.1
- `datascript/datascript` 1.7.8 — daemon store
- `io.github.hive-agi/hive-dsl` — Result monad, rescue/guard DSL
- `babashka/process` — emacsclient subprocess execution
- `com.taoensso/timbre` — logging

## License

Copyright (c) 2024-2026 hive-agi contributors

EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0. See [LICENSE](LICENSE).
