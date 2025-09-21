# Serializotron

![Serializotron Logo](./serializotron.png)

**A high-performance, content-addressable serialization format with deduplication**

Serializotron is a binary serialization format designed for efficient storage and transmission 
of structured data. It features automatic deduplication, content-addressable storage, and strong 
type safety.

# TODO

## Interning

Experiment with interning strings.

## Transport mode

Offer a "transport mode" that prunes rarely used metadata (e.g. field labels, module names) even from 
the initial type table. For many consumers, a type tag plus field order is enough.

## Tool Ideas

- szt view – pretty-print DynamicValue trees with collapsible paths, raw vs interpreted modes, and optional JSON export for piping into other tooling.
- szt jq – jq-style query language that understands constructors (Just, Left, etc.) so you can extract substructures or field values without deserialising in Haskell.
- szt diff – semantic diff between two .szt files that highlights constructor/index changes, shared-table reuse, and reports size deltas.
- szt edit – small interactive editor to tweak primitive fields or swap constructors, writing back a valid .szt; handy when reproducing bugs.
- szt stats – summarise dedup efficiency, schema versions, type frequencies, and detect oversized blobs; useful in CI to monitor regressions.
- szt fsck – packaging of existing integrity checks plus round-trip fuzzing on subsets; emits actionable hints (missing constructor cases, unknown types).
- szt grep – search primitives/constructor names across files (with decoding), returning file:field paths; helps track where a value lives in large archives.
- szt pack/unpack – bundle multiple .szt into a tar-like container with index and metadata, enabling atomic updates or streaming pipelines.
- szt schema – extract a declarative schema (e.g. JSON/YAML) showing constructors/fields; can diff schemas or check compatibility against a git baseline.
- szt repl – launch a shell with toSzt/fromSzt helpers, quick conversions to JSON/CBOR, and ability to run guessed FromSZT instances for exploratory decoding.

## License

MIT License - see [LICENSE](./LICENSE) for details.
