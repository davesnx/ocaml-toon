# Vendored spec fixtures

Source: https://github.com/toon-format/spec
Path: `tests/fixtures`
Commit: d6db4b04303bdea132351ce45aed612311c850b2
Date: 2026-08-23
Spec version: 4.1

To refresh, from a checkout of the spec repo at the desired commit:
```sh
for d in encode decode; do cp path/to/toon-spec/tests/fixtures/$d/*.json test/fixtures/$d/; done
```
Files are copied verbatim; do not hand-edit them.
