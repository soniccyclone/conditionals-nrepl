# conditionals-nrepl

This repository exists ONLY to create new hosted versions of nREPL. It does NOT change any existing nREPL functionality.

## What that means

- Shared code changes only to add a host seam (`nrepl.util.*` facades with per-host bodies) or a reader conditional. The JVM behaviour behind every seam stays what nREPL shipped.
- A new host implements parity with the JVM behaviour, including the parts that look wrong. Parity is the contract; `plan/03-design.org` is the spec.
- Anything that looks wrong in nREPL's own behaviour is documented, not fixed: file an issue with the `inherited` label that states the behaviour, where it lives, how it was observed and what a fix would change, so Nathan can address all of them together once the host implementations are done. Issue #44 is the pattern.
- Tests added here assert nREPL's existing contract or a new host's parity with it, never a behaviour nREPL did not promise. A flake in a fork-added assertion is fixed in the assertion, not in nREPL.
- A crash on one of nREPL's own documented paths may be fixed to do what nREPL documents (issue #17 is the pattern). Everything else goes in an `inherited` issue as above.
- No distribution, packaging, or upstream work until the project works. Only this fork exists; never push to or open anything against nrepl/nrepl.

## Working here

- Plan and decisions: `plan/*.org`. Roadmap order is the `P1`..`P4` labels on the issues.
- Local test loop: podman images `nrepl-jvm` and `nrepl-clr` from `plan/verify/`. CI (`.github/workflows/jvm.yml`, `clr.yml`) is the final confirmation on a PR, never the dev loop.
- Every PR closes an issue; stacked PRs target the branch they stack on; Nathan merges.
