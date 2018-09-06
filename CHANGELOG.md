# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Extended grammar in the `ext` module.
- Lowering of extended grammar to basic grammar.
- Support for grouped, optional, and repeated symbols.
- Names for grammar symbols (e.g. `IDENT:foo`).
- Switch to dump the parsed grammar (`--dump-grammar`).
- Switch to dump the parsed extended grammar (`--dump-ext-grammar`).
- Switch to synthesize the AST (`--synth-ast`).
- Automated Syntax Tree node and reduction function synthesis.

### Changed
- Command line tool builds uses extended grammar now.

## 0.4.1 - 2018-08-13
### Changed
- Now dual-licensed under MIT and Apache 2.0.

### Fixed
- Shift/shift conflicts produced by Honalee algorithm.

## 0.4.0 - 2018-08-11
### Added
- Separately track state history in conflict arcs.
- Detect reconverging grammars in GLR analysis.
- Detect local ambiguities (reconvergent state traces).
- Local ambiguity and resolution printing.
- Grammar rule pretty printing.
- The `ItemSet::kernel_items` function.
- Rule slices.

### Changed
- Grammar parser is now self-hosted (generated based on a grammar file).
- Make backend annotations in grammar files optional.
- Improve conflict reporting.
- Improve grammar rule ID printing.
- `Conflict` fields made public.
- `LocalAmbiguity` fields made public.

## 0.3.0 - 2018-08-10
### Added
- Comments in grammar files.
- Backslash escapes inside quoted identifiers in grammar files.
- Detailed verbosity control for the command line tool (options `-v|-vv|-vvv|-vvvv|-vvvvv|-q|-t`).
- Add support for conflict arcs that backtrack beyond the initial point of conflict.
- Add `parser::make_grammar` function.
