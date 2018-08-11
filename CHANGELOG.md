# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Detect reconverging grammars in GLR analysis.
- Separately track state history in conflict arcs.

### Changed
- Grammar parser is now self-hosted (generated based on a grammar file).
- Make backend annotations in grammar files optional.
- Improve grammar rule ID printing.

## 0.3.0 - 2018-08-10
### Added
- Comments in grammar files.
- Backslash escapes inside quoted identifiers in grammar files.
- Detailed verbosity control for the command line tool (options `-v|-vv|-vvv|-vvvv|-vvvvv|-q|-t`).
- Add support for conflict arcs that backtrack beyond the initial point of conflict.
- Add `parser::make_grammar` function.
