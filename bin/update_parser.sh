#!/bin/bash
# Update the grammar parser.
set -e
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)
TMP=$(mktemp)
perplex $ROOT/src/grammar.plx > $TMP
cp $TMP $ROOT/src/parser_states.rs
