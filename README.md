# perplex

This is *perplex*, a grammar analyzer and parser generator for almost-context-free languages.

## Related Work

- [D. Tribble, "Practical LR(k) Parser Construction"][Tribble2004]

[Tribble2004]: http://david.tribble.com/text/lrk_parsing.html

## Todo

- [x] item set generation
- [x] item set merging/compression
- [x] conflict reporting
- [x] state space computation
- [ ] GLR analysis with recursive rules
- [x] separate extended grammar with symbol groups, optional symbols, repetitions
- [ ] generalization to CFG, tiered parsing
- [ ] location and span tracking
- [ ] automated AST synthesis

## License

Perplex is licensed under the terms of the MIT license and the Apache License (Version 2.0), at your discretion. See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) for details.
