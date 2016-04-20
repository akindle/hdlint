# hdlint
a verilog parser/linter, some day, maybe. parts of this kind of work, a bit, but lots of things are known to not work!
Current bothersome things:
- the replication operator/expression pretty much doesn't work. need to figure out how to handle replication/concatenation more cleanly.
- ternary operator implementation is bad and currently breaking range parsing
- better/more usage of recoverable parsers
- per above, check usages of "try" for places where recoverable parsers may make more sense