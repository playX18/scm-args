# args

R7RS-small CLI parser library. Handles all kinds of arguments and usage generation. It does not handle
command runners or any other way to handle subcommands. It's user responsibility to register subcommands into
parser grammar. See `example/grammar.scm` on how to create a simple parser.