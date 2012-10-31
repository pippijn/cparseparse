rule token = parse
| ['a'-'c']	{ "[a-c]" }
| [^'b']	{ "[^b]" }
