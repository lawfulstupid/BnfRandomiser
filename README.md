Generates random strings that match a given BNF structure.

## Basic BNF Rules

A rule consists of a symbol name, followed by an equals sign, followed by an *expression*:

    symbol = "literal string" another_symbol | 'literal 2'
	 another_symbol = "foobar"

Symbol names may contain alphanumeric characters, underscores (_) and dashes (-).

An expression consists of one or more alternative *sequences* separated by pipes (|).

Each sequences consists of one or more *terms* separated by spaces.

A term is either a symbol name or a literal string.

Literal strings may be enclosed by single quotes (') or double quotes (").


## File Input

    symbol <- path/file.txt

if "path/file.txt" contains:

    option1
    option2
    option3

Then this is equivalent to:

    symbol = "option1" | "option2" | "option3"


## Sequence Weighting

Each sequence may be given an explicit weighting for random generation. This is done by appending a colon followed by an integer:

	long_string = 'x' long_string : 2 | ''

Sequences not assigned an explicit weight will have a weight of 1.


## Back References

A term may also be a reference to another term in the sequence, indicated by a dollar sign ($) followed by the index of the term to reference:
	
	double_letter = letter $1

The first term in the sequence has index 1, and so on.


## Commands
	
* load: Loads a BNF file into memory
* gen: Generate a symbol using file in memory
* run: Temporarily load a file and run any !commands contained within it
