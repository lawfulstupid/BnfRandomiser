Generates random strings that match a given BNF structure.

File format:

    symbol = another_symbol | "raw string"
    load another_symbol from path/file.txt

if "path/file.txt" contains:

    option1
    option2
    option3

Then this is equivalent to:

    symbol = another_symbol | "raw_string"
    another_symbol = "option1" | "option2" | "option3"
