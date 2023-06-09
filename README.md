Generates random strings that match a given BNF structure.

File format:

    symbol = "raw string" another_symbol | "raw string"
    another_symbol <- path/file.txt

if "path/file.txt" contains:

    option1
    option2
    option3

Then this is equivalent to:

    symbol = "raw_string" another_symbol | "raw_string"
    another_symbol = "option1" | "option2" | "option3"

Commands:
	
* load: Loads a BNF file into memory
* gen: Generate a symbol using file in memory
* run: Temporarily load a file and run any !commands contained within it
