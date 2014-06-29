# Experimental Haskell module for parsing and evaluating PHP code.

Tokenizer: parseString is used to process PHP files into an AST structure

Evaluator: evalParseResults is used to evaluate a parse result from parseString

CodeGen: genApp is used to generate Haskell code from a PHP AST structure

Building with cabal creates a binary which evaluates php file passed in as argument.

# Contribute

It might be a good idea to check out the PHP source code and look in
Zend/zend_language_parser.y to find missing things and to try to name the
Parsec tokens like the tokens in PHPs yacc tokenizer.