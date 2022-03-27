# Instructions
- open command prompt in `main` folder. 
- run `make all` in command prompt. SML window will be opened.
- use `make clean` to remove extra files when you're done.   

# Functions Implemented
- use `parseFile "<FileName>"` to generate parse tree. All tokens will be printed along with parse tree for that file. (Note: File should be in same directory. Test Cases are available in `test-cases` folder)
- use `typeCheck "<FileName>"` to do type-checking.
- use `postfix "<FileName>"` to generate postfix form of the while language code.
- use `execute "<FileName>"` to evaluate the postfix expression generated. Type-checking is done before evaluating and relevant errors are printed in the console.

# Assignment 4 Files
- **stack.sml** : Signature for stack is included.
- **funstack.sml** : Created Stack structure based on signature defined in `stack.sml`
- **typeChecker.sml** : Created TypeChecker structure for type checking. Created symbol-table, and used AST to do type checking recursively.
- **Vmc.sml** : VMC structure to execute postfix command sequence in terms of semantic rules of VMC machine.
- **while-ast.aml** : Entry points to various functions like parseFile, parseString, typeCheck, execite etc have been defined.
  
# Other Design & Implementation Decisions
- Merged IntTerm, IntFactor, BoolTerm, BoolFactor into Expression to avoid reduce-reduce conflicts in parser. 

# Acknowledgements
- Boilerplate Code for load-while.sml referred from http://cs.wellesley.edu/~cs235/fall08/lectures/35_YACC_revised.pdf.