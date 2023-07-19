# lambda

This is a small lambda calculus intepreter that I wrote to keep my Haskell game up. A very painful lesson that I have learnt from this project is [this one](https://stackoverflow.com/questions/9976388/haskell-text-parsec-combinator-choice-doesnt-backtrack). You can very easily figure out the input format from the file `test.lambda`. The input is either provided from `STDIN` by default (terminates when `EOF` is provided) or by providing an input file as an argument.
