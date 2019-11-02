# Mary

#### Mary had a little lambda: a λ-calculus -> RISC-V compiler

- This project is a compiler
- The input language is the untyped [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
- The output language is RISC-V assembly, targeting RV64I systems
- The implementation language is Haskell
- Among other reasons, I am writing this project to fulfill a project class at my college
- Name/pun credits go to Cassie @porglezomp: https://twitter.com/porglezomp/status/1189651774199566337?s=20
- Previous work towards this goal was written in Python with ANTLR4, but that code is not online

### Current status

- Actually just a lambda calculus interpreter, has nothing to do with RISC-V
- Seems to kinda work as an interpreter tho 🤷‍♀️

### Building

- You should have Stack, Happy, Alex, and BNFC installed:
    - for Stack: `curl -sSL https://get.haskellstack.org/ | sh`
    - for everything else: `sudo apt-get install happy alex bnfc`
- In the main directory, run `bnfc --haskell -o src Lambda.bnfc`
- This will put generate the parser and lexer implementations and put everything in `src` where we don't touch it
- It'll also put a program called TestLambda.hs in there, which for some reason has a module called Main, which messes everything up, so run `rm src/TestLambda.hs`
- Run `stack build` to build
- Run `stack exec Mary-app`
