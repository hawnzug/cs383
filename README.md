# simpl

CS383 project.

## Build

First install the Haskell tool Stack:
```
curl -sSL https://get.haskellstack.org/ | sh
```

Build this project:
```
stack build
```

Run the tests:
```
stack test
```

Run the interpreter:
```
stack exec simpl -- -- examples/sum.spl
```
