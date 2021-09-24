# Hindley-Milner type inference in F#

A toy implementation of the Hindley-Milner type inference algorithm for the
purpose of learning F# and type inference.

### Supported Stuff

- int (`int`)
- bool (`bool`)
- lambda (`arg -> ret`)
- lambda application
- Automatically generated type variables

### Stuff not supported yet

- `let`
- `letrec`
- Polymorphic `let`

### Running

You will need `dotnet` installed on your machine.

```sh
dotnet run --project HindleyMilner
```

The output will look something like

```
❯ dotnet run --project HindleyMilner
Term: ((fn x => x) 1)
Inferred type: int

Term: ((fn x => x) true)
Inferred type: bool

Term: +
Inferred type: int -> int -> int

Term: (+ 1)
Inferred type: int -> int

Term: *
Inferred type: int -> int -> int

Term: (* 1)
Inferred type: int -> int

Term: (fn x => ((+ x) 1))
Inferred type: int -> int

Term: ((+ 1) false)
Inference error: Types int and bool do not unify

Term: (fn x => x)
Inferred type: 'l -> 'l

```
