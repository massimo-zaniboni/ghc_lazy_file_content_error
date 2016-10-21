## Haskell GHC Bug

Test the interaction between hGetContents and hClose in case of lazy evaluation.

`hGetContents` closes automatically the handle when the end of the file
is reached. But in case of actions with exceptions, the end of the
file could be not reached and the handle can be left temporally open,
before the garbage collector reclaim it.

So the suggested pattern is managing file handles inside `bracket`
resource management functions. But this can introduce problems in case
of lazy evaluations, because the code processing the file content
can be called after the handle is closed.

This code tests the bad behaviour between brackets functions,
and lazy evaluation.

A bad behaviour is:
a file exception is raised at run-time,
when the lazy thunk is executed on a closed handle.

A very bad behaviour is:
no file exception is raised at run-time,
and the lazy thunk receives an empty file content,
also if the file has some content.

Many test cases have a very bad behaviour.

## Work Arounds

I tried with Lazy Attoparsec, and in case of errors during parsing
the file handle is correctly closed.
So in this case it suffices to not use the `bracket` resource management,
because there are no problems of resource leaks.

Another workaround is forcing a strict evaluation of the code inside the
`bracket` resource management, in order that no lazy thunk escape the
bracket.

## Definitive Solutions

A good behaviour should be:
the type signature prevents non strict evaluations to escape
the bracket action.

Another good behaviour should be:
`bracket` type signature creates a strict action that
when invoked acquires the resources, calculate the result,
and release the resources. So it has a strict behaviour
documented from the type system.

## Source Code

`Main.hs` contains examples of code with problems, and workaround examples.

## Installation and Testing
 
    stack build
    stack exec bracket
    stack exec bracket -- --stress-files

