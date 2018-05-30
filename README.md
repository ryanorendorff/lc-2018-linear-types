Introduction to Linear and Substructural Type Systems
=====================================================


Running the code in the repo
----------------------------

The code in this presentation is 

1. Set up docker on your local machine
2. Get the linear Haskell from the tweag docker repository:

    ```
    docker pull tweag/linear-types@sha256:6f68b8e02c2cd9e495af7826a13cd7c98e6503f8fab7d93a495d2b1676e9f336
    ```

   This code is only tested against this specific version of linear Haskell.

3. Initialize the submodules `git submodule update --init`
4. All the normal stack commands can be used. For example, to drop
   down into a linear Haskell REPL, use `stack ghci`. Note that the linear
   extension is always on in this branch of GHC.


Compiling the presentation
--------------------------

### Prerequisites

- `lhs2tex` installed on the system path. Run `stack install lhs2tex` outside
  of this directory.
- A system copy of LaTex with `xelatex`.

To compile the presentation, simply use `make`. This will make two versions
of the presentation: the normal presentations with slide breaks (`\pause` in
beamer) and a handout version with no slide breaks.
