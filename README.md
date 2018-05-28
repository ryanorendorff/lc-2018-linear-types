Introduction to Linear and Substructural Type Systems
=====================================================


Running the code in the repo
----------------------------

The code in this presentation is 

1. Set up docker on your local machine
2. Get the linear Haskell from the tweag docker repository:

    ```
    docker pull git pull tweag/linear-types@sha256:cfa3c9e1473bbe95714c523d1b60dde95d7b9a2809f7478bb53bf86d87f1ec58
    ```

   This code is only tested against this specific version of linear Haskell.

3. Initialize the submodules `git submodule update --init`
4. All the normal stack commands can be used. For example, to drop
   down into a linear Haskell REPL, use `stack ghci`. Note that the linear
   extension is always on in this branch of GHC.


Compiling the presentation
--------------------------

To compile the presentation, simply use `make`. This will make two versions
of the presentation: the normal presentations with slide breaks (`\pause` in
beamer) and a handout version with no slide breaks.
