Introduction to Linear and Substructural Type Systems
=====================================================

An introductory talk to practical use linear and substructural type
systems through Linear Haskell and Rust.

All of the Linear Haskell commentary is describing
the work by Tweag (more inforation can be found
[here](https://github.com/tweag/ghc-proposals/blob/linear-types/proposals/00
00-linear-types.rst)). Tweag also has documentation
[about how to play around with their extension
](https://github.com/tweag/ghc/tree/linear-types), including how to use it
simply with docker.


Running the code in the repo
----------------------------

The code in this presentation is 

1. Set up docker on your local machine
2. Get the linear Haskell from the tweag docker repository:

    ```
    docker pull tweag/linear-types:0.1.8
    ```

   This code is only tested against this specific version of linear Haskell.

3. All the normal stack commands can be used. For example, to drop
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


Sources/Recommended Reading
---------------------------

- [Watertight 3D models](https://github.com/gelisam/linear-examples) that are
  correct by construction, with the help of Linear Haskell.
- Tweag's [Linear Haskell GHC](https://github.com/tweag/ghc/tree/linear-types) 
  branch.
- Tweag's [linear base library](https://github.com/tweag/linear-base/).
- [Linear Haskell GHC proposal](https://ghc.haskell.org/trac/ghc/wiki/LinearTypes)
- A more in-depth [proposal](https://github.com/tweag/ghc-proposals/blob/linear-types/proposals/0000-linear-types.rst) from Tweag.
- Everything listed in the `bib.bib` file.
