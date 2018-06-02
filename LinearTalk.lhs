%if False

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE EmptyDataDecls #-}
> module LinearTalk where
> import qualified Prelude.Linear as PL
> import Prelude hiding (FilePath)
> import Num

> -- Linear Array interface taken from the paper
> data MArray a 
> data Array a

> newMArray :: Int -> (MArray a ->. PL.Unrestricted b) ->. b
> newMArray = undefined
>
> write :: MArray a ->. (Int, a) -> MArray a
> write = undefined
>
> read :: MArray a ->. Int -> (MArray a, PL.Unrestricted a)
> read = undefined
> 
> freeze :: MArray a ->. PL.Unrestricted (Array a)
> freeze = undefined
>
> foldlL :: (a ->. b ->. a) -> a ->. [b] ->. a
> foldlL = undefined
>
> -- The actual function that guarantees arrays are written correctly.
> array :: Int -> [(Int, a)] -> Array a
> array size pairs = newMArray size (\ma -> freeze (foldlL write ma pairs))

%endif

\documentclass{beamer}

\usepackage[backend=biber,citestyle=authoryear,style=alphabetic]{biblatex}
\bibliography{bib.bib}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Beamer Style (based on Berkeley colors)  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\usetheme{metropolis}

\definecolor{BerkeleyBlue}{RGB}{0,50,98}
\definecolor{FoundersRock}{RGB}{59,126,161}
\definecolor{Medalist}{RGB}{196,130,14}

\setbeamercolor{frametitle}{fg=white,bg=FoundersRock}
\setbeamercolor{title separator}{fg=Medalist,bg=white}


\usefonttheme[onlymath]{serif}


\usepackage{amsmath}
\usepackage{amsthm}
\usepackage{textcomp}
\usepackage{amssymb}

\usepackage{hyperref}

\usepackage{appendixnumberbeamer}

\usepackage[utf8]{inputenc}
\usepackage{pmboxdraw}

\usepackage{fancyvrb}
\usepackage{xcolor}

\usepackage{mathpartir}
\usepackage{fontspec}
\usepackage{cmll}
\usepackage{unicode-math}

\usepackage[plain]{fancyref}

\usepackage{minted}

%% Footnotes without an accomanying numerical binding.
\newcommand\blfootnote[1]{%
  \begingroup
  \renewcommand\thefootnote{}\footnote{#1}%
  \addtocounter{footnote}{-1}%
  \endgroup
}

\newcommand{\m}[1]{$#1$}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lhs2tex formatting rules                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%include polycode.fmt
%include beamer.fmt
%include forall.fmt
%format ->. = "\multimap"
%format .         = ". "
%format _ (a)         = "_{" a "}"
%format ω = "\omega"
%format π = "\pi"
%format ρ = "\rho"
%format ⅋ = "\parr"

%format +. = "+_{\!1} "
%format -. = "-_{1} "
%format *. = "*_{1} "
%format /. = "/_{1} "

\author{Ryan~Orendorff, Daniel~Hensley}
\title{Introduction to Linear Type Systems}
\subtitle{\verb|github.com/ryanorendorff/???????|}
\date{June 5th, 2018}
\hypersetup{pdflang={English}}


\begin{document}

\frame{\titlepage}


\begin{frame}
\frametitle{Outline}
\tableofcontents[]
\end{frame}

% For the first section, do not highlight the current section (overview slide)
\AtBeginSection[] {}
\section{Motivation: Properly handling a resource}

% For all subsequent sections, highlight what section we are currently in.
\AtBeginSection[]
{
  \begin{frame}
    \frametitle{Table of Contents}
    \tableofcontents[currentsection]
  \end{frame}
}

\begin{frame}{Opening a file is pretty easy in any programming language}

Let's say we have the following \textsc{api} for accessing a resource (files).

> data File
> data FilePath 

> openF    ::  FilePath -> IO File
> closeF   ::  File -> IO ()

\pause

> appendF  ::  File -> String -> IO ()

\pause

> now      ::  IO String

%if False

> openF   = undefined
> appendF = undefined
> closeF  = undefined
> now     = undefined

%endif

\end{frame}


\begin{frame}{Simple IO program for accessing files}

Let's write a simple program for adding the current date to the end of the
file.

> appendTimeToFile :: FilePath -> IO ()
> appendTimeToFile path = do
>   f <- openF path 
>   n <- now
>   appendF f n
>   closeF f

\end{frame}


\begin{frame}{What if we close the file on accident}

What if we made a mistake and closed the file. Does the result still typecheck?

> appendTimeToFile' :: FilePath -> IO ()
> appendTimeToFile' path = do
>   f <- openF path 
>   n <- now
>   closeF f     
>   appendF f n  -- Oops, we closed the file

\pause

\emph{The developer is responsible} for a property that the compiler does
not check for.

\end{frame}


\begin{frame}{Block diagram of the \textsc{api} demonstrates the issue}

If we look at the \textsc{api} interconnectivity, we can see the challenge.

\begin{center}
  \includegraphics{figs/file_api.pdf}
\end{center}

\end{frame}


\begin{frame}{Block diagram of the \textsc{api} demonstrates the issue}

If we look at the \textsc{api} interconnectivity, we can see the challenge.

\begin{center}
  \includegraphics{figs/file_api_error.pdf}
\end{center}

\end{frame}


\begin{frame}{Second attempt: compiler enforced resource closing}

If we instead had an \textsc{api} where the file handle \emph{must} be
closed, then we could assure that any open resource must be closed.

\begin{center}
  \includegraphics{figs/file_linear_api.pdf}
\end{center}

Substructural types allow us to accomplish this!

\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        Substructural Section                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Substructural Type Systems}

\begin{frame}{Unrestricted type systems have three structural rules}

In most type systems, three structural properties that allow unrestricted
use of a variable; unrestricted meaning variables can be dropped,
duplicated, and reordered.

The structural rules are exchange, contraction, and weakening.

\end{frame}


\begin{frame}{Exchange: we can use type proofs in any order}

The exchange rule allows us to type check in any desired order when multiple
terms have to be checked at the same time.

\pause

Restricting this property means that we have to type check terms \emph{in
the opposite order they were introduced} (a \textsc{filo} order).

% \begin{equation*}
%   Γ, x:A, y:B ⊢  t:T \text{, then } Γ, y:B, x:A ⊢  t:T
% \end{equation*}

\pause

For example, when type checking the following function

> exchange = let x = (4 :: Int) in let y = (5 :: Int) in (x, y)

We can check that |x| or |y| are |Int|s in either order.

\end{frame}


\begin{frame}{Contraction: we can make duplicates of type proofs}

The contraction rule lets us use a proof of a variable's type twice.

\pause

Restricting this property means we \emph{can't use a term more than once}.

% \begin{equation*}
%   Γ, x:A, x:A ⊢  t:T \text{, then } Γ, x:A ⊢  t:T
% \end{equation*}

\pause

For example, when type checking the following function

> dup :: a -> (a, a)
> dup x = (x, x)

We can use the proof that |x :: a| twice.

\end{frame}


\begin{frame}{Weakening: we can discard undeeded type proof}

The weaking rule means we can discard unnecessary type proofs.

\pause

Restricting this property means we \emph{must use a term at least once}.


% \begin{equation*}
%   Γ ⊢  t:T \text{, then } Γ, x:A ⊢  t:T
% \end{equation*}

\pause

For example, when type checking the following function

> kill :: a -> ()
> kill x = ()

We do not need to use the fact that |x :: a| while type checking the right
hand side.

\end{frame}


\begin{frame}{Substructural type systems remove 1 or more structural rule}

Substructural type systems remove or replace one or more of the structural
rules.

Let's look at the most useful substructural systems.

\end{frame}


\begin{frame}{Unrestricted type systems are most common}

You can use a variable as many times as you like, including zero.

This is the type system for Haskell, Java, C, etc.

\begin{center}
  \includegraphics[width=\textwidth]{figs/unrestricted.pdf}
\end{center}

\end{frame}


\begin{frame}{Relevant type systems: every variable used at least once}

In relevant typing systems, a variable \emph{must be used}. 

Drops the weakening rule.

\begin{center}
  \includegraphics[width=\textwidth]{figs/relevant.pdf}
\end{center}

\end{frame}


\begin{frame}{Affine type systems: every variable used at most once}

A variable can be used zero or one times. Drops the contraction rule.

Example languages include Alms and Rust.

\begin{center}
  \includegraphics[width=\textwidth]{figs/affine.pdf}
\end{center}

\end{frame}


\begin{frame}{Linear type systems: every variable used \emph{exactly} once}

A variable must be used \emph{exactly} once. Drops both the weakening and
contraction rules.

This type system is implemented in the Linear Haskell extension.

\begin{center}
  \includegraphics[width=\textwidth]{figs/linear.pdf}
\end{center}

\end{frame}


\begin{frame}{Ordered type systems: every variable must be used in order}

All variables must be used and must be used in \textsc{filo} order.

Ordered type systems have none of the structural rules.

\begin{center}
  \includegraphics[width=\textwidth]{figs/ordered.pdf}
\end{center}

\end{frame}


\begin{frame}{Substructural type system relation diagram}

This leads to a convenient diagram describing how the substructural systems
relate to each other.

\begin{center}
  \includegraphics[width=\textwidth]{figs/substructural.pdf}
\end{center}

\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               Linear Types by Linear Haskell Section                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Overview of Linear Types through Linear Haskell}

\begin{frame}{The |sum| function can be written linearly}

Say we want to take the sum of a list. How do we check that we consumed every
element exactly once?
\blfootnote{|->.| is represented as \verb|->.| in source code}

\pause

%format sumL = "\Varid{sum_{L}} "
%format x_extra = "\textcolor{red}{\Varid{x}} "

> sumL :: [Int] ->. Int
> sumL []      =  0
> sumL (x:xs)  =  x +. sumL xs

\end{frame}


\begin{frame}[fragile]{When a linear |sum| uses an element twice}

What if we accidentally use an element twice?

< sumL :: [Int] ->. Int
< sumL []      =  0
< sumL (x:xs)  =  x +. sumL xs +. x_extra

\pause
 
The type checker will helpfully tell us we violated linearity.

\begin{Verbatim}[commandchars=\\\{\}]
LinearTalk.lhs:447:10: error:
    • \textcolor{red}{Couldn't match expected weight '1' of}
      \textcolor{red}{variable 'x' with actual weight '\m{\omega}'}
\end{Verbatim}

\end{frame}


\begin{frame}[fragile]{When a linear |sum| forgets an element?}

What if we forgot to use the elements of our list?

%format one_kill = "\textcolor{red}{1} "

< sumL :: [Int] ->. Int
< sumL []      =  0
< sumL (x:xs)  =  one_kill +. sumL xs 

\pause
 
The type checker will helpfully tell us we violated linearity.

\begin{Verbatim}[commandchars=\\\{\}]
LinearTalk.lhs:447:10: error:
    • \textcolor{red}{Couldn't match expected weight '1' of}
      \textcolor{red}{variable 'x' with actual weight '0'}
\end{Verbatim}

\end{frame}


\begin{frame}{What does it mean to "consume" a variable}

To consume a variable exactly once, we use the following rules

\begin{itemize}[<+->]
  \item An atomic base type: evaluate the value once
  \item A function: Pass in one argument and consume the result exactly
        once.
        \begin{itemize}[<+->]
          \item This is a bit tricky. It means, for |(f :: A ->. B)|, if |f u| 
                is used exactly once, \emph{then} |u| is used exactly once.
        \end{itemize}
  \item For any algebraic data type, pattern match and consume all components
        exactly once.
        \begin{itemize}
          \item For a pair, this means pattern match and consume each
                component exactly once.
        \end{itemize}
\end{itemize}

\end{frame}


\begin{frame}{Gotchas with the function arrow}

> f :: s ->. t
> g :: s -> t
> g x = f x

%if False

> f = undefined

%endif 

\end{frame}

\section{Two examples using Linear Types}

\begin{frame}
\frametitle{Examples using Linear Types}
  \begin{itemize}
    \item File IO or array example
    \item Scanners galore!
  \end{itemize}
\end{frame}


\section{The competition}

\begin{frame}
  Also called "the competition".
\end{frame}


\section{Rust}

\begin{frame}
\frametitle{Rust}
  Rust is a ``mainstream'' language that ships with a substructural type
  system.
\end{frame}

\begin{frame}[fragile]
\frametitle{Opening a File in Rust}
\begin{minted}{rust}
fn append_time_to_file(
    p: Path,
    n: String
) -> io::Result<()>
{
    let f = File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    s.push_str(&n);
    drop(f); // close the file
}
\end{minted}
\end{frame}

\begin{frame}[fragile]
\frametitle{What if We Forgot to Close it?}
\begin{minted}{rust}
fn append_time_to_file(
    p: Path,
    n: String
) -> io::Result<()>
{
    let f = File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    s.push_str(&n);
    // <- no `drop`
}
\end{minted}
\end{frame}

\begin{frame}[fragile]
\frametitle{Cannot Forget! The Compiler Inserts `drop`}
\begin{minted}{rust}
fn append_time_to_file(
    p: Path,
    n: String
) -> io::Result<()>
{
    let f = File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    s.push_str(&n);
} // compiler `drop`s all resources of scope here
\end{minted}
\end{frame}

\begin{frame}
\frametitle{`drop` in Rust Ownership Type System}

The Rust compiler establishes and tracks \textbf{ownership}.

\begin{itemize}
    \item The compiler automatically inserts calls to \mintinline{rust}{drop}
    when an owned type goes out of scope.
    \item This provides automatic memory safety without GC.
    \item It also means that you cannot forget to ``finalize'' resources such
    as files, sockets, etc.
\end{itemize}
\end{frame}

\begin{frame}[fragile]
\frametitle{What if We Close or `move` the File Early?}
\begin{minted}{rust}
fn append_time_to_file(
    p: Path,
    n: String
) -> io::Result<()>
{
    let f = File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    drop(f); // early drop
    s.push_str(&n);
}
\end{minted}

Here we accidentally close the file too early.
\end{frame}

\begin{frame}[fragile]
\frametitle{What if We Close or `move` the File Early?}
\begin{minted}{rust}
fn append_time_to_file(
    p: Path,
    n: String
) -> io::Result<()>
{
    let f = File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    drop(f); // early drop!
    s.push_str(&n);
}
\end{minted}

\pause

No worries -- we will get a Rust compile time error. An owned file cannot be
used again after being used once (here, \mintinline{rust}{drop}ped).
\end{frame}

\begin{frame}[fragile]
\frametitle{What if We Close or `move` the File Early?}
\begin{minted}{rust}
fn append_time_to_file(
    p: Path,
    n: String
) -> io::Result<()>
{
    let f = File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    send_file_to_other_function(f); // whoops!
    s.push_str(&n);
}
\end{minted}

Here we \mintinline{rust}{move} the file to another function for processing but still try to
append to it.
\end{frame}

\begin{frame}[fragile]
\frametitle{What if We Close or `move` the File Early?}
\begin{minted}{rust}
fn append_time_to_file(
    p: Path,
    n: String
) -> io::Result<()>
{
    let f = File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    send_file_to_other_function(f); // whoops!
    s.push_str(&n);
}
\end{minted}

\pause

Again, we will get a Rust compile time error. An owned file cannot be used
again after being used once (here, \mintinline{rust}{move}d to another function).
\end{frame}

\begin{frame}
\frametitle{Rust and Substructural Types}
Like Linear Haskell, the Rust type system combines linear (or affine) types
along with unrestricted types.
\begin{itemize}
    \item Linear Haskell provides flexible opt-in linearity \textit{on the
    function arrow}.
    \item Rust's system is a pervasive \textit{ownership} type system that
    includes \textbf{borrow} types.
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{Factors Influencing Rust's Implementation}
Understanding the goals of the Rust language helps to understand Rust's
substructural type system implementation.

\begin{itemize}
    \item Enable low-level systems programming.
    \item Automatic memory management without GC.
    \item Statically verified memory and thread safety.
    \item Static guarantee of exclusive mutability.
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{`copy` and `move` semantics}
For Rust owned types, \textit{unrestricted} types obey
\mintinline{rust}{copy} semantics and the linear/affine types obey
\mintinline{rust}{move} semantics.

\begin{itemize}
    \item Copy types are bit-copied on use and associated with primitive and
    stack-allocated data.
    \item non-Copy types are \mintinline{rust}{move}d on use and associated
    with heap-allocated data or data you want/need to be linear/affine.
    \item Custom types are \mintinline{rust}{move} by default, you must opt
    in to unrestricted types.
\end{itemize}
\end{frame}

\begin{frame}[fragile]
\frametitle{Linear/Affine types with `move` Semantics}
\begin{minted}{rust}
// Custom types are linear/affine by default.
struct OwnedInt(i32);

let n = OwnedInt(1);
take(n); // `n` is `move`d; ownership is transferred.
println!("Number: {}", n.0);
// Compiler error -- use of `move`d value!
\end{minted}
\end{frame}

\begin{frame}[fragile]
\frametitle{Unrestricted types with `copy` Semantics}
\begin{minted}{rust}
 // We can opt in to unrestricted types by implementing
 // the Copy trait (here, automatically derived).
 #[derive(Copy)]
 struct OwnedInt(i32);

 let n = OwnedInt(1);
 take(n); // data is bit-copied and sent to `take`.
 println!("Number: {}", n.0);
 // OK! `n` was not `move`d.
\end{minted}
\end{frame}

%\begin{frame}
%\frametitle{Rust Substructural Type Diagram}
%\begin{center}
%  \includegraphics[width=\textwidth]{figs/rust_ownership_types.pdf}
%\end{center}
%\end{frame}

\begin{frame}[fragile]
\frametitle{References in Rust -- Borrow Types}
Owned types alone can be limiting and inefficient.  As a low-level systems
language, Rust provides pass-by-reference types termed \textbf{borrow} types.

\begin{minted}{rust}
&'a T // Immutable borrow to T with lifetime `a`.
&'a mut T // Mutable borrow to T with lifetime `a`.
\end{minted}

Often the lifetime can be elided and figured out by the compiler.

Borrow types are part of the ownership system and must enforce the same
invariants

\end{frame}

\begin{frame}
\frametitle{References in Rust -- Borrow Types}
\begin{itemize}
    \item Just like pass-by-value types, there are \textit{unrestricted} and
    \textit{linear/affine} variants.
    \item An owned type may not be mutated or used when any reference exists.
    \item Any number of immutable (read-only) references may exist at once.
    \item Only one mutable reference may ever exist and it excludes any
    immutable references.
    \item Borrow types may not outlive their owned referent (enforced by
    \mintinline{rust}{borrowck} via lifetimes).
\end{itemize}
\end{frame}

\begin{frame}[fragile]
\frametitle{Unrestricted Immutable Borrow Types}
\begin{minted}{rust}
struct OwnedInt(i32);

let n = OwnedInt(1);
let r1 = &n;
let r2 = &n; // We can have unlimited immutable borrows.
take(n); // compiler error -- `n` moved with live refs.
\end{minted}
\end{frame}

\begin{frame}[fragile]
\frametitle{Unrestricted Immutable Borrow Types}
\begin{minted}{rust}
struct OwnedInt(i32);

let n = OwnedInt(1);
let r1 = &n;
let r2 = &n; // We can have unlimited immutable borrows.
use_ref(r1); // compiler error -- `r1` could outlive `n`.
\end{minted}
\end{frame}

\begin{frame}[fragile]
\frametitle{Unrestricted Immutable Borrow Types}
\begin{minted}{rust}
struct OwnedInt(i32);

let n = OwnedInt(1);
{
    let r = &n;
    take_ref(r1); // OK.
} // Delimited scope ensures `n` outlives `r`.
\end{minted}
\end{frame}

\begin{frame}[fragile]
\frametitle{Linear/Affine Mutable Borrow Types}
\begin{minted}{rust}
struct OwnedInt(i32);

let mut n = OwnedInt(1); // `n` must be mutable itself.
let mut_r = &mut n;
*mut_r.0 = 2;
\end{minted}
\end{frame}

\begin{frame}[fragile]
\frametitle{Linear/Affine Mutable Borrow Types}
\begin{minted}{rust}
struct OwnedInt(i32);

let mut n = OwnedInt(1); // `n` must be mutable itself.
let mut_r = &mut n;
take_mut_ref(r1); // mutable ref is moved.
*mut_r.0 = 2; // compiler error -- use of `move`d value.
\end{minted}
\end{frame}

\begin{frame}
\frametitle{Rust Substructural Type Diagram}
\begin{center}
  \includegraphics[width=\textwidth]{figs/rust_ownership_types2.pdf}
\end{center}
\end{frame}

\begin{frame}
\frametitle{Affine or Linear?}
Are those restricted types in Rust affine or linear?

\begin{itemize}
    \item \mintinline{rust}{move} semantics ensure an owned resource is
    used \textit{at most once}.
    \item But a resource is not required to be used \textbf{explicitly}
    \textit{at least} once.
    \item However, because of ownership tracking, the Rust compiler manually
    inserts \mintinline{rust}{drop} (deallocation calls) at compile time.
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{What do Linear/Affine Types Provide in Rust?}

Substructural typing is at the core of Rust's Ownership Type system, providing:

\begin{itemize}
    \item Statically ensure memory and thread safety without a GC.
    \item Statically ensure proper management (finalization) of resources
    (\textit{e.g.}, sockets, files, medical imaging scanners).
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Tradeoffs with Rust's Ownership Type System}
\begin{itemize}
    \item Strictly less flexible (expressive) than say C/C++.
    \item Some (\textit{e.g.}, self-referential) data structures are not
    possible.
    \item Safe code may be less efficient.
\end{itemize}

\pause

... but Rust provides some escape hatches:

\begin{itemize}
    \item `Arc` and `Rc` reference counting types for shared ownership that
    bypass \mintinline{rust}{move}.
    \item \mintinline{rust}{unsafe} blocks.
\end{itemize}

\end{frame}

\section{Conclusion}

\begin{frame}
\frametitle{Conclusion}
  Something about why linear types are cool, try it out, etc.
\end{frame}


\appendix

\begin{frame}

> mkPair :: a ->. b ->. (a, b)
> mkPair a b = (a, b)

> data Color = Red | Green | Blue deriving (Show, Eq)
> 
> colorf :: Color ->. Color ->. Color
> colorf  Red   q      =  q
> colorf  p     Green  =  p
> colorf  Blue  q      =  q


> -- More general version of sum
> combineL :: (Int ->. Int ->. Int) -> Int ->.  [Int] ->. Int
> combineL _ id_elem [] = id_elem
> combineL op id_elem (x:xs) = x `op` combineL op id_elem xs

\end{frame}

\end{document}
