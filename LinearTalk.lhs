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

\usepackage{mathpartir}
\usepackage{fontspec}
\usepackage{cmll}
\usepackage{unicode-math}
\usepackage[plain]{fancyref}

\usepackage{minted}

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

Let's say we have the following \textsc{api} for accessing files.

> data File
> data FilePath 

> openF    ::  FilePath -> IO File
> closeF   ::  File -> IO ()

\pause

> readF    ::  File -> IO String
> appendF  ::  File -> String -> IO ()

\pause

> now      ::  IO String

%if False

> openF   = undefined
> readF   = undefined
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
>   input_data <- readF f
>   n <- now
>   appendF f n
>   closeF f

\end{frame}


\begin{frame}{What if we close the file on accident}

What if we made a mistake and closed the file. Does the result still typecheck?

> appendTimeToFile' :: FilePath -> IO ()
> appendTimeToFile' path = do
>   f <- openF path 
>   input_data <- readF f
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

The structural rules are

\begin{itemize}[<+->]
  \item Exchange: We can use terms in any order while type checking.
  \item Contraction:  We can duplicate proofs while type checking.
  \item Weakening: We can throw away unnecessary proofs while type checking.
\end{itemize}

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


\begin{frame}{Substructural type system relation}

This leads to a convenient diagram describing how the substructural systems
relate to each other.

\begin{center}
  \includegraphics[width=\textwidth]{figs/substructural_linear_hi.pdf}
\end{center}

We will be focusing on linear types mostly in this talk.

\end{frame}

\section{Overview of Linear Types through Linear Haskell}


\begin{frame}{What constitutes a use?}

\begin{itemize}
  \item An atomic base type: evaluate the value once
  \item A function: Pass in one argument and consume the result exactly
        once.
        \begin{itemize}
          \item This is a bit tricky. It means if |(f :: A ->. B) u| is used
                exactly once, \emph{then} |u| is used exactly once.
        \end{itemize}
  \item For any ADT, pattern match and consume all components exactly once. For
        a pair, this means pattern match and consume each component exactly once.
\end{itemize}

\end{frame}


\begin{frame}{Arrow representation}
  \verb|->.| is the representation of |->.|.
\end{frame}

\section{Two examples using Linear Types}

\begin{frame}
\frametitle{Examples using Linear Types}
  \begin{itemize}
    \item File IO or array example
    \item Scanners galore!
  \end{itemize}
\end{frame}


\section{Other ways of solving the similar problems}

\begin{frame}
  Also called "the competition".
\end{frame}

\subsection{Idris and Uniqueness/Dependent Types}

\begin{frame}
\frametitle{Idris}
  Idris is cool, I have heard.
\end{frame}

\subsection{Indexed Monads}

\begin{frame}
\frametitle{Indexed Monads}
  Very basic dependently type-ish thing! Who got state in my type system?
\end{frame}

\subsection{Rust}

\begin{frame}
\frametitle{Rust}
  Daniel explodes here.
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
\frametitle{`drop` in Rust}
As part of its ownership system, the Rust compiler tracks all ``owned''
resources in a program and automatically inserts calls to `drop` when a type
goes out of scope.

This provides automatic memory safety without GC. It also means that you
cannot forget to ``finalize'' resources such as files, sockets, etc.
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
    drop(f) // early drop
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
    drop(f) // early drop!
    s.push_str(&n);
}
\end{minted}

No worries -- we will get a Rust compile time error. An owned file cannot be
used again after being used once (here, `drop`ped).
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

Here we `move` the file to another function for processing but still try to
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

Again, we will get a Rust compile time error. An owned file cannot be used
again after being used once (here, `move`d to another function).
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
> f :: Color ->. Color ->. Color
> f  Red   q      =  q
> f  p     Green  =  p
> f  Blue  q      =  q

> g :: Int ->. Int ->. Int
> g x y = x +. y

> sumL :: Num a => [a] -> a
> sumL [] = 0
> sumL (x:xs) = x +. sumL xs


\end{frame}

\end{document}
