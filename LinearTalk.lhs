%if False

> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE EmptyDataDecls         #-}
> {-# LANGUAGE RebindableSyntax       #-}
> {-# LANGUAGE RecordWildCards        #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE OverloadedStrings      #-}
>
> module LinearTalk where
> 
> import qualified Prelude.Linear as PL
> import qualified Prelude as P
> import Prelude hiding ((>>=), (>>))
> 
> import Num
>
> import qualified System.IO as SI
> import qualified System.IO.Resource as SIR
>
> import Data.Text (Text, pack)
> import Data.String (fromString)
>
> import Data.Time.Clock.POSIX

To the reader of this source

1. You are a pretty cool person
2. There are more examples and examples from the paper at the end of the
   file

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

%% The frownie face
\usepackage{wasysym}

\usepackage{minted}

%% Footnotes without an accompanying numerical binding.
\newcommand\blfootnote[1]{%
  \begingroup
  \renewcommand\thefootnote{}\footnote{#1}%
  \addtocounter{footnote}{-1}%
  \endgroup
}

\newcommand{\m}[1]{$#1$}

\newcommand{\novspacepause}{\vspace*{-\baselineskip}\pause}

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

%format +. = "+_{\!L} "
%format -. = "-_{\!L} "
%format *. = "*_{\!L} "
%format /. = "/_{\!L} "

%format f_1

\author{Ryan~Orendorff, Daniel~Hensley}
\title{Practical Introduction to Substructural Type Systems through
       Linear Haskell and Rust}
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

\vspace*{-0.7\baselineskip}
\vspace*{-\baselineskip}

< data FilePath 

\novspacepause

We can open and close files,

> openF    ::  FilePath -> IO File
> closeF   ::  File -> IO ()

\novspacepause

we can append to files,

> appendF  ::  File -> Text -> IO ()

\novspacepause

and we can get the current time as a string.

> now      ::  IO Text

%if False

> openF   = undefined
> appendF = undefined
> closeF  = undefined
> now     = (pack . show . round) `fmap` getPOSIXTime 
> {-# NOINLINE now #-}

%endif

\end{frame}


\begin{frame}{Simple IO program for accessing files}

Let's write a simple program for adding the current date to the end of the
file.

> appendTimeToFile :: FilePath -> IO ()
> appendTimeToFile path = do
>     f <- openF path 
>     n <- now
>     appendF f n
>     closeF f

%if False

>   where
>     (>>=) = (P.>>=)
>     (>>) = (P.>>)

%endif

\end{frame}


\begin{frame}{What if we close the file on accident}

What if we made a mistake and closed the file. Does the result still typecheck?

> appendTimeToFile' :: FilePath -> IO ()
> appendTimeToFile' path = do
>     f <- openF path 
>     n <- now
>     closeF f     
>     appendF f n  -- Oops, we closed the file

%if False

>   where
>     (>>=) = (P.>>=)
>     (>>) = (P.>>)

%endif

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

\end{frame}


\begin{frame}{Threading the resource}

Linear types and Linear Haskell can help us guarantee a value is used once.

< appendTimeToFile' :: FilePath -> IO ()
< appendTimeToFile' path = do
<     f <- openF path 
<     n <- now
<     f_1 <- appendF f n 
<     closeF f_1     

\pause

To get to this compiler-checked form, let's look a bit at how linear and
substructural types work.

\end{frame}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        Substructural Section                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Substructural Type Systems}

\begin{frame}{Unrestricted type systems have three structural rules}

In most type systems, three structural properties that allow unrestricted
use of a variable; unrestricted meaning variables can be dropped,
duplicated, and reordered. \cite{WalkerChapter:2005}

The substructural rules are 

\begin{itemize}
  \item Exchange,
  \item Contraction, and
  \item Weakening.
\end{itemize}


\end{frame}


\begin{frame}{Exchange: we can use type proofs in any order}

The exchange rule allows us to type check in any desired order when multiple
terms have to be checked at the same time.

% \begin{equation*}
%   Γ, x:A, y:B ⊢  t:T \text{, then } Γ, y:B, x:A ⊢  t:T
% \end{equation*}

\pause

For example, when type checking the following function

> exchange = let x = (4 :: Int) in let y = (5 :: Int) in (x, y)

We can check that |x| or |y| are |Int|s in either order.

\pause

Restricting this property means that we have to type check terms \emph{in
the opposite order they were introduced} (a \textsc{filo} order).

\end{frame}


\begin{frame}{Contraction: we can make duplicates of type proofs}

The contraction rule lets us use a proof of a variable's type twice.

% \begin{equation*}
%   Γ, x:A, x:A ⊢  t:T \text{, then } Γ, x:A ⊢  t:T
% \end{equation*}

\pause

For example, when type checking the following function

> dup :: a -> (a, a)
> dup x = (x, x)

We can use the proof that |x :: a| twice.

\pause

Restricting this property means we \emph{can't use a term more than once}.

\end{frame}


\begin{frame}{Weakening: we can discard unused type proofs}

The weakening rule means we can discard unnecessary type proofs.


% \begin{equation*}
%   Γ ⊢  t:T \text{, then } Γ, x:A ⊢  t:T
% \end{equation*}

\pause

For example, when type checking the following function

> kill :: a -> ()
> kill x = ()

We do not need to use the fact that |x :: a| while type checking the right
hand side.

\pause

Restricting this property means we \emph{must use a term at least once}.

\end{frame}


\begin{frame}{Substructural type systems remove 1 or more structural rule}

Substructural type systems remove or replace one or more of the structural
rules.

Let's look at the most useful substructural systems.

\end{frame}


\begin{frame}{Unrestricted type systems are most common}

You can use a variable as many times as you like, including zero.
\cite{WalkerChapter:2005}

This is the type system for Haskell, Java, C, etc.

\begin{center}
  \includegraphics[width=\textwidth]{figs/unrestricted.pdf}
\end{center}

\end{frame}


\begin{frame}{Relevant type systems: every variable used at least once}

In relevant typing systems, a variable \emph{must be used}. 
\cite{WalkerChapter:2005}

Drops the weakening rule.

\begin{center}
  \includegraphics[width=\textwidth]{figs/relevant.pdf}
\end{center}

\end{frame}


\begin{frame}{Affine type systems: every variable used at most once}

A variable can be used zero or one times. Drops the contraction rule.
\cite{WalkerChapter:2005}

Example languages include Alms and Rust.

\begin{center}
  \includegraphics[width=\textwidth]{figs/affine.pdf}
\end{center}

\end{frame}


\begin{frame}{Linear type systems: every variable used \emph{exactly} once}

A variable must be used \emph{exactly} once. Drops both the weakening and
contraction rules.
\cite{WalkerChapter:2005}

This type system is implemented in the Linear Haskell extension.

\begin{center}
  \includegraphics[width=\textwidth]{figs/linear.pdf}
\end{center}

\end{frame}


\begin{frame}{Ordered type systems: every variable must be used in order}

All variables must be used and must be used in \textsc{filo} order.
\cite{WalkerChapter:2005}

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

\pause 

% Remember, the use once property of linear type systems is what we wanted to
% handle resources in a type safe way.

\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               Linear Types by Linear Haskell Section                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Overview of Linear Types through Linear Haskell}

\begin{frame}{The |sum| function can be written linearly}

Say we want to take the sum of a list. How do we check that we consumed every
element exactly once? \cite{Bernardy:2017}
\blfootnote{"|->.|" is represented as "\verb|->.|" in source code}

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

\novspacepause
 
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

\novspacepause
 
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
  \item An atomic base type (|Int|, |Bool|, etc): evaluate the value once.
  \item A function: Pass in one argument and consume the result exactly
        once.
        \begin{itemize}[<+->]
          \item This is a bit tricky. It means, for |(f :: A ->. B)|, if |f x| 
                is used exactly once, \emph{then} |x| is used exactly once.
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

The linear arrow says how the function \emph{uses its argument}; it does not
restrict what is passed to the function.

\pause

> f :: s ->. t
> g :: s -> (t, t)
> g x = (f x, x)

%if False

> f = undefined

%endif

|g| does not specify how its argument is used; it could be aliased.

\end{frame}


\begin{frame}[fragile]{All datatypes are linear by default}

In Linear Haskell, all data constructors use linear arrows by default.

%format (Pair (a) (b)) = "( " a ", " b ") "
%format fst' = "\Varid{fst} "
%format fst'' = "\Varid{fst}_{\frownie} "

> data Pair a b where
>   Pair :: a ->. b ->. Pair a b

\novspacepause

So we can define the normal |fst| function and have it work as expected.

> fst' :: Pair a b -> a
> fst' (Pair a b) = a

\novspacepause

But a linear |fst| is not possible, as it requires a linear use of |b|.

< fst'' :: Pair a b ->. a
< fst'' (Pair a b) = a -- 'b' has weight 0 instead of 1

% \begin{Verbatim}[commandchars=\\\{\}]
% LinearTalk.lhs:572:16: error:
%     • \textcolor{red}{Couldn't match expected weight ‘1’ of variable ‘b’}
%       \textcolor{red}{with actual weight ‘0’}
% \end{Verbatim}


\pause

Linear datatypes work in a non-linear context; we do not need special data
constructors for linear versus nonlinear data.

\end{frame}


\begin{frame}{Unrestricted data}

You can define a term to have unlimited use by using the following data type.

> data Unrestricted a where
>   Unrestricted :: a -> Unrestricted a
> --  \quad \quad \quad \quad \quad \quad \ $\uparrow$ note the normal arrow!


This allows you to define functions like so

%format snd_L

> snd_L :: (Unrestricted a, b) ->. b
> snd_L ((Unrestricted a), b) = b

\end{frame}


\section{Motivation part 2: A (hopefully) better way}

%format SIR.RIO = "\Varid{IO}_L "
%format SIR.run = "\Varid{run}_L "
%format SIR.return = "\Varid{return}_L "

%format File_L = "\Varid{File} "

%format appendTimeToFile_L
%format openF_L
%format appendF_L
%format closeF_L

%format PL.Unrestricted = "\Varid{Unrestricted} "
%format P.>>= = >>= 


\begin{frame}{Linear File \textsc{api}}

%if False

> type File_L = SIR.Handle

%endif

Let's say we have the following \textsc{api} for accessing a resource (files).

< data File_L
< data FilePath 

\novspacepause

We can open and close files,

> openF_L    ::  FilePath -> SIR.RIO File_L
> closeF_L   ::  File_L ->. SIR.RIO (PL.Unrestricted ())

\novspacepause

we can append to files,

> appendF_L  ::  File_L ->. Text -> SIR.RIO File_L

\novspacepause

And we can get the current time as a string.

< now      ::  IO Text

%if False

> openF_L   = (\f -> SIR.openFile f SI.AppendMode)
> appendF_L = SIR.hPutStrLn
> closeF_L f = SIR.hClose f >> (SIR.return (PL.Unrestricted ()))
>     where
>         SIR.Builder {..} = SIR.builder


%endif

\end{frame}


\begin{frame}{We will also need a Linear IO Monad}

To glue this all together, we need a linear IO monad.

%format return_L
%format bind_L = >>= "_{L} "

\pause

|return| is nearly identical, but uses a linear arrow |->.|.

> return_L  :: a ->. SIR.RIO a

\novspacepause

And similarly |bind| is defined using linear arrows. 

> bind_L    :: SIR.RIO a ->. (a ->. SIR.RIO b) ->. SIR.RIO b
> 

\pause

The linear bind forces us to use the \emph{value linearly}, instead of
relying on the linear arrow for linearity.

% The linear bind continuation (|k :: (a ->. IO b) ->. IO b|) forces us to
% use the \emph{value linearly}, instead of relying on the linear arrow for
% linearity.

%if False

> return_L = undefined
> bind_L = undefined

%endif

\end{frame}


\begin{frame}{Appending time to a file part 2}

We can now take a crack at our file example again.

%{

%format do = "\mathrm{\mathbf{do}}_{L} "

> appendTimeToFile_L :: FilePath -> IO ()
> appendTimeToFile_L path = now P.>>= (\n -> SIR.run $ do
>     f <- openF_L path 
>     f_1 <- appendF_L f n
>     closeF_L f_1)

%}

%if False

>     where
>         -- The builder here is only for using @RebindableSyntax@ in this
>         -- monad.
>         SIR.Builder {..} = SIR.builder

%endif

\end{frame}


\begin{frame}[fragile]{Appending time to a file part 2}

If we forget to close the file, the compiler tells us about this error.

%format appendTimeToFile_L' = "\Varid{appendTimeToFile}_{L, \frownie} "

%{

%format do = "\mathrm{\mathbf{do}}_{L} "

%% This format is so that the syntax highlinting in my editor stays nice.

%format `dollar` = $

%if False

> dollar = ($)

%endif

< appendTimeToFile_L' :: FilePath -> IO ()
< appendTimeToFile_L' path = now P.>>= (\n -> SIR.run `dollar` do
<     f <- openF_L path 
<     f_1 <- appendF_L f n
<     SIR.return (PL.Unrestricted ()))

%}

%if False

<     where
<         -- The builder here is only for using @RebindableSyntax@ in this
<         -- monad.
<         SIR.Builder {..} = SIR.builder

%endif

\begin{Verbatim}[commandchars=\\\{\},codes={\catcode`$=3\catcode`^=7\catcode`_=8}]
LinearTalk.lhs:846:7: error:
    • \textcolor{red}{Couldn't match expected weight '1' of}
      \textcolor{red}{variable '$f_1$' with actual weight '0'}
\end{Verbatim}


\end{frame}


\section{Rust}

\begin{frame}
\frametitle{Rust}
Rust is a ``mainstream'' language that ships with a
substructural type system.\footnotemark

\footnotetext{https://www.rust-lang.org}
\end{frame}

\begin{frame}[fragile]
\frametitle{Using a File in Rust}
\begin{minted}{rust}
fn append_time(p: &Path, n: String) -> io::Result<()>
{
    let mut f = File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    s.push_str(&n);
    drop(f); // close the file
    Ok(())
}
\end{minted}
\end{frame}

\begin{frame}[fragile]
\frametitle{What if We Forgot to Close it?}
\begin{minted}{rust}
fn append_time(p: &Path, n: String) -> io::Result<()>
{
    let mut f = File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    s.push_str(&n);
    // <- no `drop`
    Ok(())
}
\end{minted}
\end{frame}

\begin{frame}
\frametitle{`drop` in Rust Ownership Type System}

The Rust compiler establishes and tracks
\textbf{ownership}.\footcites{clarke1998ownership,reed2015patina}

\begin{itemize}
    \item The compiler automatically inserts calls to \mintinline{rust}{drop}
    when an owned type with \mintinline{rust}{move} semantics goes out of
    scope.
    \item This provides automatic memory safety without GC.
    \item It also means that you cannot forget to ``finalize'' these
    resources (\textit{e.g}, files, sockets, locks, etc.).
\end{itemize}
\end{frame}

\begin{frame}[fragile]
\frametitle{Cannot Forget! The Compiler Inserts `drop`}
\begin{minted}{rust}
fn append_time(p: &Path, n: String) -> io::Result<()>
{
    let mut f = File::open(p)?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    s.push_str(&n);
    Ok(())
} // compiler `drop`s all resources of scope here
\end{minted}
\end{frame}

\begin{frame}[fragile]
\frametitle{What if We Close or `move` the File Early?}
\begin{minted}{rust}
fn append_time(p: &Path, n: String) -> io::Result<()>
{
    let mut f = File::open(p)?;
    let mut s = String::new();
    drop(f); // early drop
    f.read_to_string(&mut s)?; // invalid second use
    s.push_str(&n);
    Ok(())
}
\end{minted}

Here we accidentally close the file too early.

\pause

\begin{center}
  \includegraphics[width=\textwidth]{figs/early_drop_compiler_error.png}
\end{center}
\end{frame}

%No worries -- we will get a Rust compile time error. The file cannot be used
%again after being used once (here, \mintinline{rust}{drop}ped).
%\end{frame}

\begin{frame}[fragile]
\frametitle{What if We Close or `move` the File Early?}
\begin{minted}{rust}
fn append_time(p: &Path, n: String) -> io::Result<()>
{
    let mut f = File::open(p)?;
    let mut s = String::new();
    send_file_to_other_function(f); // whoops!
    f.read_to_string(&mut s)?; // compiler error here
    s.push_str(&n);
    Ok(())
}
\end{minted}

Here we \mintinline{rust}{move} the file early.

\pause

\begin{center}
  \includegraphics[width=\textwidth]{figs/move_then_use_compiler_error.png}
\end{center}

%Again, we get a Rust compile time error. The file cannot be used again after
%being used once (\mintinline{rust}{move}d to another function).

\end{frame}

\begin{frame}[fragile]
\frametitle{Linearly Threading (`move`) the File}
\begin{minted}{rust}
fn append_time(p: &Path, n: String) -> io::Result<()>
{
    let mut f = File::open(p)?;
    let mut s = String::new();
    let f_moved_back = process(f); // `f` moved back
    f_moved_back.read_to_string(&mut s)?;
    s.push_str(&n);
    Ok(())
}
\end{minted}
\end{frame}

\begin{frame}
\frametitle{Rust and Substructural Types}
Like Linear Haskell, the Rust type system supports both restricted types
(affine) and unrestricted types.
\begin{itemize}
    \item Linear Haskell provides flexible opt-in linearity \textit{on the
    function arrow}.
    \item Rust's system is a pervasive \textit{ownership} type system that
    includes \textit{borrow} types.\footcites{wadler1990linear,reed2015patina}
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{Factors Influencing Rust's Implementation}
To understand Rust's substructural type system implementation, it is helpful
to understand the goals of the Rust language.

\begin{itemize}
    \item Enable low-level systems programming.
    \item Automatic memory management without GC.
    \item Statically verified memory and thread safety (``fearless
    concurrency'').
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Flavor of Rust's Substructural Types}
For Rust owned types, \textit{unrestricted} types obey \textbf{copy semantics}
and the \textit{affine} types obey \textbf{move semantics} (all
non-Copy types).\footcite{jung2017rustbelt}

\begin{itemize}
    \item Move types are \mintinline{rust}{move}d on use and associated
    with heap-allocated data or data you want to be affine.
    \item Copy types are bit-copied on use and associated with primitive and
    stack-allocated data.
    \item Custom types are \mintinline{rust}{move} by default; you must opt
    in to unrestricted types.
\end{itemize}
\end{frame}

\begin{frame}[fragile]
\frametitle{Affine types with `move` Semantics}
\begin{minted}{rust}
// Custom types are affine by default.
struct AffineInt(i32);
fn take<T>(n: T) { }

let n = AffineInt(1);
take(n); // `n` is `move`d; ownership is transferred.
println!("Number: {}", n.0);
\end{minted}

\pause

\begin{center}
  \includegraphics[width=\textwidth]{figs/affine_int_compiler_error.png}
\end{center}
\end{frame}

\begin{frame}[fragile]
\frametitle{Unrestricted types with `copy` Semantics}
\begin{minted}{rust}
 // We can opt in to unrestricted types by implementing
 // the Copy trait (here, automatically derived).
 #[derive(Copy, Clone)]
 struct UnrestrictedInt(i32);

 let n = UnrestrictedInt(1);
 take(n); // data is bit-copied and sent to `take`.
 println!("Number: {}", n.0);
 // OK! `n` was not `move`d.
\end{minted}
\end{frame}

\begin{frame}
\frametitle{Rust Owned Types}
\begin{center}
  \includegraphics[width=\textwidth]{figs/rust_ownership_types3.pdf}
\end{center}
\end{frame}

\begin{frame}[fragile]
\frametitle{References in Rust -- Borrow Types}
Owned types alone can be limiting and inefficient.  As a low-level systems
language, Rust provides pass-by-reference types termed \textbf{borrow} types.
\footcites{jung2017rustbelt,fluet2006linear}

\begin{minted}{rust}
&'a T // Immutable borrow to T with lifetime `a`.
&'a mut T // Mutable borrow to T with lifetime `a`.
\end{minted}

\begin{itemize}
    \item Borrow types are part of the ownership system and must enforce the
    same invariants.
    \item Aliasing and mutability mutually excluded (static guarantee).
    \item Borrow types also have unrestricted and affine variants.
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{Rust Substructural Type Diagram: The Whole Story}
\begin{center}
  \includegraphics[width=\textwidth]{figs/rust_ownership_types2.pdf}
\end{center}
\end{frame}

%\begin{frame}
%\frametitle{Affine or Linear?}
%Are those restricted types in Rust affine or linear?
%
%\begin{itemize}
%    \item \mintinline{rust}{move} semantics ensure a resource is
%    used \textit{at most once}.
%    \item But it is not required to be \textbf{explicitly} used
%    \textit{at least} once.
%    \item However, because of ownership tracking, the Rust compiler manually
%    inserts \mintinline{rust}{drop} (deallocation calls) at compile time.
%\end{itemize}
%
%So which is it in the end? Do implicit \mintinline{rust}{drop}s constitute
%linearity?
%\end{frame}

%\begin{frame}
%\frametitle{What do Affine Types Provide in Rust?}
%
%Substructural typing is at the core of Rust's ownership type system. They
%are, along with \mintinline{rust}{borrowck}, what allow Rust to provide
%powerful static guarantees.
%
%\begin{itemize}
%    \item Automatic memory management without a GC.
%    \item Statically ensure memory and thread safety.
%    \item Statically ensure proper management (finalization) of resources
%    (\textit{e.g.}, sockets, files, mutexes).
%    \item Building block for other abstractions (session types).
%\end{itemize}
%
%\end{frame}
%
%\begin{frame}
%\frametitle{Tradeoffs with Rust's Ownership Type System}
%\begin{itemize}
%    \item Strictly less flexible (expressive) than say C/C++.
%    \item Some (\textit{e.g.}, self-referential) data structures are not
%    possible.
%    \item Safe code may be less efficient.
%\end{itemize}
%
%\pause
%
%... but Rust provides some escape hatches:
%
%\begin{itemize}
%    \item \mintinline{rust}{Arc} and \mintinline{rust}{Rc} reference
%    counting types for shared ownership that bypass \mintinline{rust}{move}.
%    \item \mintinline{rust}{unsafe} blocks.
%\end{itemize}
%
%\end{frame}

\section{Examples: Substructural Types and Medical Imaging}

\begin{frame}
\frametitle{Medical Imaging Software}
Compile-time guarantees are very attractive for software that controls
medical imaging scanners:

\begin{itemize}
    \item Software controls complex and dangerous hardware.
    \item Critical communication layers.
    \item Monitor and respond to hardware sensors.
    \item We must ensure patient safety and hardware integrity.
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Applications in Our Work at Magnetic Insight}
\begin{center}
  \includegraphics[width=0.8\textwidth]{figs/mpi_pulse_sequences.png}
\end{center}

We build and operate large imaging scanners.
\footcite{goodwill2011multidimenstional}

\begin{itemize}
    \item Power large magnets and motors with ``pulse sequences'' to
    take scans and acquire data.
    \item We then reconstruct images from 1 or more scans per our governing
    physics.
\end{itemize}
\end{frame}

%\begin{frame}
%\frametitle{Important Software-Related Tasks}
%
%\begin{itemize}
%    \item Pulse sequences designed and validated in software (control high
%    currents and power -- handle with care!).
%    \item Communication of data between real-time system directly 
%    controlling the magnets and ``control'' computer.
%    \item Hardware boot up/enable and shutdown sequences.
%    \item Monitoring system hardware (sensors) and taking appropriate
%    actions for invalid conditions.
%    \item Higher level routing of scanner, as a resource, in an
%    asynchronous task management system.
%\end{itemize}
%\end{frame}

\begin{frame}
\frametitle{Examples of how We Might Leverage Linear Types}
We believe many of the safety-critical aspects of our system can benefit from
substructural typing constructs (as well as more FP).

We'll briefly discuss how linear types are attractive for these sensitive
tasks.

\begin{itemize}
    \item \textbf{session types} (require linear typing).
    \item Encoding our scanner as a linear resource.
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{Session Types}

Session types extend the notion of types from describing \textit{data} to
describing \textit{protocols}.
\footcites{pucella2008haskell,jespersen2015session}

\begin{itemize}
    \item A session type formalizes sequencing and order of a protocol in the
    type system.
    \item Often discussed in the context of channel-based communication.
    \item Convert developer responsibility to compiler responsibility.
    \item Little or no runtime cost.
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Session Types}
You can implement session types with:\footcite{pucella2008haskell}

\begin{itemize}
    \item types and classes/traits (duality, sequencing)
    \item linear threading (linear types, indexed monads).
\end{itemize}

\pause

\begin{center}
  \includegraphics[width=0.75\textwidth]{figs/session_types_need_substructural.pdf}
\end{center}

\end{frame}


\begin{frame}
\frametitle{Session Types}

%format :!: = "\ :\mkern2mu !\mkern-3mu: \  "
%format :?: = "\ :\mkern-4mu ?\mkern-5mu: \  "
%format :+: = "\ :\mkern-2mu +\mkern-2mu: \  "
%format :&: = "\ :\mkern-2mu \&\mkern-2mu: \  "
%format s_dual = "\overline{\Varid{" s "}} "

Some of the basic building block types:

> data a :!: r -- tx `a` then continue with `r`
> data a :?: r -- rx `a` then continue with `r`
> data a :+: r -- Choose `r` or `s`
> data a :&: r -- Offer `r` or `s`
> data Eps     -- Protocol is depleted

%if False

> infixr 5 :!:
> infixr 5 :?:

%endif

\end{frame}

\begin{frame}
\frametitle{Session Types}

Communicating channels are implemented as dual session types:

> s :: Int :!: Bool :?: Eps -- Channel 1
> s_dual :: Int :?: Bool :!: Eps -- Channel 2

Why session types require linearity:

> data Channel a
> c :: Channel (Int :!: Eps)
> h :: Channel (Int :!: Eps) -> Channel (Int :!: Eps) -> (Int, Int)
> p = h c c -- `h` sends Int down both channels

Two |Int|s will be sent down the same channel, violating protocol.

%if False

> s = undefined
> s_dual = undefined
> chan = undefined
> c = undefined
> h = undefined

%endif 

\end{frame}

%\begin{frame}
%
%Standard communication protocols are not the only use for session types.
%
%Session types could be very helpful to statically guarantee proper
%hardware-software interactions in our system.
%
%\end{frame}

\begin{frame}
\frametitle{Possible Applications for Us}

\begin{itemize}
    \item Encoding of proper hardware boot up/shutdown sequences.
    \item Communication protocol between our real-time system (directly
    plays pulse sequences to hardware) and control computer.
    \item Protocol for dynamic pulse sequence modification based on real-time
    feedback.
    \item Pulse sequence generation API tool.
\end{itemize}

\end{frame}

\section{Conclusion}

\begin{frame}
\frametitle{Conclusion}
  Something about why linear types are cool, try it out, etc.
\end{frame}

% \appendix


%if False

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             More examples/looking at the Tweag examples             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Old Linear Haskell (0.1.6 docker) bug?  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This code was for testing a potential bug in an older version of Linear
Haskell. In the 0.1.6 docker container, |e| was considered a valid term even
though the |d| term passed to |j| was not the correct type.

< j :: (Int ->. Int) ->. Int ->. Int
< j f = \x -> f x +. 1

< d :: (Int -> Int)
< d = \x -> 3

< e = j d

< x :: (Int -> Int) ->. Int
< x f = f 5

< y :: (Int ->. Int)
< y i = i

< z = x y


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Array example from the paper  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> -- Linear Array interface taken from the paper
> data MArray a = MArray
> data Array a = Array

> newMArray :: Int -> (MArray a ->. PL.Unrestricted b) ->. b
> newMArray = error "newMArray not implemented"
>
> write :: MArray a ->. (Int, a) -> MArray a
> write = error "write not implemented"
>
> read :: MArray a ->. Int -> (MArray a, PL.Unrestricted a)
> read = error "read not implemented"
> 
> freeze :: MArray a ->. PL.Unrestricted (Array a)
> freeze = error "freeze not implemented"

It seems that the paper has two conflicting definitions of the linear
|foldl|. In section 2.2 of the paper, it mentions that |foldl| has the type
|(a ->. b ->. a) -> a ->. [b] ->. a|, although this same section seems to
use an incompatible input parameter (|write|). In section 2.6, foldl is
mentioned to have the type |(a :p-> b :q-> a) -> a :p-> [b] :q-> a|, which
does work out with the |write| of section 2.2. Below is the definition of
(with monomorphic multiplicity) |foldl| that seems to play nice with the
type checker, which matches section 2.6.

> foldlL :: (a ->. b -> a) -> a ->. [b] -> a
> foldlL _ i [] = i
> foldlL f i (x:xs) = foldlL f (f i x) xs

> -- The actual function that guarantees arrays are written correctly.
> array :: Int -> [(Int, a)] -> Array a
> array size pairs = newMArray size (\ma -> freeze (foldlL write ma pairs))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  firstLine example from the paper  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> firstLine :: FilePath -> IO Text
> firstLine filepath = SIR.run $ do
>     f <- SIR.openFile filepath SI.ReadMode
>     (line, f1) <- SIR.hGetLine f
>     SIR.hClose f1
>     SIR.return line
>     where
>         -- The builder here is only for using @RebindableSyntax@ in this
>         -- monad.
>         SIR.Builder {..} = SIR.builder


%%%%%%%%%%%%%%%%%%%%%%%%%
%  Tweag Color example  %
%%%%%%%%%%%%%%%%%%%%%%%%%

Here, we have a data type where evaluation simply means pattern matching
or passing it through to consume.

> data Color = Red | Green | Blue deriving (Show, Eq)
> 
> colorf :: Color ->. Color ->. Color
> colorf  Red   q      =  q
> colorf  p     Green  =  p
> colorf  Blue  q      =  q


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  More general version of sumL on Ints  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This is a higher order version of |sumL|, which takes in 

Should probably actually just do this with a fold. :-)

> -- More general version of sum
> combineL :: (Int ->. Int ->. Int) -> Int ->.  [Int] ->. Int
> combineL _ idElem [] = idElem
> combineL op idElem (x:xs) = x `op` combineL op idElem xs


%endif

\end{document}
