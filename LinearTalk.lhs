%if False

> {-# LANGUAGE ScopedTypeVariables #-}
> module LinearTalk where
> import Unsafe.Linear as UL
>

%endif

\documentclass{beamer}

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

\usepackage[colorinlistoftodos,prependcaption,textsize=tiny]{todonotes}

\author{Ryan~Orendorff, Daniel~Hensley}
\title{Introduction to Linear Type Systems}
\subtitle{\verb|github.com/ryanorendorff/???????|}
\date{June 2nd, 2018}
\hypersetup{pdflang={English}}

\institute{
  Magnetic Insight
}

\begin{document}

%format +. = "+_{\!1} "
%format -. = "-_{1} "
%format *. = "*_{1} "
%format /. = "/_{1} "

%if False

> ------------------------------------------------------------------------
> --                    Poor man's linear Num class                     --
> ------------------------------------------------------------------------
> 
> (+.) :: Num a => a ->. a ->. a
> (+.) = UL.toLinear2 (+)
> 
> (-.) :: Num a => a ->. a ->. a
> (-.) = UL.toLinear2 (-)
> 
> (*.) :: Num a => a ->. a ->. a
> (*.) = UL.toLinear2 (*)
> 
> (/.) :: Fractional a => a ->. a ->. a
> (/.) = UL.toLinear2 (/)
> 
> infixl 6 +.
> infixl 6 -.
> infixl 7 *.
> infixl 7 /.

%endif

\frame{\titlepage}


\begin{frame}
\frametitle{Outline}
\tableofcontents[]
\end{frame}

% For the first section, do not highlight the current section (overview slide)
\AtBeginSection[] {}
\section{Motivation: ???}

% For all subsequent sections, highlight what section we are currently in.
\AtBeginSection[]
{
  \begin{frame}
    \frametitle{Table of Contents}
    \tableofcontents[currentsection]
  \end{frame}
}

\section{Types of Substructural Type Systems}

\begin{frame}
\frametitle{Substructural Relation}
  Figure here
\end{frame}

\section{Overview of Linear Types through Linear Haskell}

\begin{frame}
\frametitle{Arrow representation}
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


\end{frame}

\end{document}
