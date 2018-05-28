%if False

> {-# LANGUAGE ScopedTypeVariables #-}
> module LinearTalk where
>

%endif

\documentclass{beamer}

%include lhs2TeX.fmt
%include lhs2TeX.sty
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

\frame{\titlepage}

\begin{frame}

> mkPair :: a ->. b ->. (a, b)
> mkPair a b = (a, b)

\pause

> data Color = Red | Green | Blue deriving (Show, Eq)
> 
> f :: Color ->. Color ->. Color
> f Red q = q
> f p Green = p
> f Blue q = q

\end{frame}

\end{document}
