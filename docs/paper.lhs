\documentclass[a4paper,journal,compsoc]{IEEEtran}

% \usepackage{float}
\usepackage{mathtools}
\usepackage{hyperref}
\usepackage{microtype}
\usepackage{trfrac}
\usepackage{multirow}
\usepackage{textcomp}
% \usepackage{caption}
% \usepackage[main=english,russian]{babel}

% \selectlanguage{english}

% \sectionsAreChapters


%include polycode.fmt

%format forall = "\forall"
%format a = "\alpha"
%format a'
%format b = "\beta"
%format b'
%format c = "\gamma"
%format c'
%format d = "\delta"
%format d'
%format s = "\sigma"
%format m = "\mu"
%format r = "\psi"
%format x = "\eta"
%format x'
%format y = "\zeta"
%format y'
%format ^ =  " "
%format ^^ =  "\;"
%format !!  = "{\;}''\!\!"
%format <> = "\diamond"

% \floatstyle{boxed}
% \newfloat{figure}{hbp}{lst}

\newcommand{\parr}[1]{\medskip\textbf{#1}\hspace{0.6cm}}
\newcommand{\strOrNum}{(\text{String} || \text{Number})}


% \year{2021}
\title{Static analyzer for the Nix Expression Language}
% \author{Student, BSE174}{I. I. Kostyuchenko}
% \academicTeacher{Associate Professor, PhD}{V. V. Kuliamin}

% \documentTitle{Explanatory note}
% \documentCode{RU.17701729.04.05-01 ПЗ 01-1}

\author{
  \IEEEauthorblockN{Kostyuchenko Ilya}
  \IEEEauthorblockA{
    Faculty of Computer Science,\\
    Higher School of Economics,\\
    Moscow, Russia
  }
}

\begin{document}
\maketitle
\thispagestyle{plain}
\pagestyle{plain}

\begin{abstract}
  Configuring and building even the most straightforward software projects is often not simple -- it requires downloading and installing copious amounts of prerequisite software. This often makes reproducing builds of a project on different machines problematic. The Nix package manager aims to address this problem by providing a unified language for describing software packages in a purely functional way. This language is called the Nix Expression Language. Since all of the complexity of software configuration needs to be expressed in the Nix Expression Language, the expressions themselves often become quite complicated, making it difficult to understand and extend existing expressions without introducing errors. A widespread tool for easing the understandability and correctness of expressions in other languages is static type checking. This paper will explore the techniques that can be used to add static type checking to the Nix Expression Language.
\end{abstract}

% \begin{otherlanguage}{russian}

%   \begin{abstract}
%     Настройка и сборка даже самых простых программных проектов часто бывает непростой задачей -- для этого требуется загрузить и установить большое количество необходимого программного обеспечения. Это часто делает проблематичным воспроизведение сборок проекта на разных компьютерах. Менеджер пакетов Nix стремится решить эту проблему, предоставляя единый язык для описания пакетов программного обеспечения чисто функциональным способом. Этот язык называется языком Nix Expression Language. Поскольку вся сложность конфигурации программного обеспечения должна быть выражена на языке Nix Expression Language, сами выражения часто становятся довольно сложными, что затрудняет понимание и расширение существующих выражений без ошибок. Широко распространенным инструментом для облегчения понимания и проверки корректности выражений на других языках является статическая проверка типов. В этой статье будут рассмотрены методы, которые можно использовать для добавления статической проверки типов в язык выражений Nix Expression Language.
%   \end{abstract}

% \end{otherlanguage}

% \parr{Source code} \url{https://github.com/ilyakooo0/tix}

% \newpage

% \tableofcontents

\section*{Introduction}

Today developing software is riddled with large amounts of complexity -- newer software is built on top of older software, dragging the whole stack below as dependencies. As a result, building software requires a large number of dependencies to be preinstalled. Modern build systems like Gradle\footnote{\url{https://gradle.org}}, Maven\footnote{\url{https://maven.apache.org}}, and Stack\footnote{\url{https://docs.haskellstack.org/en/stable/README/}} try to mitigate the issue by automatically downloading and installing the required dependencies. However, in most cases, such tools can only manage language-level dependencies, requiring the user to install additional libraries and tools manually.

Such manual dependency management leads to several problems. Firstly, when a user wants to work on a project he has not encountered before, he will have to install the required dependencies, leading to additional tedious work. Furthermore, the required versions of project dependencies might conflict with the versions that another project requires. The user will have to manually switch the versions of executables and libraries when switching between projects.

This problem is partially solved by so-called virtual environment tools. A virtual environment is a way of restricting and customizing the available external tools (usually compilers) with the ability to dynamically switch between them. These tools can generally be split into one of two categories:

\begin{enumerate}
  \item \emph{language-specific tools} which simplify installing and switching between different versions of some tools (usually compilers). Examples of such tools are:
        \begin{enumerate}
          \item |rustup|\footnote{\url{https://github.com/rust-lang/rustup}} for Rust tools
          \item |ghcup|\footnote{\url{https://www.haskell.org/ghcup/}} for Haskell tools
          \item |Ruby Version Manager|\footnote{\url{http://rvm.io}} for Ruby tools
        \end{enumerate}
  \item \emph{general-purpose tools} which automate switching the available tools (or versions of tools) that have been previously installed in some other way:
        \begin{enumerate}
          \item |direnv|\footnote{\url{https://direnv.net}}
          \item |autoenv|\footnote{\url{https://github.com/inishchith/autoenv}}
          \item |asdf|\footnote{\url{https://github.com/asdf-vm/asdf}}
          \item |ondir|\footnote{\url{https://github.com/alecthomas/ondir}}
        \end{enumerate}
\end{enumerate}

Even so, such tools still require explicit user action: the user needs to explicitly switch between (or install) the necessary tools when switching projects in the case of  \emph{language-specific tools}, the user needs to explicitly install the required tools in some other way for them to be imported by \emph{general-purpose tools}.

Secondly, manual dependency management impedes the reproducibility of the project builds -- building the same project on multiple machines will, in all likelihood, produce different results since the machines will likely differ in many ways that impact the build, such as having slightly different versions\footnote{``Version'' here refers to actual variations on the bit level, and not just version numbers.} of libraries and executables.

Nix~\cite{dolstra2008nixos} attempts to solve the problems mentioned above by describing software project builds in a generic and language-agnostic way. Furthermore, the resulting descriptions define the build process in a complete and deterministic way -- everything that impacts the build in any way needs to be explicitly defined, even system-level libraries are reproducible on the bit level.

Defining the build process in such a detailed and pedantic manner is not generally easy and often requires large amounts of boilerplate, which provides much room for error. While most build systems define project builds with static configurations using formats such as YAML~\cite{ben2009yaml}, Nix employs a fully-fledged programming language as its configuration language. This programming language is called the Nix Expression Language\footnote{\url{https://nixos.wiki/wiki/Nix_Expression_Language}}.

Using a fully-fledged programming language allows the users to reuse common definitions and create new domain-specific abstractions. This helps to mitigate a lot of the aforementioned potential problems.

Nix Expression Language is a dynamic, lazy, purely functional programming language. The purely functional nature of the language aids in build reproducibility guarantees that Nix aims to provide.

Even considering the tremendous benefits that using a programming language for build configuration brings, the dynamic nature of the language often still makes it challenging to understand and reuse existing code. This is in part due to the large amounts of code required to describe real-world systems in all of their complexity.\footnote{The standard Nix package repository |nixpkgs| contains 2 million lines of code.}

Refactoring existing code is also tricky since the generated expressions are only really checked for correctness\footnote{Here, ``correctness'' is used to mean the absence of evaluation-time errors.} at their evaluation time, and the evaluation time of expressions is difficult to predict due to the lazy nature of the language.

A widespread tool for easing development and alleviate the aforementioned problems in other languages is static type checking -- the process of analyzing the types of expressions and detecting type errors in programs without evaluating them.

Even though static type checking in most mainstream programming languages does not influence the semantics of the programs, it is nonetheless widely used. This is an indication of the usefulness of static type checking during development. Type checking helps detect likely ``incorrect'' programs without evaluating them (very helpful during refactoring), and aids in understanding existing code since types often expose some semantics of expressions (this is especially true for purely functional programs).

The Nix Expression Language does not currently support static type checking of any kind. Of course, retrofitting a static type checker onto an existing dynamic programming language will reject programs that will not produce errors at runtime. However, this is still a goal worth pursuing because even with all of the situations when the type checker produces false negatives, inferring types for existing expressions, as mentioned above, will aid in understanding existing code and surface potentially problematic areas of code to the developer.

\section{Nix Expression Language}

In this chapter we will examine the semantics of major terms of the Nix Expression Language. We will from now on abbreviate \emph{Nix Expression Language} as \emph{Nix} as it is in most cases clear from the context what is being referred to: the language or the system as a whole.

\subsection{Atomic types}

Atomic types are described in table~\ref{tab:atomicTypes}.

\begin{table}[hbp]
  \centering
  \caption{Table of atomic Nix Expression Language types}
  \begin{tabular}{ ||c||p{2cm}||p{4cm}|| } \hline
    \textbf{Atomic type} & \textbf{Description}                                                                      & \textbf{Example terms}                 \\ \hline \hline
    URI                  & A string literal representing a URI. Does not require quoting.                            & \texttt{https://example.com}           \\ \hline
    Path literal         & A path literal referring to either a relative or absolute path. Does not require quoting. & \texttt{../../directory/file.txt}      \\ \hline
    Integer              & The current C implementation of Nix allows values of 64-bit signed integers.              & \texttt{69} \newline \texttt{-42}      \\ \hline
    Float                & Floating point numbers.                                                                   & \texttt{42.9} \newline \texttt{-69.42} \\ \hline
    Bool                 & Boolean values.                                                                           & \texttt{true} \newline \texttt{false}  \\ \hline
    Null                 & A type with a single value.                                                               & \texttt{null}                          \\ \hline
  \end{tabular}
  \label{tab:atomicTypes}
\end{table}

\subsection{Strings}

\subsubsection{Double quoted string literals}

Double quoted string literals are string literals just like in many other programming languages. It can contain esaped newline character literals \texttt{\textbackslash n} which will be preserved as shown in figure~\ref{lst:newlineString}.

\begin{figure}
  \begin{minipage}{\textwidth}
    \begin{verbatim}
"Sampel\nPalnet"
    \end{verbatim}
  \end{minipage}
  \caption{Nix Expression Language double quoted string literal example}
  \label{lst:newlineString}
\end{figure}

\subsubsection{Multiline string literals}

Multiline string literals are enclosed by pairs of single quotes (\texttt{''} as opposed to \texttt{"}). Newlines are preserved and indentation is stripped from the beginning of the literal. An example multiline string literal is shown in figure~\ref{lst:multilineString}.

\begin{figure}
  \begin{minipage}{\textwidth}
    \begin{verbatim}
''
Sampel
Palnet''

"Sampel\nPalnet"
    \end{verbatim}
  \end{minipage}
  \caption{Nix Expression Language multiline string literal example and an equivalent double quoted string literal}
  \label{lst:multilineString}
\end{figure}

% \subsubsection{String interpolation}

% The Nix Expression Language also allows for arbitrary expressions of the language to be interpolated in string using the following syntax: \texttt{\$\{ <expression> \}} where \texttt{<expression>} is a Nix expression.An example of string interpolation is show in figure~\ref{lst:stringInterpolation}.

% \begin{figure}
%   \begin{minipage}{\textwidth}
%     \begin{verbatim}
% "one is ${ if 1 == 2 then "equal" else "not equal" } to two."

% "one is not equal to two."
%     \end{verbatim}
%   \end{minipage}
%   \caption{Nix Expression Language string interpolation example with the resulting string.}
%   \label{lst:stringInterpolation}
% \end{figure}

\subsection{Lists}

Nix supports a list type. List literals are enclosed by square brackets \texttt{[]} and the elements are separated by whitespace. Examples of list literals are shown in figure~\ref{lst:list}.

\begin{figure}
  \begin{minipage}{\textwidth}
    \begin{verbatim}
[1 2 "hello" ../../file.txt]
    \end{verbatim}
  \end{minipage}
  \caption{Nix Expression Language List literals.}
  \label{lst:list}
\end{figure}

\subsection{Attribute sets} \label{sec:attrSet}

An attribute set is internally implemented as a hashmap with strings as keys where values can be arbitrary expressions. An example attribute set literal is shown in figure~\ref{lst:attrSet}.

\begin{figure}
  \begin{minipage}{\textwidth}
    \begin{verbatim}
{ a = 42;
  b = 6 + 9;
  c = ../file.txt;
  d = "henlo, world";
}
    \end{verbatim}
  \end{minipage}
  \caption{Nix Expression Language attribute set literals.}
  \label{lst:attrSet}
\end{figure}

Fields (keys) of an attribute set can be accessed through the \emph{dot} (\texttt{.}) operator as shown in figure~\ref{lst:attrSetAccess}. Accessing an absent field yields a runtime error.

\begin{figure}
  \begin{minipage}{\textwidth}
    \begin{verbatim}
{ a = 42;
  b = 6 + 9;
}.b

15
    \end{verbatim}
  \end{minipage}
  \caption{Nix Expression Language attribute set field accessing.}
  \label{lst:attrSetAccess}
\end{figure}

\subsection{Attribute set updates} \label{sec:update}

There is a binary \emph{update} operator that takes two \emph{attribute sets} as operands and ``merges'' them in a shallow manner, preferring values from the right operand. The operator is written as two forward slashes \texttt{//}. This has the effect of updating the left operand with values from the right operand. An example is shown in figure~\ref{lst:update}.

\begin{figure}
  \begin{minipage}{\textwidth}
    \begin{verbatim}
{x = {a = 1;}; y = 2;}
  // {x = "hello"; z = 1.5;}

{ x = "hello"; y = 2; z = 1.5; }
    \end{verbatim}
  \end{minipage}
  \caption{Nix Expression Language attribute set updates.}
  \label{lst:update}
\end{figure}

\subsection{Recursive attribute sets} \label{sec:recAttrSet}

While ordinary \emph{attribute sets} discussed in section~\ref{sec:attrSet} can only reference values defined outside of the attribute set literal, \emph{recursive attribute sets} can also reference values defined in the attribute set literal itself. The keys of the attribute set are available in the scopes of the attribute set values. An example \emph{recursive attribute set} is given in figure~\ref{lst:recAttrSet}. Note that it is syntactically distinguished form normal \emph{attribute set} literals by the \texttt{rec} key word at the beginning.

\begin{figure}
  \begin{minipage}{\textwidth}
    \begin{verbatim}
rec {
  a = 42;
  b = a + 9;
  c = world;
  d = "henlo, ${c}";
}
    \end{verbatim}
  \end{minipage}
  \caption{Nix Expression Language recursive attribute set literals.}
  \label{lst:recAttrSet}
\end{figure}

\subsection{Let expressions} \label{sec:let}

A \emph{let expression} has two parts: \emph{declarations} and the \emph{body}. The format can be roughly described like so: \texttt{let <definitions> in <body>}. A \emph{let expression} allows us to define values in the \emph{declarations} that can be reused in the \emph{body} of the expression. Just like \emph{recursive attribute sets} from section~\ref{sec:recAttrSet} all declared values are available in scope in the defined expressions. An example of \emph{let expressions} is available in figure~\ref{lst:let}.

\begin{figure}
  \begin{minipage}{\textwidth}
    \begin{verbatim}
let
  c = world;
  d = "henlo, ${c}";
in d
    \end{verbatim}
  \end{minipage}
  \caption{Nix Expression Language \emph{let} expression literals.}
  \label{lst:let}
\end{figure}

\subsection{With expression} \label{sec:withExp}

\emph{With expressions} are a way of bringing all values from an \emph{attribute set} into scope. It is conceptually very similar to \emph{let expressions} discussed in section~\ref{sec:let}. The syntax can be roughly describe like so: \texttt{with <set>; <body>} where \texttt{<set>} is an expression which evaluates to an \emph{attribute set} and all values from the set are available as definitions in the \texttt{<body>} expression. An example of a \emph{with expression} can be found in figure~\ref{lst:with}.

\begin{figure}
  \begin{minipage}{\textwidth}
    \begin{verbatim}
let
  c = world;
  d = "henlo, ${c}";
in d
    \end{verbatim}
  \end{minipage}
  \caption{Nix Expression Language \emph{with} expression literals.}
  \label{lst:with}
\end{figure}

\section{Existing typecheckers}

In this section we will examine existing attempts at developing a typechecker for the Nix Expression Language, determine the features of the implemented type systems and assess how well maintained the implementations are.

\subsection{regnat/tix} \label{sec:regnat/tix}

regnat/tix\footnote{\url{https://github.com/regnat/tix}} is an implementation of a typechecker for the \emph{Nix Expression Language} implemented in \emph{OCaml}\footnote{\url{https://ocaml.org}}. The type checker itself is implemented on a different much simpler language named \emph{Nix Light}. To typecheck a normal \emph{Nix} program(expression) the program need to be first converted to an equivalent \emph{Nix Light} program. The type checker has a row-polymorphism-inspired feature: records are represented as a product of one-row types. In \emph{Nix Light} lists are represented as a single-linked heterogeneous list. The type system does not have support for parametric polymorphism in favour of a gradual typing paradigm.

\parr{Status} The last commit was published in 2017 and the project is now abandoned in favour of regnat/ptyx which we examin in section~\ref{sec:regnat/ptyx}.

\subsection{regnat/ptyx} \label{sec:regnat/ptyx}

regnat/ptyx\footnote{\url{https://github.com/regnat/ptyx}} is in essence a reimplementation of regnat/tix in Haskell\footnote{\url{https://www.haskell.org}} which we examined in section~\ref{sec:regnat/tix}. The basic ideas and techniques are the same. In fact, so much so, that the simpler language is also called \emph{Nix Light} and \emph{Nix} expressions are also converted to it before type checking. There is still no support for parametric polymorphism.

\parr{Status} The last commit was published in 2018. Unsupported language features include lists and attribute sets.

\subsection{haskell-nix/hnix}

haskell-nix/hnix\footnote{\url{https://github.com/haskell-nix/hnix}} is primarily not a typechecker, but rather the goal of the project is to fully reimplement the \emph{Nix Expression Language} interpreter in \emph{Haskell}. There is, however, a number of modules dedicated to a typechecker implementation which was migrated into the project. Contrary to how it may seem, the typechecker is not integrated with the rest of the project in any significant way. The type system itself is based on the Hindley-Milner type system. However, the type system was not extended with additional features to better support the dynamic nature of \emph{Nix}. For instance, there is no support for row-polymorphism which greatly impedes typechecking code that interacts with \emph{attribute sets} in any way.

\parr{Status} Last time the typechecker was modified in any substantial way was in 2019. It does not appear to be actively developed. Furthermore, there is no description of the implemented type system.


\section{Existing Approaches}

In this section we will explore existing techniques used to develop typecheckers, and explore how they can be applicable to our implementation.

\subsection{Type System Approach}

There has been much research done in the field of type systems for functional programming languages. The most notable contribution was the introduction of the Damas-Milner\footnote{The Damas-Milner type system is sometimes referred to as the Hindley–Milner type system.} type system and the W inference algorithm~\cite{damas1982principal}, which was later proven to be sound and complete~\cite{vaughanproof}.

The Damas-Milner type system is a type system for lambda calculus with parametric polymorphism. The most notable property of the Damas-Milner type system is the proven ability of the W algorithm to infer the most general types without any user-provided type annotations.

While the Damas-Milner describes a simple language, it is robust enough to be used as the basis for more advanced type systems such as the one used in the Haskell programming language~\cite{jones1999typing}, which extends the Damas-Milner type system in many ways which break the excellent properties it provides but remains extremely useful in practice.

Since the Nix Expression Language is a relatively simple functional programming language, extending the Damas-Milner type system and algorithms to fit the language will be the approach we take as the tried and true method.

\subsection{Type Checker Error Handling}

Due to the inevitable presence of false negatives during type checking, for the type checker to be useful enough to be worth implementing, the algorithm should gracefully recover after encountering expressions that it deems to be ``wrong''.

Since the interpreter of the programs will not be related to the developed type checker in any way, the type checker can not influence the semantics of the programs. This fact can be used to implement the following failure recovery heuristic: when the type checker encounters an expression that yields type checking errors, the errors are collected to be displayed to the user, and the expression is given the least general type possible such that it does not yield type errors.

This is, in general, always possible since any syntactically valid expression will be correct with respect to types if every subexpression is assigned the polymorphic type:

\begin{equation}
  \forall \alpha. \alpha
\end{equation}

In some cases, this approach will likely yield too little information to be useful, but it seems like a reasonable solution.

\subsection{Improving Type Checker Error Reporting}

The original W inference algorithm is a single-stage inference algorithm, meaning that it attempts to satisfy type constraints as soon as it encounters them. A consequence of this is that errors are reported as soon as they are encountered. Often, the information available about the program at the time the error had occurred is very limited due to not all of the program being processed. This leads to poor error reporting.

In contrast to the single-stage algorithm, the two-stage algorithm~\cite{jones2005practical,serrano2016type} collects the type constraints as the first stage of type checking and solves the collected constraints as a second step. This leads to most errors occurring during the second stage when all available type information has already been collected. As a result, better type errors are produced. This is the more preferable approach for our goals.

\subsection{Row-Polymorphism}

One of the language-level constructs in the Nix Expression Language is the ``attribute set'' which can be thought of as a first-class hashmap with strings as keys. In combination with the dynamic nature of the language, these ``attribute sets'' have the semantics of row-polymorphic records.

There has been some research conducted in the field of type systems with row-polymorphic records~\cite{morris2019abstracting,leijen2005extensible}. The approach that should be taken in developing a static type checker for the Nix Expression Language is describing row-polymorphic attribute sets with a construct similar\footnote{The way it should be displayed to the user will be distinct, since, unlike in Haskell, the Nix Expression Language does not have constraints as a language construct.} to Haskell constraints~\cite{orchard2010haskell}.

% \subsection{Message Reporting}

% Displaying errors to the user is not as simple as it might initially seem. For the type checker to be as helpful as possible, error messages should display the encountered issues in the language contexts in which they had occurred. Ideally, the errors should point to the exact terms that are most likely problematic.

% Even if all of the information necessary for detailed error messages is collected during type checking, displaying errors to the user is a problem that deserves its own research\footnote{This also relates to how types are laid out when displaying to the user even when no errors have occurred.}. The users might have different screen sizes. The error message should reflow all of the displayed text, code, and type annotations to accommodate the available space.

% Hughes introduced a general-purpose algebraic pretty-printer~\cite{hughes1995design}, which was later improved upon~\cite{wadler2003prettier}, to solve precisely this problem. Using one of the pretty-printers based on that research is the industry standard and will be used by our type checker.

\section{Type system}

As mentioned above, we have opted to use the Damas-Milner type system as a basis for our implementation. Our goal is not to just add types for the sake of types, but give additional reassurance to the developer, and reject as many potentially ``invalid'' programs as possible. If the developed typechecker processes a program without errors, then the program should have no type errors during execution.

\subsection{Polymorphism} \label{sec:polymorphism}

Dynamic languages have potentially limitless polymorphism – you can pass anything of any type to any function and it can potentially work without errors – it is in principal possible to check the ``types'' of passed values at runtime. This leads to the fact that there is really no way to know which ``types'' are acceptable in which positions without executing the program.

The Nix community, just like with any other language, has developed idiomatic ways of solving problems. Some of these idioms are heavily based on polymorphic behavior at runtime. We should strive to develop a type system which supports as much of idiomatic behavior as possible (and where not possible, similar behavior should be expressible within the type system).

Parametric polymorphism has proven to strike a nice balance between strictness, expressiveness and type inference. We feel it will be most useful as a type system for \emph{Nix}.

Inferring types with parametric polymorphism implies generalizing the inferred types at certain points in the syntax tree. Generalization is also known as ``closing over type variable'' – it can be though of as explicitly choosing a point where the variable is quantified over. Figure~\ref{fig:closing} is an example of closing over a type variable.

Traditionally in languages similar to \emph{simply-typed lambda calculus} generalization is performed in \emph{let} declarations – so-called \emph{let-generalization}. \emph{Nix} has a very similar \emph{let} construct as discussed in section~\ref{sec:let} which is also a great point to perform type generalization.

\begin{figure}

  \[
    (\alpha \rightarrow \beta \rightarrow \alpha) \xrightarrow{\text{close over } \alpha} (\forall \alpha.\; \alpha \rightarrow \beta \rightarrow \alpha)
  \]
  \caption{An example of closing over $\protect\alpha$. $\protect\beta$ is considered to be an open type variable.}
  \label{fig:closing}
\end{figure}

Apart from \emph{let expressions} Nix also has \emph{attribute sets} discussed in section~\ref{sec:attrSet}, definitions in which are semantically similar to those in \emph{let expressions}. \emph{Attribute set} definitions also seem like a good point for \emph{generalization}.

\subsection{Inferring recursive types} \label{sec:recLet}

Recursive definitions pose a challenge for type inference without developer-supplied type annotations (which will be the case for the vast majority of code). There are three major approaches to inferring types of recursive definitions:

\begin{enumerate}
  \item Infer all recursive definitions as being monomorphic. This is the approach taken in \emph{Standard ML}~\cite{milner1990definition}. The obvious drawback is the fact that types are monomorphic. This is not something we want as discussed in section~\ref{sec:polymorphism}.
  \item Analyze the call graph of recursive definitions, separate the definitions into strongly connected components, topologically sort the components, and infer the types of each strongly connected component separately. This is the approach taken in \emph{Haskell}~\cite{damas1982principal}. While this approach yields polymorphic types, it does not produce the ``most polymorphic'' (principal) types.
  \item The last implements a more complicated iteration-based type inference algorithm with a user-configurable number of iterations.~\cite{goldberg2000mercury} If the specified number of iterations is not enough to deduce the type of a recursive definition, it is rejected. The clear upside of this approach is that the inferred type (if it is given enough iterations and succeeds) is the principal (most general, most polymorphic) type of the expression.
\end{enumerate}

While the last approach would allow us to, in theory, type more programs, we feel that tasking the user with configuring the typechecker would make the barrier to entry higher than it needs to be, and in practice the second approach would be useful enough (and faster), so that is the approach we chose to go with.

\subsection{Row polymorphism}

Nix has first-class hashmaps with strings as keys called \emph{attribute sets} as discussed in section~\ref{sec:attrSet} which are in practice used as structures in much the same way it is done in JavaScript\footnote{\url{https://www.javascript.com}}. This happens because the languages are dynamic and don't afford the programmer the ability to explicitly define custom structure types. Of course, there is the ability to not only create \emph{attribute sets}, but you can also modify existing \emph{attribute sets} as discussed in section~\ref{sec:update}.

Of course, since \emph{attribute sets} are used as structures, their internal structure (keys and types of values) has significance and should be tracked in types just like objects and structures are tracked in most statically typed programming languages. Even though it is in most cases implemented as a mechanism that allows the programmer to introduce new types which correspond to a static set of field names and types, and the type system is presented in terms of those types, this is not significantly different from tracking keys directly. In fact, the \emph{Go}\footnote{\url{https://golang.org}} programming language has a notion of \emph{protocols} which bridges the gap between the two paradigms.

This leads to a non-trivial typechecking case where modifying a structure leads to the resulting type depending on the type of the input structure as shown in figure~\ref{lst:update_ex}. The figure shows a lambda expression which receives an \emph{attribute set} as input, and updates the field \texttt{a} with the value of \texttt{1}. It quickly becomes problematic to derive a type for this expression if we decide to track fields at the type level: we can say that the lambda takes a value of type \texttt{\{\}} and returns a value of type \texttt{\{a = Integer;\}} (type of functions will later be written as \texttt{<input type> -> <output type>}). This might seem like a natural type to derive, however it is obviously not correct. Calling it with a values of type \texttt{\{b = String;\}} and \texttt{\{b = Float; a = Bool;\}} would both be very much valid, and would both produce different resulting types: \texttt{\{a = Integer; b = String;\}} and \texttt{\{a = Integer; b = Float;\}}. As stated above, the output type of a lambda expression like this depends on the input type.

\begin{figure}
  \begin{minipage}{\textwidth}
    \begin{verbatim}
x: x // {a = 1;}
    \end{verbatim}
  \end{minipage}
  \caption{A lambda expression which updates an input set.}
  \label{lst:update_ex}
\end{figure}

This problem is widely known as \emph{row polymorphism}~\cite{wand1991type} – the term \emph{row} refers to a single key-value type pair. Thus, \emph{row polymorphism} is the polymorphism of key-value pairs. This fields has already received some research attention: a recent paper on the topic was written by Morris and McKinna~\cite{morris2019abstracting}. In the paper the authors introduce the \emph{Rose}\footnote{The name is a pun on ``rows'' referring to row polymorphism.} programming language which supports \emph{row polymorphism}.

\subsubsection{Rose} \label{sec:rose}

In \emph{Rose} row polymorphism is implemented by introducing two predicates on types. The first one denotes a \emph{combination} of two ``record'' types:

\begin{equation} \label{eq:combination}
  \alpha \odot \beta \sim \gamma
\end{equation}


where $\alpha$ and $\beta$ are the two types that are being combined and $\gamma$ is the resulting type. The semantics of what \emph{combination} is depend on the semantics of the language itself. Examples include:

\begin{enumerate}
  \item The two types can not have overlapping rows (combining types with overlapping rows is considered an error), and the resulting type is the union of the rows from the two types.
  \item The two types can have overlapping rows, and the resulting type is either a left-biased or right-biased union of the rows from the two types.
\end{enumerate}

\def\clt{\mathrel{\mathrlap{\bigcirc}\!<\:}}

The second predicate is:

\begin{equation}
  \alpha \clt \beta
\end{equation}

where all rows from $\alpha$ are present in $\beta$. This predicate isn't strictly necessary, and can be expressed through predicate~\ref{eq:combination}:

\begin{equation}
  \alpha \clt \gamma = \exists \beta. \; \beta \odot \alpha \sim \gamma
\end{equation}

Nevertheless, this predicate was still introduced as it closely matches the language term syntax and simplifies further processing of type.

\subsubsection{Row polymophism in Nix} \label{sec:nixRowPoly}

When deciding how to represent row polymorphism in Nix it is helpful to examine the base operators which would conjure row-related predicates during type inference. They include:

\begin{enumerate}
  \item Attribute set field accessing (the \texttt{.} operator) as discussed in section~\ref{sec:attrSet}
  \item Attribute set field accessing with a default value (\texttt{\{a = 1;\}.b or 8})
  \item Attribute set field presence check (\texttt{\{a = 1;\} ? a})
  \item Attribute set updates (the \texttt{//} operator) as discussed in section~\ref{sec:attrSet}
\end{enumerate}

In terms of the predicates described in section~\ref{sec:rose} the expression \texttt{x: x.a}, which is a lambda expression which return the field \texttt{a} from the attribute set supplied as the argument, would have the following type:

\begin{equation}
  \forall \alpha \beta. \; (\{ a = \beta; \} \clt \alpha) \Rightarrow \alpha \rightarrow \beta
\end{equation}

Analogously, the expression \texttt{x: y: x // {a = y;}}, which returns the attribute set supplied as the argument updated with the field \texttt{a} supplied as the second argument, would have the following type:

\begin{equation}
  \forall \alpha \beta \gamma. \; (\alpha \odot \{ a = \beta; \} \sim \gamma) \Rightarrow \alpha \rightarrow \beta \rightarrow \gamma
\end{equation}

The types of the other two expressions are not fully expressible with the predicates from \emph{Rose}. This is due to the \emph{Rose} type system not having support for working with missing (optional) rows as it is very dynamic in nature. Nix, on the other hand, being a dynamic language, has full support for introspecting values at runtime, including branching on the presence of a key in an \emph{attribute set} as mentioned before.

\subsubsection{Nix row predicates}

When deciding how to model the four base row-related operations we decided to derive the predicates from the operations themselves. This would yield simpler types, and having simpler types for base operations leads to simpler types overall. Furthermore, the type system would in all likelihood be easier for Nix developers to use and understand if it is expressed in terms of familiar operations.

We will now describe the row predicates we have chosen for Nix. Note the analogy with the enumeration from section~\ref{sec:nixRowPoly}.

\newcommand{\update}{\mathbin{/\!\!/}}
\newcommand{\optDot}{.\!?}

\paragraph{Attribute set field predicate} \label{sec:attrSetPred}

\begin{equation} \label{eq:fieldPred}
  \alpha. \text{x} = \beta
\end{equation}

The type $\alpha$ should be an attribute set and it should contain a field $x$ of the type $\beta$. This predicate arises from the use of the \emph{dot operator} (\texttt{.}) as discussed in section~\ref{sec:attrSet}. The syntax was chosen to be analogous to that of the Nix term syntax.

The term \texttt{x: x.y} would yield the following type:

\begin{equation} \label{eq:dotExample}
  \forall \alpha \beta. \; \alpha.\text{y} = \beta \Rightarrow \alpha \rightarrow \beta
\end{equation}

\paragraph{Optional attribute set field predicate}

\begin{equation} \label{eq:optFieldPred}
  \alpha \optDot \text{x} = \beta
\end{equation}

The type $\alpha$ should be an attribute set and if it contains the field $x$, then the type of the field should be $\beta$. This predicate arises from the combination of the \emph{dot operator} (\texttt{.}) and the \emph{or operator} (\texttt{or}) which is used to supply a fallback value if the attribute set does not contain the required field. The syntax was chosen to be analogous to that of the Nix term syntax.

The term \texttt{x: x.y or 1} would yield the following type:

\begin{equation} \label{eq:optFieldPredExample}
  \forall \alpha. \; \alpha \optDot \text{y} = \text{Number} \Rightarrow \alpha \rightarrow \text{Number}
\end{equation}

This predicate also arises from the use of the \emph{attribute set field presence check operator} (\texttt{?}). In that case the

The term \texttt{x: x ? y} would yield the following type:

\begin{equation} \label{eq:optFieldPredExample2}
  \forall \alpha \beta. \; \alpha \optDot \text{y} = \beta \Rightarrow \alpha \rightarrow \text{Bool}
\end{equation}

\paragraph{Record update predicate}

\begin{equation} \label{eq:upatePred}
  \alpha \update \beta \sim \gamma
\end{equation}

All $\alpha$, $\beta$ and $\gamma$ should be \emph{attribute sets}. The \emph{attribute set} $\gamma$ should contain all fields from both $\alpha$ and $\beta$, preferring the types from $\beta$ if they happen to overlap. That is, if $\alpha$ and $\beta$ both contain a field with the same name, then $\gamma$ should contain the same field with the type from $\beta$. This behavior matches the way the \emph{update operator} (\texttt{//}) works as discussed in section~\ref{sec:update}.

The term \texttt{x: y: x // y} would yield the following type:

\begin{equation} \label{eq:updatePredExample}
  \forall \alpha \beta \gamma. \; \alpha \update \beta \sim \gamma \Rightarrow \alpha \rightarrow \beta \rightarrow \gamma
\end{equation}

\subsection{Atmoic types}

Nix is a dynamic language, and as such, allows dynamic coercions between certain types. This means that we can not use the dynamic types from the interpreter to perform static type checking -- if we did that we would reject \emph{a lot} of valid programs.\footnote{``Valid'' not just in the sense that they would not produce runtime errors, but ``valid'' in the sense that the programmer would judge the program as being ``correct''.}

For this reason, we will have to define a smaller set of static types that will be employed in our type system. The static types along with the corresponding dynamic types from table~\ref{tab:atomicTypes} are shown in table~\ref{tbl:staticTypes}.

\begin{table}[hbp]
  \centering
  \caption{Mapping from static types to dynamic types}
  \begin{tabular}{ ||c||c|| } \hline
    \textbf{Static type}    & \textbf{Dynamic types} \\ \hline \hline
    \multirow{3}{*}{String} & URI                    \\ \cline{2-2}
                            & Path literal           \\ \cline{2-2}
                            & String                 \\ \hline \hline
    \multirow{2}{*}{Number} & Integer                \\ \cline{2-2}
                            & Float                  \\ \hline \hline
    Bool                    & Bool                   \\ \hline \hline
    Null                    & Null                   \\ \hline
  \end{tabular}
  \label{tbl:staticTypes}
\end{table}

\begin{table*}[hbp]
  \centering
  \caption{Table of Nix Expression Language operators and their static types}
  \begin{tabular}{||c||c||c||c||} \hline
    \textbf{Operator} & \textbf{Operator description} & \textbf{Static type}                                                                                                         \\ \hline \hline
    \texttt{-}        & Numeric negation              & $\text{Number} \rightarrow \text{Number}$                                                                                    \\ \hline
    \texttt{!}        & Boolean negation              & $\text{Bool} \rightarrow \text{Bool}$                                                                                        \\ \hline
    \texttt{++}       & List concatenation            & $\forall \alpha. \; [\alpha] \rightarrow [\alpha] \rightarrow [\alpha]$                                                      \\ \hline
    \texttt{==}       & Equality                      & $\forall \alpha. \; \alpha \rightarrow \alpha \rightarrow \text{Bool}$                                                       \\ \hline
    \texttt{!=}       & Not equality                  & $\forall \alpha. \; \alpha \rightarrow \alpha \rightarrow \text{Bool}$                                                       \\ \hline
    \texttt{<}        & Less than                     & $\text{Number} \rightarrow \text{Number} \rightarrow \text{Bool}$                                                            \\ \hline
    \texttt{<=}       & Less than or equal            & $\text{Number} \rightarrow \text{Number} \rightarrow \text{Bool}$                                                            \\ \hline
    \texttt{>}        & Greater than                  & $\text{Number} \rightarrow \text{Number} \rightarrow \text{Bool}$                                                            \\ \hline
    \texttt{>=}       & Greater than or equal         & $\text{Number} \rightarrow \text{Number} \rightarrow \text{Bool}$                                                            \\ \hline
    \texttt{+}        & Addition                      & $\forall \alpha. \; \strOrNum \sim \alpha \Rightarrow \alpha \rightarrow \alpha \rightarrow \alpha$                          \\ \hline
    \texttt{-}        & Subtraction                   & $\text{Number} \rightarrow \text{Number} \rightarrow \text{Number}$                                                          \\ \hline
    \texttt{*}        & Multiplication                & $\text{Number} \rightarrow \text{Number} \rightarrow \text{Number}$                                                          \\ \hline
    \texttt{/}        & Division                      & $\text{Number} \rightarrow \text{Number} \rightarrow \text{Number}$                                                          \\ \hline
    \texttt{\&\&}     & Boolean ``and''               & $\text{Bool} \rightarrow \text{Bool} \rightarrow \text{Bool}$                                                                \\ \hline
    \texttt{||||}     & Boolean ``and''               & $\text{Bool} \rightarrow \text{Bool} \rightarrow \text{Bool}$                                                                \\ \hline
    \texttt{->}       & Implication                   & $\text{Bool} \rightarrow \text{Bool} \rightarrow \text{Bool}$                                                                \\ \hline
    \texttt{//}       & Attribute set update          & $  \forall \alpha \beta \gamma. \; \alpha \update \beta \sim \gamma \Rightarrow \alpha \rightarrow \beta \rightarrow \gamma$ \\ \hline
  \end{tabular}
  \label{tbl:allOperators}
\end{table*}

\subsection{Addition} \label{sec:addition}

Among all operators, addition (\texttt{+}) has semantics that stand out from the rest: addition can be applied to either two \texttt{Number} operands or two \texttt{String} operands. All other operands are monomorphic in that they can only be applied to two operands of a predefined type. In that sense addition in polymorphic.

Since our current implementation does not support sum types (see section~\ref{sec:sumTypes} for more detail), it is non-trivial to encode the type of addition in our current type system. We wanted to encode the type without having to resort to extending the type system too much (without implementing sum types).

The simplest approach is to introduce a simple predicate which is satisfied only if the argument is either of type \texttt{Number} or \texttt{String}:

\begin{equation}
  \strOrNum \sim \alpha
\end{equation}

With this predicate the type of the addition operator (\texttt{+}) now becomes:

\begin{equation}
  \forall \alpha. \; \strOrNum \sim \alpha \Rightarrow \alpha \rightarrow \alpha \rightarrow \alpha
\end{equation}

\subsection{Typing rules}

What follows is a formal description of the developed type system. For brevity we have omitted rules for typing literal constant values described in tables \ref{tab:atomicTypes} and \ref{tbl:staticTypes}, and most operators described in table~\ref{tbl:allOperators} as those rules are trivial.

\subsubsection{Lists}

After doing an informal analysis of existing Nix codebases we have concluded that it is not idiomatic to have heterogeneous lists in Nix. Thus, we have chosen to enforce homogenous lists in our static type system:

\begin{equation}
  \trfrac
  {\Gamma \vdash e_1:\tau \quad \cdots \quad \Gamma \vdash e_n:\tau}
  {\Gamma \vdash [e_1 \; \cdots \; e_n ] : [\tau]}
\end{equation}

\begin{equation}
  \trfrac
  {\Gamma \vdash e_1:[\tau] \qquad \Gamma \vdash e_2 : [\tau]}
  {\Gamma \vdash (e_1 +\!\!+ \: e_2) : [\tau]}
\end{equation}

\subsubsection{Lambda abstractions}

Typing rules for both lambda abstraction definitions and application is common among type system derived from Hindley-Milner:

\begin{equation}
  \trfrac
  {\Gamma \vdash e_1:\tau_1 \rightarrow \tau_2 \qquad \Gamma \vdash e_2 : \tau_1}
  {\Gamma \vdash (e_1 \; e_2) : \tau_2}
\end{equation}

\begin{equation}
  \trfrac
  {\Gamma, \text{x}: \tau_1 \vdash e : \tau_2}
  {\Gamma \vdash (\text{x: } e) : \tau_1 \rightarrow \tau_2}
\end{equation}

\subsubsection{Addition}

The rationale for having a special case for the addition operator is layed out in section~\ref{sec:addition}:

\begin{equation}
  \trfrac
  {\Gamma \vdash e_1:\tau \qquad \Gamma \vdash e_2 : \tau \qquad \Gamma \vdash (\text{String} || \text{Number}) \sim \tau}
  {\Gamma \vdash (e_1 + e_2) : \tau}
\end{equation}

\subsubsection{Conditional expressions}

Conditionals are also very similar to what is described in the Hindley–Milner type system:

\begin{equation}
  \trfrac
  {\Gamma \vdash e_1 : \text{Bool} \qquad \Gamma \vdash e_2 : \tau \qquad \Gamma \vdash e_3 : \tau}
  {\Gamma \vdash (\text{if} \; e_1 \; \text{then} \; e_2 \; \text{else} \; e_3) : \tau}
\end{equation}

\subsubsection{\texttt{let}-generalization}

Just like in many functional type systems, our type system will support let-generalization -- constants bound in a \texttt{let} expression are generalized as much as possible. All free type variables that were defined while inferring an expression are closed over.

A simple version of rule is as follows:

\begin{equation} \label{eq:letGeneralization}
  \trfrac
  {\Gamma \vdash e_1 : \tau_1 \qquad \Gamma, (\text{x}: \forall \{\text{ftv}(\tau_1) - \text{ftv}(\Gamma)\}. \; \tau_1) \vdash e_2 : \tau_2}
  {\Gamma \vdash (\text{let} \; \text{x = } e_1; \; \text{in} \; e_2) : \tau_2}
\end{equation}

where $\text{fvt}(\alpha)$ is the set of all free variables that occur in $\alpha$.

Of course, this does not cover all possible syntactic constructs -- it is only concerned with inferring a single declaration. It is trivially extendable to a series of independent declarations. Inferring recursive declarations is discussed in section~\ref{sec:recLet}.

Inferring \emph{let expressions} trivially extends to inferring fields of \emph{attribute sets} and is omitted for brevity.

\subsubsection{Instantiation}

When using expressions with generalized types, we need to instantiate theme to concrete types:

\begin{equation}
  \trfrac
  {\Gamma \vdash e : \forall \alpha. \; \tau_1}
  {\Gamma \vdash e: [\alpha \rightarrow \tau_2] \tau_1}
\end{equation}

where $[\alpha \rightarrow \beta]\gamma$ denotes the result of substituting all free occurrences of $\alpha$ in $\gamma$ with $\beta$.

\subsubsection{Attribute sets}

Basic \emph{attribute set} operations are now trivially expressible through the predicate defined in section~\ref{sec:attrSetPred}:

\begin{equation}
  \trfrac
  {\Gamma \vdash e_1 : \tau_1 \qquad \Gamma \vdash e_2 : \tau_2 \qquad \Gamma \vdash \tau_1 \update \tau_2 \sim \tau_3}
  {\Gamma \vdash (e_1 \update e_2) : \tau_3}
\end{equation}

\begin{equation}
  \trfrac
  {\Gamma \vdash e.\text{x} = \tau}
  {\Gamma \vdash (e.\text{x}) : \tau}
\end{equation}

\begin{equation}
  \trfrac
  {\Gamma \vdash e_2 : \tau \qquad \Gamma \vdash e_1 \optDot \text{x} = \tau}
  {\Gamma \vdash (e.\text{x} \; \text{or} \; e_2) : \tau}
\end{equation}

\begin{equation}
  \trfrac
  {\Gamma \vdash e \optDot \text{x} = \tau}
  {\Gamma \vdash (e \; \text{?} \; \text{x}) : \text{Bool}}
\end{equation}

When defining typing rules for \emph{attribute sets} we will use the notion of rows similar to what is used in the \emph{Rose} paper~\cite{morris2019abstracting}: for typing purposes an attribute set will be defined as a product of a set of \emph{rows} – key-value pairs: $\Pi \xi$ where $\xi$ is a set of \emph{rows}.

A key-value pair will be denoted as $\text{x} = \alpha$ where `x' is the name of a field and $\alpha$ is the type of the field.

Of course, a \emph{set of rows} can not contain multiple \emph{rows} with the same key.

With that we can define how those predicate are actually satisfied:

\begin{equation}
  \trfrac
  {(\text{x} = \tau) \in \xi}
  {(\Pi \xi).\text{x} = \tau}
\end{equation}

\begin{equation}
  \trfrac
  {\text{x} \notin \xi}
  {(\Pi \xi) \optDot \text{x} = \tau}
\end{equation}

\begin{equation}
  \trfrac
  {\Gamma \vdash \tau_1.\text{x} = \tau_2}
  {\Gamma \vdash (\tau_1 \optDot \text{x}) = \tau_2}
\end{equation}

where $(\text{x} = \tau) \in \xi$ denotes the presence of the \emph{row} $(\text{x} = \tau)$ in the \emph{set} $\xi$, and $\text{x} \notin \xi$ denotes the absence of a \emph{row} with the key `x' in the \emph{set} $\xi$.

The definition of the ``update'' predicate uses standard set comprehension notation:

\begin{equation}
  \trfrac
  {}
  {\Pi \xi_1 \update \Pi \xi_2 \sim \Pi (\xi_2 \cup \{a = \tau || (a = \tau) \leftarrow \xi_1, a \notin \xi_2\})}
\end{equation}

\subsubsection{with expressions} \label{sec:withExprType}

\emph{with expressions} discussed in section~\ref{sec:withExp} are not difficult to define a typing rule for:

\begin{equation}
  \trfrac
  {\Gamma \vdash e_1: \Pi \xi \qquad \Gamma, \xi \vdash e_2 : \tau}
  {\Gamma \vdash (\text{with } e_1\text{; } e_2) : \tau}
\end{equation}

\emph{with expressions} do, however, pose a particularly hard problem when implementing the type system – we can't always statically know the fields in $e_1$ -- it can be passed in as a function argument. This means that the typing environment of a function can depend on its argument. Implementing this in a wholistic manner would require our type system support a form of dependent types. That is not a direction we want to pursue -- introducing dependent types would greatly complicate the type system to a point where automated type inference would not practical and the type system would be a lot harder to work with.

Instead, we opted to define a heuristic which in our testing was proven to cover most practical cases: when encountering a variable in the body of a \emph{with expression}:

\begin{enumerate}
  \item if a variable was defined in the body of the with expression and is in scope, return that type
  \item if the \emph{attribute set} from the \emph{with expression} is statically known and defined a key with the appropriate name then return that types
  \item if the \emph{attribute set} from the \emph{with expression} is not statically known and the outer context of the \emph{with expression} contains a variable with the appropriate name, return that type
  \item else, create a constraint that the first operand in the \emph{with expression} contains a field with the appropriate name and type
\end{enumerate}

\section{Implementation} \label{sec:implementation}

In this section we will explore the techniques employed in our implementation~\cite{github-source} of our type system. It is implemented in the \emph{Haskell}\footnote{\url{https://www.haskell.org}} programming language.

% \subsubsection{Effects}

% Our implementation uses the \emph{freer-simple}\footnote{\url{https://github.com/lexi-lambda/freer-simple}} library as the effects system.

% The

% \begin{code}
% type InferEffs =
%   !! [  Fresh,
%        Reader SrcSpan,
%        Writer (SrcSpan, Errors),
%        Writer PredicateError,
%        Writer Pred,
%        State (MKM.Map Pred),
%        VariableMapEff,
%        Writer UnifyingError,
%        Writer InferError,
%        State Substitution,
%        TreeTracer
%   ]
% \end{code}

\subsection{Effect system}

\subsubsection{Monad transformers}

Developing a typechecker requires working with a large amount of non-trivial algorithms which influence each other in complex ways. A widespread way of dealing with such situations is introducing a common computational environment through which different parts of the program can interact (instead of ``manually'' passing state around). A well-established way of handling state in purely functional programming languages is the \emph{Monad} abstraction~\cite{moggi1988computational}.

It is natural to want to abstract the underlying monad – explicitly declare what features a function actually needs the monad to have. Furthermore, it is desireable to decompose the features of the a monad not only in function definitions, but also at the call site – it is desireable to be able to conjure up monads with desireable traits in an ad-hoc manner. This has been achieved a while ago with \emph{monad transformers}~\cite{liang1995monad}.

A drawback of monad transformers is their somewhat rigid nature – every behavior a monad needs to have needs to be defined as a distinct transformer with appropriate instances. The instances need to not only implement the desired behavior, but also explicitly ``pass through'' \emph{all} other effects that it might be used in combination with. This means that adding extra behavior leads to a large amount of boilerplate. Furthermore, monads themselves (monad transformers) are not in general composable, leading to a whole other set of problems. In addition, it is cumbersome to locally change parts of the underlying implementation of a monad dynamically – it might be useful to tweak some behavior in only a part of an algorithm, leaving the rest to the abstract implementation; monad transformers don't really have such capabilities.

\subsubsection{Free monad extensible effects}

Recently a new approach to effect systems has been getting more attention – \emph{free monads algebraic effects}~\cite{kiselyov2015freer, ploeg2014reflection, kiselyov2013extensible}. This approach solves the problems with monad transformers mentioned above. Rather than being transformers, effects are described as \emph{functors}. The composability problem is resolved due to functors being in general composable. Contrary to how monad transformers operate, effects (or their carriers) in \emph{free monads algebraic effects} (we will from now on refer to them as just \emph{algebraic effects}) don't have any particular interpretation of the effect associated with them. Effects are interpreted in an ad-hoc manner at the call site. Furthermore, this allows us to dynamically modify the behavior of effects without changing the types or implementations of functions we wish to change the behavior of.
% How we use \emph{algebraic effects} will be discussed in more detail in section~\ref{sec:implementation}.

\subsection{Substitutions}

\subsubsection{Substitutions in other implementations}

In most typechecker implementations a substitution is a mapping from \emph{type variables} to \emph{types}. These substitutions are usually generated during the unification of type constraints and are, in general, applied to types as soon as they are available.

The notion of substitution composition is usually taken to mean a binary operation which produces a new substitution that when applied (|sub|) to a type would produce the same result as sequentially applying the two source substitutions:

\begin{code}
sub (s1 . s2) = sub s1 . sub s2
\end{code}

\subsubsection{Substitutions in our implementation}

In our implementation the application of substitutions is delayed as much as possible. They are applied in exactly two cases:

\begin{enumerate}
  \item Right before unifying a constraint
  \item Right before closing over a type
\end{enumerate}

This choice has several implications. Firstly, we can never have conflicting ``keys'' in substitutions (which is not the case in other implementations). Using this information we can define stronger substitution composition:

\begin{code}
newtype Substitution = Substitution
  {unSubstitution :: Map TypeVariable NType}

instance Semigroup Substitution where
  a'@(Substitution a) <> b'@(Substitution b) =
    Substitution $ M.unionWith undefined
      (fmap (sub b') a)
      (fmap (sub a') b)
\end{code}


This means that the order of composition does no longer matter, and applying the composition of substitutions yields more information than applying them sequentially.

Another consequence of the decision to delay substitutions is that now the role of the |infer| step of typechecking only does the following tree things:

\begin{enumerate}
  \item returns a type variable that represents the type of the expression being inferred
  \item emits constraints generated during inferring
  \item emits predicates generated during inferring
\end{enumerate}

\subsection{Typing context}

The typing context in our implementation in essence consists of two distinct data structures:

\begin{enumerate}
  \item the types of variables currently in scope
  \item the predicates which are applicable to those types
\end{enumerate}

\subsubsection{Variables in scope}

Note that in practice the ``variables in scope'' data structure only needs to contain type variables since we apply substitutions as late as possible.

In actual fact, due to the heuristics described in section~\ref{sec:withExprType} the types of variables in scope can not be represented as static data structure, but rather is implemented as a scoped effect\footnote{Effects in our case are implemented using the \emph{freer-simple} library: \url{https://github.com/lexi-lambda/freer-simple}}:

\begin{code}
data VariableMapEff x where
  LookupVariable
    :: VarName
    -> VariableMapEff (Maybe Scheme)

lookupVar
  :: Member VariableMapEff eff
  => VarName -> Eff eff (Maybe Scheme)
lookupVar = send . LookupVariable
\end{code}

This allows us to dynamically redefine what it means to lookup a variable in terms of its implementation in the outer context.

\subsubsection{Predicates}

Predicates can, on the other hand, be stored as a static data structure. The only non-standard behavior is that the set of predicates needs to be queried and manipulated based on the types that they contain, rather than on the predicates themselves.

For this reason we have developed a data structure we call ``multi-keyed map''.

\begin{code}
class (Ord (Key t), Ord t) => Keyable t where
  type Key t  :: Type
  getKeys     :: t -> [Key t]

data MultiKeyedMap t = MultiKeyedMap
  (M.Map (Key t) (S.Set t)) (S.Set t)
\end{code}

The data structure is the product of two other data structures:

\begin{enumerate}
  \item A map, mapping possible keys of the stored |t| to a set of values the contain the given key
  \item A set which contains the values that don't contain any keys
\end{enumerate}

The |Keyable| typeclass is used to extract the keys from the values that need to be stored.

The union of two maps is defined trivially -- by taking the union of the inner structures:

\begin{code}
instance (Ord (Key t), Ord t)
  => Semigroup (MultiKeyedMap t) where
  (MultiKeyedMap x x') <> (MultiKeyedMap y y') =
    MultiKeyedMap
      (M.unionWith S.union x y) (x' <> y')
\end{code}

Creating a singleton map from a single values is defined like so:

\begin{code}
singleton :: Keyable t => t -> MultiKeyedMap t
singleton t = case getKeys t of
  [] -> MultiKeyedMap M.empty (S.singleton t)
  ks@(_ : _) -> MultiKeyedMap
    (M.fromList $ fmap (,s) ks) S.empty
  where
    s = S.singleton t
\end{code}

With these operations defining the |insert| operation becomes trivial:

\begin{code}
insert
  :: Keyable t
  => t -> MultiKeyedMap t -> MultiKeyedMap t
insert t m = m <> singleton t
\end{code}

Now we come to the lookup operation. Note that the algorithmic complexity of querying the predicates by a type they contain is now the cost of an ordinary map lookup -- $O(log(n))$ where $n$ is the number of values contained in the map (in our case the number of all types referenced from within the predicates):

\begin{code}
lookup
  :: Keyable t
  => Key t -> MultiKeyedMap t -> S.Set t
lookup k (MultiKeyedMap m _) =
  fromMaybe S.empty $ M.lookup k m
\end{code}

\subsection{Closing over type variables}

An observation we can make is the following: it only makes sense to close over the type variables that were generated during the inference of the type we are closing over. Any other type variables either:

\begin{enumerate}
  \item are not present in the type
  \item are used in an outer lexical scope (environment)
\end{enumerate}

This greatly simplifies implementing the typing rule~\ref{eq:letGeneralization} since instead of having to calculate a set of free type variables of the whole context, we only need to obtain a predicate which identifies if a type variable was introduced during the inference of an expression. This is made even easier if type variables are internally represented as integers, as they often are.

In our case this is complicated by the fact that we need to introduce type variables which are fields of an \emph{attribute set} from an outer lexical scope. This is problematic because while the type variable can be defined during inference, it is actually logically defined at the same position as the initial \emph{attribute set}; it has the same lexical scope as the \emph{attribute set}. So, the lexical scope of a type variable can not be derived from the inference context of an expression.

A solution we came up with is to have two ways a type variable can be constructed -- as an ordinary type variable backed by a single integer, or as a sub-type variable which has the same lexical scope as another type variable:

\begin{code}
data TypeVariable
  =    TypeVariable     Int
  |    SubTypeVariable  Int Int
\end{code}

This way, when applying a predicate to check the lexical scope of a type variable we can always restrict ourselves to the first integer.

\section{Possible improvements}

\subsection{Making predicates more user-friendly}

\subsubsection{A novel technique}

The reader might observe that, although the predicates have a parallel with the Nix term syntax, visually parsing predicates and mentally applying them to the specified type variables can be difficult even in such simple examples. Here we will explore a technique that can be used to make these predicates easier for the programmer to work with.

The technique is based on the fact that a type variable by itself can only convey the following information:

\begin{enumerate}
  \item positional information -- a specific position in a type
  \item identity information -- the type variable is equivalent to other type variable with the same name within the scope where it is declared
\end{enumerate}

Note that all of what follows is only concerned with syntactic constructs. It does in no way alter the semantic of the predicates described above.

We would like to point out that all of the predicates have an operand which is the ``result'' of the predicate -- a single type variable which is said to be equivalent to a more complex expression. That is $\beta$ in predicates \ref{eq:fieldPred} and \ref{eq:optFieldPred}, and $\gamma$ in predicate \ref{eq:upatePred}.

The observation we need to make is that \emph{referential transparency}\footnote{\url{https://en.wikipedia.org/wiki/Referential_transparency}} is it natural notion in functional programming languages like Nix, and it is natural to replace variables with their definitions. In some sense the ``results'' of predicates described above can be interpreted as being ``defined'' by the predicate to be ``equal'' to the combination of the other operands, and, as such, can be replaced with their definitions.

This means that we can syntactically replace \emph{type variables} with a portion of a predicate -- replace the variable with the definition. If we apply this technique to the type~\ref{eq:dotExample} we get the following type:

\begin{equation}
  \forall \alpha. \; \alpha \rightarrow \alpha.\text{y}
\end{equation}

When applied to the type~\ref{eq:updatePredExample} it yields:

\begin{equation}
  \forall \alpha \beta. \; \alpha \rightarrow \beta \rightarrow (\alpha \update \beta)
\end{equation}

We believe the derived types above are a lot easier to understand than their initial counterparts.

\subsubsection{Implementation notes}

For practical reasons, when applying the technique, there should be a threshold that limits the syntactic complexity of types that can be ``inlined''. This is due to the types being potentially as big as literals in the source expressions.

Complexity of types should also be used as a heuristic in cases where there are multiple potential ``definitions'' for the same type variable – it is preferable to use the ``definitions'' with the lowest syntactic complexity as it leads to simpler types.

\subsubsection{Limitations}

The technique is not in applicable if the ``result'' of a predicate is not a type variable like in type \ref{eq:optFieldPredExample2}.

Another limitation is that we can not remove predicates if their ``result'' type variable is not present in the head of the type like in type \ref{eq:optFieldPredExample}, since we would be losing typing information by doing so.

\subsection{Type hints}

Dynamic types in Nix contain more information than our static type system can convey. A usability improvement to provide more utility to the programmer might be to allow extra information to be tagged onto type information and propagated during type inference. This information would not influence the typechecking algorithm in any way. It would only provide utility to the person reading the resulting types or to external tools.

Some information that might be useful as type hints:

\begin{enumerate}
  \item The dynamic type of strings (URI, Path, etc.) – useful information for the programmer
  \item Programmer-supplied annotations. When writing nix expressions it is common practice to have a program in some other programming language embedded in Nix as a string literal. It would be useful to propagate information about the underlying programming language to aid IDEs -- they could enable special syntax highlighting and linting based on the programming language.
\end{enumerate}

\subsection{Sum types} \label{sec:sumTypes}

In our static type system we introduced a ``special case'' for the \emph{addition operator} as it could be applied to a number of different types.

The idea of this improvement is to generalize the case and allow the type system to handle arbitrary sums of arbitrary types. This can be implemented as a predicate stating that a type variable can be a certain type:

\begin{equation}
  \alpha \lessdot \beta
\end{equation}

where $\beta$ can be the type $\alpha$. With this predicate, our ``String or Number'' predicate can be expressed like so:

\begin{equation}
  \strOrNum \sim \alpha = (\text{String} \lessdot \alpha, \text{Number} \lessdot \alpha)
\end{equation}

\section{Conclusion}

Nix is a rapidly growing ecosystem. At the time of writing, nixpkgs, the Nix package registry, contains over 60000 packages\footnote{\url{https://github.com/NixOS/nixpkgs}} that can be either installed or used as dependencies in other projects. There is much interest in the approach used by Nix, and making the ever-growing codebase of configurations easier to manage and extend would be a welcome development.

Even though the developed type checker will need to check code for an existing dynamic language, and no type checker can cover all of the valid cases, producing false negatives, covering most of the cases encountered in practice would be useful enough to be worth using. Furthermore, plenty of research has already been conducted on which the Nix Expression Language type checker can be based.

\newpage

\bibliographystyle{IEEEtran}
\bibliography{bibl}

% \registrationList

\end{document}
