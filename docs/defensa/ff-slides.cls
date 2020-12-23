\RequirePackage{xkeyval}
\DeclareOptionX{handout}{\PassOptionsToClass{handout}{beamer}}
\ProcessOptionsX

\LoadClass[spanish]{beamer}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ff-slides}[2019/02/08 Plantilla para slides (por Franco Frizzo)]

% ============================================================================

\RequirePackage[utf8]{inputenc}
\RequirePackage[spanish,activeacute]{babel}
\spanishplainpercent

\RequirePackage{etoolbox}
\RequirePackage{kvoptions-patch}

\RequirePackage{beamerthemesplit}

\RequirePackage{lmodern}
\RequirePackage{libertineRoman}
\RequirePackage{biolinum}
\RequirePackage{eulervm}
\RequirePackage[T1]{fontenc}

\RequirePackage{amsmath,amssymb,amsthm,mathabx,mathtools}
\RequirePackage[italic]{mathastext}

\RequirePackage{multicol}

% ----------------------------------------------------------------------------

% Beamer settings
\useoutertheme{split}
\useinnertheme{rectangles}
\usefonttheme[onlymath]{serif}
\beamertemplatenavigationsymbolsempty

\definecolor{colbg}{RGB}{255,255,255}      % background color
\definecolor{colmain}{RGB}{128,128,128}    % main color

\DeclareOptionX{color}{\definecolor{colmain}{RGB}{#1}} % customize main color
\DeclareOptionX{hexcolor}{\definecolor{colmain}{HTML}{#1}}
\ProcessOptionsX

\setbeamertemplate{blocks}[rounded]
\setbeamercolor{background canvas}{bg=colbg}
\setbeamercolor{item}{fg=colmain}
\setbeamercolor{item projected}{fg=colbg, bg=colmain}
\setbeamercolor{palette primary}{use=structure,fg=colbg,bg=colmain}
\setbeamercolor{block title}{bg=colmain,fg=colbg}
\setbeamercolor{block body}{bg=colmain!15}

% ----------------------------------------------------------------------------

% Paragraph spacing
\setlength{\parskip}{.5em}

% Item spacing
\usepackage{etoolbox}
\makeatletter
\patchcmd{\@listI}{\itemsep3\p@}{\itemsep6\p@}{}{}
\patchcmd{\@listii}{\itemsep\parsep}{\itemsep3\p@}{}{}
\makeatother

% Section title frames
\newcommand\titleframe[1]{
\begin{frame}{}
    \vfill
    \centering
    \begin{beamercolorbox}[sep=8pt,center]{title}
        \usebeamerfont{title}\insertsectionhead{#1}\vspace{.2em}
    \end{beamercolorbox}
    \vfill
\end{frame}
}
% To disable them
\newcommand\notitleframes{\renewcommand\titleframe[1]{}{}}

% Continous enumerations
\newcounter{savedenum}
\resetcounteronoverlays{savedenum}
\newcommand{\saveenum}{\setcounter{savedenum}{\theenumi}}
\newcommand{\loadenum}{\setcounter{enumi}{\thesavedenum}}

% ----------------------------------------------------------------------------

\RequirePackage{listingsutf8}
\lstset{
    basicstyle=\ttfamily,
    commentstyle=\color{gray},
    keywordstyle=\color{colmain}\textbf,
    basewidth=0.5em,
    breaklines=true,                        % Automatic line breaking?
    keepspaces=true,
    breakatwhitespace=false,                % Automatic breaks only at whitespace?
    showspaces=false,                       % Don't make spaces visible
    showtabs=false,                         % Don't make tabs visible
    showstringspaces=false,                 % Don't make spaces visible inside quotes
    xleftmargin=\parindent,
    inputencoding=utf8/latin1
}

\lstdefinestyle{numbered}{
  numbers=left,
  numberstyle=\ttfamily\scriptsize\color{gray}
}

% ============================================================================
