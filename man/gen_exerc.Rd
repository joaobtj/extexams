% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_exerc.R
\name{gen_exerc}
\alias{gen_exerc}
\title{Gerar exercícios}
\usage{
gen_exerc(
  exs,
  curso = "Curso",
  semestre = "1999.1",
  n_exerc = 1,
  template = c("exercicio_pt", "solution_pt"),
  decimal = ",",
  ...
)
}
\arguments{
\item{exs}{vetor com o nome dos arquivos dos exercícios}

\item{curso}{Nome da disciplina}

\item{semestre}{Semestre da disciplina}

\item{n_exerc}{numero de exercicios que serao gerados}

\item{template}{arquivo latex com o template}

\item{decimal}{separador de decimal}

\item{...}{argumentos da funcao exams2pdf}
}
\description{
Gerar exercícios
}
\examples{
\dontrun{
p <- gen_exerc(exs = c("anova", "countrycodes"), n_exerc = 2)
}

}
