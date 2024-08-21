#' Gerar exercícios
#'
#' @param exs vetor com o nome dos arquivos dos exercícios
#' @param curso Nome da disciplina
#' @param semestre Semestre da disciplina
#' @param n_exerc numero de exercicios que serao gerados
#' @param template arquivo latex com o template
#' @param decimal separador de decimal
#'
#' @import exams
#' @export
#'
gen_exerc <- function(
    exs,
    curso,
    semestre,
    n_exerc,
    template = c("exercicio_pt.tex", "solution_pt.tex"),
    decimal = ",") {
  options(OutDec = decimal)


  getID <- function(i) {
    paste0(
      sample(10:99, 1),
      sample(LETTERS, 1),
      sample(1000:9999, 1),
      gsub(" ", "0", format(i, width = 2))
    )
  }

  header <- list(
    Course = curso,
    Semester = semestre,
    ID = getID
  )

  for (i in exs) {
    set.seed(semestre)
    exams::exams2pdf(
      paste0(i, ".Rmd"),
      n = n_exerc,
      template = c("exercicio_pt.tex", "solution_pt.tex"),
      header = header,
      name = c(
        glue::glue("exercicio_{i}_{semestre}_{curso}_"),
        paste0("gabarito_", i, "_", semestre, curso)
      ),
      dir = paste0("gerada_", semestre),
      verbose = TRUE
    )
  }
}
