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
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' gen_exerc(exs="countrycodes")
#' }
#'

gen_exerc <- function(
    exs,
    curso="",
    semestre=1,
    n_exerc=1,
    template = c("exercicio_pt", "solution_pt"),
    decimal = ",",
    ...) {




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
      template = system.file("tex",paste0(template, ".tex"), package="extexams"),
      header = header,
      name = c(
        glue::glue("exercicio_{i}_{semestre}_{curso}_"),
        glue::glue("gabarito_{i}_{semestre}_{curso}_")
      ),
      dir = paste0("gerada_",curso,"_", semestre),
      verbose = TRUE,
      #...
    )
  }
}
