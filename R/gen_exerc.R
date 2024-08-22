#' Gerar exercícios
#'
#' @param exs vetor com o nome dos arquivos dos exercícios
#' @param curso Nome da disciplina
#' @param semestre Semestre da disciplina
#' @param n_exerc numero de exercicios que serao gerados
#' @param template arquivo latex com o template
#' @param decimal separador de decimal
#' @param ... argumentos da funcao exams2pdf
#'
#' @import exams
#' @import dplyr
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' p <- gen_exerc(exs = c("anova", "countrycodes"), n_exerc = 2)
#' }
#'
gen_exerc <- function(
    exs,
    curso = "",
    semestre = 1,
    n_exerc = 1,
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

  p <- a <- c()
  for (i in seq_along(exs)) {
    set.seed(semestre)
    p[[i]] <- exams::exams2pdf(
      paste0(exs[[i]], ".Rmd"),
      n = n_exerc,
      template = system.file("tex", paste0(template, ".tex"), package = "extexams"),
      header = header,
      name = c(
        glue::glue("exercicio_{i}_{semestre}_{curso}_"),
        glue::glue("gabarito_{i}_{semestre}_{curso}_")
      ),
      dir = glue::glue("gerada_{curso}_{semestre}"),
      verbose = TRUE
      # ,
      # ...
    )

    a[[i]] <- exams::exams_metainfo(p[[i]], class = "data.frame", tags = TRUE)
  }

  return(
    a %>% do.call(rbind, .)%>% dplyr::select(replication,file, string,  solution)
  )
}


