#' Generate Personalized Academic Exams
#'
#' @title Generate Personalized Exams for Students
#' @description
#' A wrapper for `exams::exams2pdf` that automates the generation of
#' personalized exams based on a student list. It supports conditional
#' exercise substitution based on student performance.
#'
#' @param exercises Character vector of exercise file names (without .Rmd).
#' @param course Character string for the course name.
#' @param semester Character string for the academic semester (e.g., "1999.1").
#' @param exam_id Character string for the exam identifier (e.g., "P1").
#' @param data A data frame containing student information.
#' @param name_col Character string specifying the column containing student names.
#' @param substitution_list A named list mapping data columns to exercise indices to be replaced by "feito" if value > 0.
#' @param templates Character vector of LaTeX template names located in the package's tex folder.
#' @param decimal Character string for the decimal separator (default is ",").
#' @param output_dir Character string specifying the directory for output files.
#' @param ... Additional arguments passed to `exams::exams2pdf`.
#'
#' @return A data frame containing the concatenated answer keys for all students.
#' @family exam_generation
#' @author Joao Batista Tolentino Jr. \email{joao.tolentino@@ufsc.br}
#'
#' @importFrom dplyr any_of bind_rows select
#' @importFrom exams exams2pdf exams_metainfo
#' @importFrom glue glue
#' @importFrom purrr map map_chr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' students <- data.frame(Nome = c("Joao", "Maria"), anova = c(1, 0))
#' exs <- list("anova", "fruit")
#' answer_key <- generate_exams(
#'   exercises = exs,
#'   data = students,
#'   substitution_list = list(anova = 1),
#'   decimal = "."
#' )
#' (answer_key)
#' }
generate_exams <- function(
  exercises,
  course = "Course",
  semester = "1999.1",
  exam_id = "P1",
  data,
  name_col = "Nome",
  substitution_list = NULL,
  templates = c("prova", "gabarito"),
  decimal = ",",
  output_dir = NULL,
  ...
) {
  # Safely modify the global decimal separator and ensure it reverts on function exit
  old_opt <- options(OutDec = decimal)
  on.exit(options(old_opt), add = TRUE)

  data <- as.data.frame(data)
  data[is.na(data)] <- 0

  if (is.null(output_dir)) {
    output_dir <- glue::glue("gerada_{course}_{semester}")
  }

  base_seed <- as.numeric(gsub("[^0-9]", "", as.character(semester)))

  results <- data |>
    nrow() |>
    seq_len() |>
    purrr::map(\(i) {
      student_name <- data[i, name_col]
      current_exercises <- exercises

      # Conditional Exemption Logic:
      # Replaces a specific exercise with the "feito" (done) placeholder if the
      # student has a score > 0 in the corresponding mapped column.
      if (!is.null(substitution_list)) {
        for (col_name in names(substitution_list)) {
          if (col_name %in% names(data)) {
            exercise_idx <- substitution_list[[col_name]]
            if (data[i, col_name] > 0) {
              current_exercises[[exercise_idx]] <- "feito"
            }
          }
        }
      }

      # Resolve file paths: points to the internal package directory for "feito",
      # otherwise assumes the .Rmd is in the current working directory.
      final_exercises <- current_exercises |>
        purrr::map_chr(\(x) {
          if (x == "feito") {
            system.file("exercises", "feito.Rmd", package = "extexams")
          } else {
            paste0(x, ".Rmd")
          }
        })

      p <- exams::exams2pdf(
        file = final_exercises,
        n = 1,
        template = system.file("tex", paste0(templates, ".tex"), package = "extexams"),
        header = list(
          Course = course,
          Semester = semester,
          ID = student_name
        ),
        name = glue::glue("{templates}_{exam_id}_{semester}_{course}_{student_name}_"),
        dir = output_dir,
        ...
      )

      # Extract the solution string for this specific iteration and tag it with the student's name
      meta <- exams::exams_metainfo(p, class = "data.frame", tags = TRUE)
      meta_clean <- dplyr::select(meta, dplyr::any_of(c("replication", "file", "string")))
      meta_clean$student <- student_name

      return(meta_clean)
    })

  return(dplyr::bind_rows(results))
}
