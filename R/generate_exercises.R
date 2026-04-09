#' Generate Randomized Academic Exercises
#'
#' @title Generate Batch Exercises
#' @description
#' Generates multiple randomized copies of academic exercises based on a
#' specific seed derived from the semester. Ideal for weekly practice assignments.
#'
#' @param exercises Character vector of exercise file names (without .Rmd).
#' @param course Character string for the course name.
#' @param semester Character string for the academic semester (e.g., "1999.1").
#' @param n_exerc Numeric integer. Number of random copies to generate.
#' @param templates Character vector of LaTeX template names located in the package's tex folder.
#' @param decimal Character string for the decimal separator (default is ",").
#' @param output_dir Character string specifying the directory for output files.
#' @param ... Additional arguments passed to `exams::exams2pdf`.
#'
#' @return A data frame containing the metadata and solutions (replication, file, and string).
#' @family exam_generation
#' @author Joao Batista Tolentino Jr. \email{joao.tolentino@@ufsc.br}
#'
#' @importFrom dplyr any_of bind_rows select
#' @importFrom exams exams2pdf exams_metainfo
#' @importFrom glue glue
#' @importFrom purrr map
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generating 5 personalized exercises for an Irrigation class
#' report <- generate_exercises(
#'   exercises = c("anova", "fruit"),
#'   n_exerc = 5, decimal = "."
#' )
#' (report)
#' }
generate_exercises <- function(
  exercises,
  course = "Course",
  semester = "1999.1",
  n_exerc = 1,
  templates = c("exercicio", "gabarito"),
  decimal = ",",
  output_dir = NULL,
  ...
) {
  # Safely modify the global decimal separator and ensure it reverts on function exit
  old_opt <- options(OutDec = decimal)
  on.exit(options(old_opt), add = TRUE)

  if (is.null(output_dir)) {
    output_dir <- glue::glue("gerada_{course}_{semester}")
  }

  base_seed <- as.numeric(gsub("[^0-9]", "", as.character(semester)))

  # Map over the index of exercises to generate files and capture metadata
  results <- seq_along(exercises) |>
    purrr::map(\(file_idx) {
      # Internal generator for unique, alphanumeric student IDs (e.g., "42K891201")
      getID <- function(rep_num) {
        # Cache and isolate the global RNG state. This prevents the ID generation
        # from advancing the global seed, which would desynchronize the question variations.
        if (exists(".Random.seed", envir = .GlobalEnv)) {
          old_seed <- get(".Random.seed", envir = .GlobalEnv)
          on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
        }

        set.seed(base_seed + file_idx * 1000 + as.numeric(rep_num))

        paste0(
          sample(10:99, 1),
          sample(LETTERS, 1),
          sample(1000:9999, 1),
          gsub(" ", "0", format(as.numeric(rep_num), width = 2))
        )
      }

      header <- list(Course = course, Semester = semester, ID = getID)

      # Force a consistent seed right before rendering so `exams2pdf` draws identical questions
      set.seed(base_seed)

      p <- exams::exams2pdf(
        file = paste0(exercises[[file_idx]], ".Rmd"),
        n = n_exerc,
        template = system.file("tex", paste0(templates, ".tex"), package = "extexams"),
        header = header,
        name = glue::glue("{templates}_{exercises[[file_idx]]}_{semester}_{course}_"),
        dir = output_dir,
        ...
      )

      # Extract metadata and select relevant columns directly inside the map loop
      meta <- exams::exams_metainfo(p, class = "data.frame", tags = TRUE)
      meta_clean <- dplyr::select(meta, dplyr::any_of(c("replication", "file", "string")))

      return(meta_clean)
    })

  # Bind all exercise metadata into a single tidy data frame
  return(dplyr::bind_rows(results))
}
