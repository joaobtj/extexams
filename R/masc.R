#' masc Multiple Answer Single Choice Question
#'
#' Presents several statements. Judge each of them as correct or incorrect.
#' Tick the true alternative.
#'
#' @param statements A list containing the statements.Each element of the list
#' can be a vector, in which only one of the elements of the vector will be
#' randomly selected to compose the question.
#' @param statements_truth a list of logical values (TRUE/FALSE) of statements
#' @param explanation A list with the explanation (optional).
#' @param n_wrong Number of wrong alternatives. 4 by default.
#' @return A list with the randomized statements (character),
#' the alternatives (character) and the answers (logical).
#' @importFrom magrittr %>%
#' @importFrom knitr combine_words
#' @importFrom utils as.roman
#' @importFrom utils combn
#' @importFrom utils head
#' @importFrom stats na.omit
#' @importFrom glue glue
#' @examples
#' statements <- list(c("A"), c("D", "E"), c("G", "H", "I"))
#' statements_truth <- list(c(TRUE), c(FALSE, TRUE), c(FALSE, FALSE, TRUE))
#' explanation <- list(c("A is a vowel"), c("D is a consonant", "E is a vowel"),
#'  c("G is a consonant", "H is a consonant", "I is a vowel"))
#' masc(statements, statements_truth, explanation)
#' @export

masc <- function(statements, statements_truth, explanation = NULL, n_wrong = 4) {
  # checks----

  ## length of statements and statements_truth list
  if (length(statements) != length(statements_truth)) stop("The statements and statements_truth must be the same length.")

  ## length of statements and explanation list
  if (length(statements) != length(explanation)) stop("The statements and explanation must be the same length.")

  ## length of each element within statements and statements_truth list
  l <- c()
  for (i in seq_along(statements)) {
    l[i] <- length(statements[[i]]) == length(statements_truth[[i]])
  }
  if (isFALSE(all(l))) stop("Each element within the statements and statements_truth must be the same length.")

  ## length of each element within statements and explanation list
  l <- c()
  for (i in seq_along(statements)) {
    l[i] <- length(statements[[i]]) == length(explanation[[i]])
  }
  if (isFALSE(all(l))) stop("Each element within the statements and explanation must be the same length.")


  ## maximum number of wrong alternatives
  n_wrong_max <- sum(choose(length(statements), seq_along(statements))) - 1
  if (n_wrong > n_wrong_max) stop(glue::glue("Number of wrong alternatives is too big. n_wrong must not be grater than {n_wrong_max}"))

  # choose one statement from each vector----
  statements_choice <- c()
  statements_truth_choice <- c()
  explanation_choice=c()
  for (i in seq_along(statements)) {
    choice <- seq_along(statements[[i]]) %>% sample(1)
    statements_choice[i] <- statements[[i]][choice]
    statements_truth_choice[i] <- statements_truth[[i]][choice]
    explanation_choice[i]=explanation[[i]][choice]
    rm(choice)
  }


  ## reorder the chosen statements
  o <- sample(seq_along(statements))
  statements_choice <- statements_choice[o]
  statements_truth_choice <- statements_truth_choice[o]
  explanation_choice=explanation_choice[o]

  # create right alternatives----
  ## right with the "correct" word
  y_right_correct <- as.roman(seq_along(statements_choice))[which(statements_truth_choice)]
  right_correct <- ifelse(length(y_right_correct) == 1,
    glue::glue("Somente a alternativa {y} está correta.",
      y = y_right_correct
    ),
    glue::glue("As afirmativas {y} estão corretas.",
      y = knitr::combine_words(y_right_correct, and = " e ")
    )
  )

  ## right with the "incorrect" word
  y_right_incorrect <- as.roman(seq_along(statements_choice))[which(!statements_truth_choice)]
  right_incorrect <- ifelse(length(y_right_incorrect) == 1,
    glue::glue("Somente a afirmativa {y} está incorreta.",
      y = y_right_incorrect
    ),
    glue::glue("As afirmativas {y} estão incorretas.",
      y = knitr::combine_words(y_right_incorrect, and = " e ")
    )
  )

  ## choose one right alternative
  right <- c(right_correct, right_incorrect) %>%
    na.omit() %>%
    sample(1)


  # create wrong alternatives----
  ## all combinations with the word "correct"
  comb_correct <- c()
  for (i in seq_along(statements)) {
    comb_correct1 <- data.frame(combn(seq_along(statements), i))
    comb_correct <- merge(comb_correct, comb_correct1, by = 0, all = TRUE)[-1]
    rm(comb_correct1)
  }

  ## remove the right one with the word "correct"
  for (i in seq_along(comb_correct)) {
    z <- comb_correct[i] %>%
      na.omit() %>%
      unlist()
    if (length(z) != sum(statements_truth_choice)) next
    if (all(which(statements_truth_choice) == z)) {
      comb_correct <- comb_correct[-i]
      break
    }
  }

  ## wrong with the "correct" word
  wrong_correct <- list()
  for (i in seq_along(comb_correct)) {
    y_wrong_correct <- comb_correct[[i]] %>%
      na.omit() %>%
      unlist() %>%
      as.roman()
    wrong_correct[[i]] <- ifelse(length(y_wrong_correct) == 1,
      glue::glue("Somente a afirmativa {y} está correta.",
        y = y_wrong_correct
      ),
      glue::glue("As afirmativas {y} estão corretas.",
        y = knitr::combine_words(y_wrong_correct, and = " e ")
      )
    )
  }

  ## all combinations with the word "incorrect"
  cb_incorrect <- c()
  for (i in seq_along(statements)) {
    cb_incorrect1 <- data.frame(combn(seq_along(statements), i))
    cb_incorrect <- merge(cb_incorrect, cb_incorrect1, by = 0, all = TRUE)[-1]
    rm(cb_incorrect1)
  }

  ## remove the right one with the word "incorrect"
  for (i in seq_along(cb_incorrect)) {
    z <- cb_incorrect[i] %>%
      na.omit() %>%
      unlist()
    if (length(z) != sum(!statements_truth_choice)) next
    if (all(which(!statements_truth_choice) == z)) {
      cb_incorrect <- cb_incorrect[-i]
      break
    }
  }



  ## wrong with the "incorrect" word
  wrong_incorrect <- list()
  for (i in seq_along(cb_incorrect)) {
    y_wrong_incorrect <- cb_incorrect[[i]] %>%
      na.omit() %>%
      unlist() %>%
      as.roman()
    wrong_incorrect[[i]] <- ifelse(length(y_wrong_incorrect) == 1,
      glue::glue("Somente a afirmativa {y} está incorreta.",
        y = y_wrong_incorrect
      ),
      glue::glue("As afirmativas {y} estão incorretas.",
        y = knitr::combine_words(y_wrong_incorrect, and = " e ")
      )
    )
  }


  # sortear as alternativas wrong
  ## impedir alternativas mutuamente exclusivas
  if (length(wrong_correct) > length(wrong_incorrect)) wrong_correct <- wrong_correct %>% head(-1)
  if (length(wrong_correct) < length(wrong_incorrect)) wrong_incorrect <- wrong_incorrect %>% head(-1)

  r <- sample(length(wrong_correct), sample(seq_along(wrong_correct), 1))

  alternativas_choice <- c(right, sample(c(wrong_correct[r], rev(wrong_incorrect)[-r]), n_wrong)) %>%
    unlist()
  respostas_choice <- c(TRUE, rep(FALSE, n_wrong))

  ## reorder
  o <- sample(length(alternativas_choice))
  alternativas_choice <- alternativas_choice[o]
  respostas_choice <- respostas_choice[o]


  return(list(
    statements = statements_choice,
    explanation=explanation_choice,
    alternativas = alternativas_choice,
    respostas = respostas_choice
  ))
}
