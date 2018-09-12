#' Read in ConQuest show output file
#'
#' @param fname A file path to an existing ConQuest show file (e.g.
#'   'myfile.shw'). Can take other input as for \code{readr::read_file}
#'
#' @param fit A logical value indicating if the show file has fit statistics
#'   included or not. This only affects warnings generated. It is convenient
#'   when no fit statistics have been requested as the default value \code{TRUE}
#'   generates warnings that data is missing.
#'
#' @examples
#' fname <- cq_example(display = FALSE, example_name = "ex1.shw")
#' df <- cq_show(fname)
#'
#' @export
cq_show <- function(fname, fit = c(TRUE, FALSE)) {

  sections <- paste(
    "SUMMARY OF THE ESTIMATION",
    "TABLES OF RESPONSE MODEL PARAMETER ESTIMATES",
    "TABLES OF POPULATION MODEL PARAMETER ESTIMATES",
    "MAP OF LATENT DISTRIBUTIONS AND RESPONSE MODEL PARAMETER ESTIMATES",
    "MAP OF LATENT DISTRIBUTIONS AND THRESHOLDS",
    sep = "|"
  )

  txt <- fname %>%
    readr::read_lines() %>%
    dplyr::as_tibble()

  txt <- txt %>%
    tidyr::extract(.data$value, c("section_index"), paste0("^(", sections, ")"), remove = FALSE) %>%
    tidyr::fill(.data$section_index)

  ## retaining parameter information
  stats_cols <- c("est", "err", "uw_mnsq", "uw_ci_lo", "uw_ci_hi", "uw_t",
                  "mnsq", "ci_lo", "ci_hi", "t")
  param_stats_cols <- c("index", "label", stats_cols)
  term_break <- "^--------------------------------------------------------------------------------$"


  # keep just the section on model parameters
  txt <- txt %>%
    dplyr::filter(.data$section_index == "TABLES OF RESPONSE MODEL PARAMETER ESTIMATES") %>%
    tidyr::extract(.data$value, "term", "^TERM (?:.+): (.+)", remove = FALSE) %>%
    tidyr::fill(.data$term) %>%
    dplyr::filter(!is.na(.data$term))

  # remove the header and footer info. Just keep the parameter rows
  txt <- txt %>%
    dplyr::group_by(.data$term) %>%
    dplyr::slice(seq(7, which(stringr::str_detect(.data$value, term_break)) - 1))

  # split line content into separate columns
  txt <- txt %>%
    dplyr::mutate(term_cols = stringr::str_split(.data$term, "\\*")) %>%
    dplyr::mutate(new_cols = purrr::map(
      .data$term_cols, ~ c(rbind(paste(.x, "index", sep = "_"), .x))
    )) %>%
    dplyr::mutate(value = trimws(.data$value)) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, function(x) {
      into_cols <- c(unlist(unique(x$new_cols)), stats_cols)
      x %>%
        tidyr::separate(.data$value, into_cols, "[\\s\\(\\),\\*]+",
                        convert = TRUE, fill = ifelse(fit, "warn", "right")) %>%
        dplyr::select(-term_cols, -new_cols)
    }))

  txt
}
