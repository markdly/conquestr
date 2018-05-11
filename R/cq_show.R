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
  param_stats_cols <- c("est", "err", "uw_mnsq", "uw_ci_lo", "uw_ci_hi", "uw_t",
                        "mnsq", "ci_lo", "ci_hi", "t")
  term_break <- "^--------------------------------------------------------------------------------$"


  # keep just the section on model parameters
  txt <- txt %>%
    dplyr::filter(section_index == "TABLES OF RESPONSE MODEL PARAMETER ESTIMATES") %>%
    tidyr::extract(.data$value, "term", "^TERM (.+)", remove = FALSE) %>%
    tidyr::fill(.data$term) %>%
    dplyr::filter(!is.na(term))

  # remove the header and footer info. Just keep the parameter rows
  txt <- txt %>%
    group_by(term) %>%
    slice(seq(7, which(stringr::str_detect(.$value, term_break)) - 1))

  # split text into two groups: one containing the term(s). the second containing stats
  txt <- txt %>%
    mutate(value = trimws(value)) %>%
    tidyr::separate(value, c("params", "stats"), "\\s{7,}")  # split on the 'big space' first

  # one column for each stat
  txt <- txt %>%
    mutate(stats = trimws(stats)) %>%
    tidyr::separate(stats, param_stats_cols, "[\\s\\(\\),\\*]+", convert = TRUE, fill = ifelse(fit, "warn", "right"))

  txt
}
