#' Read in a ConQuest produced plausible values file
#'
#' @param fname A file path to an existing ConQuest plausible values file. Can take other input as for readr::read_file
#' @param np Number of plausible values per case. Default is 5.
#' @return A tibble containing parsed results of the file.
#'
#' @seealso \code{\link[sirt]{read.pv}} which also reads plausible values files. For reading in multidimensional pv files try
#' \code{\link[sirt]{read.multidimpv}} or \url{https://rdrr.io/rforge/eatRest/src/R/get.plausible.R}
#'
#' @examples
#' fname <- cq_example(display = FALSE, example_name = "ex1_05.pv")
#' df <- cq_pv(fname)
#'
#' @export
cq_pv <- function(fname, np = 5) {

  txt <- fname %>%
    readr::read_lines() %>%
    dplyr::as_tibble()

  x <- np + 3  # number of lines per case
  n_cases <- nrow(txt) / x # number of cases
  field_names <- c("pid", paste0("pv", seq(1, np)), "eap", "eap_se")

  txt <- txt %>%
    dplyr::mutate(recid = rep(seq(1, n_cases), each = x)) %>%
    dplyr::mutate(field = rep(field_names, n_cases)) %>%
    dplyr::mutate(value = trimws(value)) %>%
    tidyr::separate(value, c("index", "val"), "[[:space:]]+", fill = "left")

  txt
}





