#' Read in ConQuest show output file
#'
#' @param fname A file path to an existing ConQuest show file (e.g.
#'   'myfile.shw'). Can take other input as for \code{readr::read_file}
#'
#' @examples
#' fname <- cq_example(display = FALSE, example_name = "ex1.shw")
#' df <- cq_show(fname)
#'
#' @export
cq_show <- function(fname) {

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

  txt %>%
    tidyr::extract(.data$value, c("section_index"), paste0("^(", sections, ")"), remove = FALSE) %>%
    tidyr::fill(.data$section_index)

  txt
}
