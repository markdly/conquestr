# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Read in a traditional ConQuest item analysis file as text
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom utils data
#' @param fname A file path to an existing ConQuest itanal file. Can take other input as for readr::read_file
#' @return A tibble containing parsed results of the itanal file.
#'
#' @examples
#' fname <- cq_example(display = FALSE)
#' df <- cq_read_itanal(fname)
#'
#' @export
cq_read_itanal <- function(fname) {

  txt <- fname %>%
    readr::read_lines() %>%
    dplyr::as_tibble()

  txt <- txt %>%
    tidyr::extract(.data$value, c("item_index"), "^Item (\\d+)$", remove = FALSE) %>%
    tidyr::fill(.data$item_index) %>%
    dplyr::filter(!is.na(.data$item_index)) %>%  # remove header info
    dplyr::mutate(line_break =
                    cumsum(
                      stringr::str_detect(.data$value,
                                          "------------------------------------------------------------------------------"))) %>%
    dplyr::filter(.data$line_break < max(.data$line_break)) %>%
    dplyr::select(-.data$line_break) %>%
    tidyr::nest(.data$value)

  txt <- txt %>%
    dplyr::mutate(
      id        = purrr::map_chr(.data$data, ~ .$value[3]),
      case_disc = purrr::map_chr(.data$data, ~ .$value[4]),
      threshold = purrr::map_chr(.data$data, ~ .$value[5]),
      delta     = purrr::map_chr(.data$data, ~ .$value[6]),
      resp_stat = purrr::map(.data$data, ~ .$value[10:(nrow(.)-2)]))

  txt <- txt %>%
    tidyr::extract(
      .data$case_disc, c("case", "disc"),
      "Cases for this item +([0-9]+) +Discrimination +(-?[0-9]+\\.[0-9]+)$")

  txt <- txt %>%
    tidyr::extract(
      .data$threshold, c("thrsh", "mnsq"),
      "Item Threshold\\(s\\)\\: (.+) Weighted MNSQ (.+)$")

  txt <- txt %>%
    tidyr::extract(
      .data$delta, c("delta"),
      "Item Delta\\(s\\)\\: +(-?[0-9]+\\.[0-9]+)$")

  # response stats
  resp_col_names <- c("label", "score", "count", "pct_tot", "pt_bis", "t", "p",
                      "blank", "pv_avg", "pv_sd")
  txt <- txt %>%
    dplyr::mutate(resp_stat = purrr::map(.data$resp_stat, function(x) {
      x <- x %>%
        stringr::str_trim() %>%
        stringr::str_split("\\s+|\\(|\\)") %>%
        purrr::map(setNames, resp_col_names) %>%
        purrr::map_dfr(as.list) %>%
        dplyr::select(-.data$blank)
      x
    }))

  txt
}


#' Main function to read an itanal. If it doesn't work try using cq_read_itanal instead
#'
#' @param fname A file path to an existing ConQuest itanal file. Can take other input as for readr::read_file
#' @return A tibble containing itanal contents with numeric fields where appropriate.
#'
#' @examples
#' fname <- cq_example(display = FALSE)
#' df <- cq_itanal(fname)
#'
#' @export
cq_itanal <- function(fname) {
  df <- cq_read_itanal(fname)
  df %>%
    dplyr::mutate(id = stringr::str_trim(.data$id)) %>%
    dplyr::mutate_at(dplyr::vars(.data$case, .data$disc, .data$mnsq), as.numeric) %>%
    dplyr::mutate_at(dplyr::vars(.data$thrsh, .data$delta), as.numeric) %>% # assumes dichotomous for now
    dplyr::mutate(resp_stat = purrr::map(.data$resp_stat, function(x) {
      dplyr::mutate_at(x, dplyr::vars(-.data$label), as.numeric)
    }))
}


#' Extracts the response stats from an itanal into usable form
#'
#' @param itanal An itanal tibble returned by cq_read_itanal or cq_itanal
#' @return A tibble containing stats for each item response
#'
#' @examples
#' fname <- cq_example(display = FALSE)
#' df <- cq_itanal(fname)
#' cq_resp_stats(df)
#'
#' @export
cq_resp_stats <- function(itanal) {
  itanal %>%
    dplyr::select(.data$id, .data$resp_stat) %>%
    tidyr::unnest()
}


