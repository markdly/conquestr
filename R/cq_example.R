#' Example ConQuest output files
#'
#' @param n the number of lines to print. only used when \code{display = TRUE}.
#' @param display When \code{TRUE} (the default) the example output is read and the first \code{n} lines are printed.
#' The file path to example conquest output file is returned invisibly.
#' Use \code{FALSE} if just the conquest output file path is needed. No output will be displayed.
#' @param name Specifies the example output file to use. Default is \code{"ex1.itn"}. Other options are
#' \code{"ex1_05.pv"} and \code{"ex1_10.pv"} which requests the plausible values example file with 5 or 10 pvs per case respectively.
#' @return Invisibly returns a valid file path to conquest output file which can be used with other conquestr functions.
#'
#' @examples
#' library(readr)
#' fname <- cq_example(display = FALSE)
#' fname
#' cq_example(n = 7)
#'
#' @export
cq_example <- function(n = 10, display = TRUE, example_name = c("ex1.itn", "ex1_05.pv", "ex1_10.pv")) {
  example_name <- match.arg(example_name)
  fname <- system.file("extdata", example_name, package = "conquestr")
  if (display) {
    x <- readr::read_lines(fname)
    cat(paste("Example itanal for first n =", n, "lines is:\n\n"))
    print(utils::head(x, n))
  }
  invisible(fname)
}


#' cq_example_itanal (deprecated)
#'
#' \code{cq_example_itanal} has been deprecated and may be removed from future versions.
#' Please use \code{cq_example} instead.
#'
#' @export
cq_example_itanal <- function(...) {
  .Deprecated("cq_example")
  cq_example(...)
}

