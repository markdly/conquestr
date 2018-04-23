#' List of ConQuest commands
#'
#' @return A named list. Each element contains a string containing a ConQuest syntax statement with placeholders
#'
#' @examples
#' cqc_cmds()
#'
#' @export
cqc_cmds <- function() {
  list(
    reset  = "reset;",
    title  = "title {name} {title};",
    data   = "data {filename}.txt;",
    format = "format {group_resp_cols} responses {resp_cols};",
    label  = "label << {name}.lab;",
    codes  = "codes {codes};",
    key    = "key {key} ! {score};",
    model  = "model {model};",
    regression = "regression {regression_groups};",
    estimate = "estimate ! iter={iter}, nodes={nodes};",
    itanal = "itanal >> {name}.itn;",
    show   = "show ! estimates=latent >> {name}.shw;",
    plot_icc = "plot icc! gins=all,estimates=latent,bins=5,filesave=yes >> {plot_icc_path}{name};",
    plot_mcc = "plot mcc! gins=all,estimates=latent,bins=5,filesave=yes >> {plot_mcc_path}{name}; set warnings=no;",
    plot_dif = "plot expected! Gins=all,estimates=latent,bins=5,group={dif_group},filesave=yes,keep=0:1 >> {plot_dif_path}{name}_DIF;",
    logfile = "export logfile >> {name}.CQL;",
    put     = "put >> {name}.CQS;")
}

#' List of default arguments for use with ConQuest commands
#'
#' @return A named list. Each element contains a default placeholder value. Designed for use with `cqc_cmds`
#'
#' @examples
#' cqc_defaults()
#'
#' @export
cqc_defaults <- function() {
  list(
    title = NULL, group_resp_cols = "", codes = NULL,
    key = NULL, score = 1L, regression_groups = NULL,
    model = "item",
    iter = 1000L, nodes = 30L, plot_icc_path = NULL,
    plot_mcc_path = NULL, dif_group = NULL,
    plot_dif_path = NULL)
}

#' Generate ConQuest syntax, with some reasonable defaults
#'
#' @param name A name for the analysis - this is used to link data, labels and output files
#' @param resp_cols Columns containing item responses. For use with ConQuest format statment
#' @param filename Name to use for the data file. Defaults to the value supplied to the \code{name} argument
#' @param cmds A list of strings. Each string containing a ConQuest statment (and may include placeholders)
#' @param lookup_vals A named list of strings. The name defines a 'placeholder' for use with ConQuest commands. The string is the value that will be used instead of the placeholder
#' @return A named list. Each element contains a Conquest command. Use with `cqc_cmds` and `cqc_defaults`
#'
#' @examples
#' # cqc_syntax only generates text which can be used by ConQuest. e.g.
#' cqc_syntax(name = "mydata", resp_cols = "1-10")
#'
#'
#' @export
cqc_syntax <- function(name, resp_cols, filename = name, cmds = cqc_cmds(), lookup_vals = cqc_defaults()) {
  lookup_vals <- c(name = name, resp_cols = resp_cols, filename = filename, lookup_vals)
  glued <- purrr::map(cmds, ~ glue::glue_data(lookup_vals, .))
  glued <- purrr::discard(glued, ~ length(.) == 0)
  glued
}


#' Make ConQuest label text
#'
#' Produces text compatible with the ConQuest label command.
#' e.g. text is multiline containing
#' `    ===> item` (where item is the conquest variable name)
#' `    1 it01` (1 is the level, it01 is the label for level 1)
#' `    2 it02` (2 is the level, it02 is the label for level 2)
#'
#' @param x a character vector containing labels
#' @param variable the ConQuest variable associated with labels. Default is `items`. Other options might be `rater` etc.
#' @return a character vector which can be written to a file for use by the ConQuest label command. levels are assigned in order of the provided labels
#'
#' @examples
#' labels <- names(short_test)[-1]
#' cqc_label(labels)
#'
#' writeLines(cqc_label(labels))
#'
#' @export
cqc_label <- function(x, variable = "item") {

  c(paste("===>", variable), paste(seq_along(x), x))

}


#' Make ConQuest fixed width text file
#'
#' Produces a fixed width text file for use with the ConQuest data command.
#'
#' @param x a dataframe containing responses for analysis. Optionally includes additional variables for analysis.
#' @param fname a filename for exporting data to
#' @param item_names character vector containing item names
#' Each item should appear as a variable in `x`
#' @param extras character variable containing extra variables for writing out
#' @return writes a fixed width text file to `fname`. returns a dataframe containing column specifications
#'
#' @examples
#' cqc_data(as.data.frame(short_test)[-1], tempfile(fileext = ".txt"))
#'
#' @export
cqc_data <- function(x, fname, item_names = names(x), extras = NULL) {
  if(!all(item_names %in% names(x))) stop("not all item_names provided exist in x")
  if(!all(extras %in% names(x))) stop("not all extras provided exist in x")
  if(length(intersect(item_names, extras)) > 0) stop("some item_names also appear in extras")
  x <- x[unique(c(item_names, extras))]
  specs <- gdata::write.fwf(x, fname, colnames = FALSE, sep = "", na = "", formatInfo = TRUE)
  if(max(specs$width[specs$colname %in% item_names]) > 1) warning("some items had width greater than one")
  specs
}



#' Condense ConQuest response columns syntax
#'
#' Designed to work with \code{cqc_data}. This is a convenience functions
#' designed for use when a subset of items are being analysed that are contained
#' in the ConQuest data file.
#'
#' @param x takes an integer vector (or attempts to coerce to one) which
#'   represents column positions for ConQuest items in a fixed-width text file
#'   used for analysis.
#' @return a string containing response columns for use with the ConQuest format statement
#'
#' @examples
#' x <- c(1,3,4,5,7,9,10,11)
#' cqc_resp_cols(x)
#'
#' @export
cqc_resp_cols <- function(x) {
  e1 <- simpleError("Response columns were not unique.")
  w1 <- simpleWarning("Response columns were not in ascending order and could not be condensed.")

  if(length(unique(x)) != length(x)) { stop(e1) }

  diffs <- c(1, diff(x))
  if(any(diffs < 1)) {
    warning(w1)
    return(paste0(x, collapse=", "))
  }

  start_indexes <- c(1, which(diffs > 1))
  end_indexes <- c(start_indexes - 1, length(x))[-1]
  dashed <- paste(x[start_indexes], x[end_indexes], sep="-")
  # remove the dash when the start and end index are the same
  indexes_diff = end_indexes - start_indexes
  dashed[indexes_diff == 0] <- paste(x[start_indexes[indexes_diff == 0]])
  return(paste0(dashed, collapse=", "))
}

