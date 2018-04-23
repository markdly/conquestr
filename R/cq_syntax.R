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
    data   = "data {name}.txt;",
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
cqc_syntax <- function(name, resp_cols, cmds = cqc_cmds(), lookup_vals = cqc_defaults()) {
  lookup_vals <- c(name = name, resp_cols = resp_cols, lookup_vals)
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
