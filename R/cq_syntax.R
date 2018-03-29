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
    label  = "label << {name}.lab;",
    format = "format {group_resp_cols} responses {resp_cols};",
    codes  = "{codes};",
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

