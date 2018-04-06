#' conquestr: A package for exchanging information with ConQuest software
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom utils data
#'
#'
#' @section The foo package provides two categories of important functions:
#'  Functions for exporting information to ConQuest, and
#'  Functions for importing ConQuest output
#'
#' @section export functions: The main function is `cq_analysis` which is used to prepare ConQuest command syntax and files for
#'   analysis based on a supplied dataframe.
#'
#' @section import functions: These functions import the results of a ConQuest analysis.  These include
#'   `cq_itanal` to import item analysis files
#'   `cq_pv` to import plausible values files
#'
#' @references Wu, M., Adams, R., Wilson, M., & Haldane, S. (2007). ConQuest Version 2: Generalised Item Response Modelling Software.
#'
#' @docType package
#' @name conquestr
NULL
