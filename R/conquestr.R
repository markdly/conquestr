#' conquestr: A package for exchanging information with ConQuest software
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom utils data
#'
#'
#' @section About: The conquestr package provides two categories of important
#'   functions
#'   1. functions for exporting information to ConQuest, and
#'   2. functions for importing ConQuest results output.
#'
#' @section Export functions: The main function is \code{cq_analysis} which is used
#'   to prepare ConQuest command syntax and files for analysis based on a
#'   supplied dataframe.
#'
#' @section Import functions: These functions import the results of a ConQuest
#'   analysis.  These include \code{cq_itanal} to import item analysis files \code{cq_pv}
#'   to import plausible values files
#'
#' @references Wu, M., Adams, R., Wilson, M., & Haldane, S. (2007). ConQuest Version 2: Generalised Item Response Modelling Software.
#'
#' @docType package
#' @name conquestr
NULL
