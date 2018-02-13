#' Unscored student response data.
#'
#' A dataset containing the responses of 1000 students to 9 multiple choice test items
#'
#' @format A tibble with 1475 rows and 10 variables:
#' \describe{
#'   \item{pid}{a unique person identifier}
#'   \item{q1:q9}{q1 to q9 are multiple choice test items, \code{NA} indicates no response was provided}
#' }
#' @source \url{http://www.smeebu.com/}
"short_test"


#' Correct answer key for \code{data(short_test)}
#'
#' A tibble containing the correst answer option for the responses provided in \code{data(short_test)}
#'
#' @format A tibble with 9 rows and 2 variables:
#' \describe{
#'   \item{item}{a unique item id}
#'   \item{resp}{the correct resp value for the corresponding item}
#' }
#' @source \url{http://www.smeebu.com/}
"short_test_score_key"
