% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cq_syntax.R
\name{cqc_label}
\alias{cqc_label}
\title{Make ConQuest label text}
\usage{
cqc_label(x, variable = "item")
}
\arguments{
\item{x}{a character vector containing labels}

\item{variable}{the ConQuest variable associated with labels. Default is `items`. Other options might be `rater` etc.}
}
\value{
a character vector which can be written to a file for use by the ConQuest label command. levels are assigned in order of the provided labels
}
\description{
Produces text compatible with the ConQuest label command.
e.g. text is multiline containing
`    ===> item` (where item is the conquest variable name)
`    1 it01` (1 is the level, it01 is the label for level 1)
`    2 it02` (2 is the level, it02 is the label for level 2)
}
\examples{
labels <- names(short_test)[-1]
cqc_label(labels)

writeLines(cqc_label(labels))

}
