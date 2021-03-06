% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cq_syntax.R
\name{cqc_data}
\alias{cqc_data}
\title{Make ConQuest fixed width text file}
\usage{
cqc_data(x, fname, item_names = names(x), extras = NULL)
}
\arguments{
\item{x}{a dataframe containing responses for analysis. Optionally includes additional variables for analysis.}

\item{fname}{a filename for exporting data to}

\item{item_names}{character vector containing item names
Each item should appear as a variable in `x`}

\item{extras}{character variable containing extra variables for writing out}
}
\value{
writes a fixed width text file to `fname`. returns a dataframe containing column specifications
}
\description{
Produces a fixed width text file for use with the ConQuest data command.
}
\examples{
cqc_data(as.data.frame(short_test)[-1], tempfile(fileext = ".txt"))

}
