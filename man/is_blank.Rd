% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csvwr.R
\name{is_blank}
\alias{is_blank}
\title{Check for blank values}
\usage{
is_blank(value)
}
\arguments{
\item{value}{any vector}
}
\value{
a logical indicating if the value is blank
}
\description{
Is the value NA or an empty string `""`?
}
\examples{
\dontrun{
is_blank(c(NA, "", 1, TRUE, "a"))
}
}
