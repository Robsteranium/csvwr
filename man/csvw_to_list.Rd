% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csvwr.R
\name{csvw_to_list}
\alias{csvw_to_list}
\title{Convert a csvw metadata to a list (csv2json)}
\usage{
csvw_to_list(csvw)
}
\arguments{
\item{csvw}{a csvw metadata list}
}
\value{
a list following the csv2json translation rules
}
\description{
Convert a csvw metadata to a list (csv2json)
}
\examples{
\dontrun{
csvw_to_list(read_csvw("example.csv"))
}
}
