% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read the Fatality Analysis Reporting System data into R}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string that gives the name of the file
to be read.}
}
\value{
This function returns a data frame.
}
\description{
This function reads the csv file into the local environment and returns
a data frame.
}
\note{
A warning is given if the file does not exist.
}
\examples{
fars_read(system.file("extdata", "accident_2013.csv.bz2", package = "mvfars"))

}
