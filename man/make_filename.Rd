% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Create filename}
\usage{
make_filename(year)
}
\arguments{
\item{year}{it can be one number (one year) or a vector of numbers (years), class numeric.}
}
\value{
it can be one string or list according to the inputs. The output of this function you use
as the input called filename in the function fards_read, class character.
}
\description{
This function has as an input a integer called year and create the filename input that you need in the
fards_read function.
}
\examples{
\dontrun{make_filename(year=c(2014,2015))}
\dontrun{make_filename(year=2013)}

}
