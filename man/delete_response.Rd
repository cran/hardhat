% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delete-response.R
\name{delete_response}
\alias{delete_response}
\title{Delete the response from a terms object}
\usage{
delete_response(terms)
}
\arguments{
\item{terms}{A terms object.}
}
\value{
\code{terms} with the response sections removed.
}
\description{
\code{delete_response()} is exactly the same as \code{delete.response()}, except
that it fixes a long standing bug by also removing the part of the
\code{"dataClasses"} attribute corresponding to the response, if it exists.
}
\details{
The bug is described here:

\url{https://stat.ethz.ch/pipermail/r-devel/2012-January/062942.html}
}
\examples{

framed <- model_frame(Species ~ Sepal.Width, iris)

attr(delete.response(framed$terms), "dataClasses")

attr(delete_response(framed$terms), "dataClasses")
}
