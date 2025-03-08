% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polygon_numbering.R
\name{polygon_enumerating}
\alias{polygon_enumerating}
\title{Function that creates a numbering for adjacent polygons.}
\usage{
polygon_enumerating(pol, idp = NULL)
}
\arguments{
\item{pol}{a sf polygon object, extended polygons that share a common boundary.}

\item{idp}{a character, name of id column in W.}
}
\value{
pol_num
}
\description{
This function generates a variable in a geographic object that ranges from 1
to n that follows the shape of a snake.
}
\details{
The polygon_enumerating function takes a geographic object as input, the centroids
 of the polygons are calculated and the numbering begins with the polygon whose
centroid is located most to the northeast, the numbering continues with the polygon
adjacent to the numbered polygon whose centroid is located most to the north.

The function returns the geographic object with a variable numbered from 1 to n.
}
\examples{
# polnum is created by running the psuR::polygon_enumarating function with the parameters set in its example.
polygon_enumerating(poly = ext_pol, idp = "idp")
}
\references{
Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas.
Valliant, R, et. al. (2013), \emph{Practical tools for Design and Weighting Survey Samples}. Springer
}
\author{
Angel Gaibor <mat.angel.gaibor at gmail.com>

Javier Núñez <mat.javier.nunez at gmail.com>
}
