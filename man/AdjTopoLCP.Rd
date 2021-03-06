% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AdjTopoLCP.R
\name{AdjTopoLCP}
\alias{AdjTopoLCP}
\title{Assymetric adjacent matrix T*topographic least cost paths and distances}
\usage{
AdjTopoLCP(
  DTM,
  costSurface,
  slopeRdCond,
  pts,
  directions = 8,
  paths = TRUE,
  zweight = 1,
  advancedloggingparameters = loggingparameters()
)
}
\arguments{
\item{DTM}{A RasterLayer for digital terrain model (DTM) data.}

\item{costSurface}{A RasterLayer for the conductance (inverse of resistance) values for each cell.}

\item{slopeRdCond}{A categorical transition layer of longitudinal and transversal slope.}

\item{pts}{A SpatialPointsDataFrame object or two-column matrix with xy coordinates for the geographic points from which to calculate pairwise distances and paths.}

\item{directions}{numeric (default = 8). The number of directions for movement between cells, either 4 or 8.}

\item{paths}{logical. Default is FALSE, in which case only topographic distances are calculated.  If TRUE, topographic paths are also identified.}

\item{zweight}{numeric (default = 1). The weight to be applied to the elevation (z) distances relative to the horizontal (xy) distances.}

\item{advancedloggingparameters}{Other parameters of the logging simulator}
}
\value{
Matrix of topographic distances (if paths = FALSE), or a list containing a matrix of topographic distances and the topographic paths as an object of class SpatialLines (if paths = TRUE).
}
\description{
Calculates topographic least cost distances and paths with an assymetric adjacent matrix
}
\details{
The values of the raster for costSurface should be conductance values rather than resistance values.
These can be calculating by taking the inverse of resistance values.
}
