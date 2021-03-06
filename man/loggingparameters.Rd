% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loggingparameters.R
\name{loggingparameters}
\alias{loggingparameters}
\title{Advanced parameters of the logging simulator}
\usage{
loggingparameters(
  MinDBHValue = 10,
  MaxAreaSlope = 27,
  MaxTrailCenterlineSlope = 22,
  MaxTrailCrossSlope = 4,
  GrappleMaxslope = 20,
  TreeMaxSlope = 22,
  PlateauMaxSlope = 5,
  SlopeDistance = 6,
  WaterSourcesBufferZone = 30,
  MinMainTrailWidth = 5,
  MaxMainTrailWidth = 6,
  ScndTrailWidth = 4,
  BigTrees = 50,
  CableLength = 40,
  GrappleLength = 6,
  IsolateTreeMinDistance = 100,
  FutureTreesMinDiameter = 35,
  TreefallSuccessProportion = 0.6,
  MinTreefallOrientation = 30,
  MaxTreefallOrientation = 45,
  TreeHollowPartForFuel = 1/3,
  Purge = 0.14,
  MaxTrailDensity = 200,
  MaxLandingArea = 1500,
  TreeHarvestableVolumeAllometry = function(DBH, aCoef, bCoef) aCoef + bCoef *
    (DBH/100)^2,
  TrunkHeightAllometry = function(DBH, TreeHarvestableVolume) TreeHarvestableVolume/(pi
    * (((DBH/100)/2)^2)),
  TreeHeightAllometry = function(DBH) exp(0.07359191 + 1.34241216 * log(DBH) +
    -0.12282344 * log(DBH)^2),
  CrownDiameterAllometry = function(DBH, TreeHeight, alpha, beta) exp(((log(DBH) -
    alpha - rnorm(length(DBH), 0, 0.0295966977))/beta))/TreeHeight,
  RottenModel = function(DBH) 1/(1 + exp(-(-5.151 + 0.042 * DBH))),
  VisiblyDefectModel = function(LogDBH) 1/(1 + exp(-(-3.392 + 0.357 * LogDBH)))
)
}
\arguments{
\item{MinDBHValue}{Minimum DBH for inclusion in the forest inventory. Default = 10, in cm
(double)}

\item{MaxAreaSlope}{Maximum area slope for inclusion in a logging unit.
Default = 27, in \% (double)}

\item{MaxTrailCenterlineSlope}{Maximum main & 2nd trail centerline slope.
Default = 22, in \% (double)}

\item{MaxTrailCrossSlope}{Maximum main & 2nd trail cross slope. Default = 4,
in \% (double)}

\item{GrappleMaxslope}{Maximum slope accessible by the grapple. Default
= 20, in \% (double)}

\item{TreeMaxSlope}{Maximum slope around the tree to access it. Default = 22,
in \% (double)}

\item{PlateauMaxSlope}{Maximum slope to define an area as a plateau. Default = 5,
in \% (double)}

\item{SlopeDistance}{Distance over which the slope is calculated. Default = 6, in m (3m each
side) (double)}

\item{WaterSourcesBufferZone}{Buffer zone around the water sources. Default =
30, in m (double)}

\item{MinMainTrailWidth}{Minimum main trail width. Default = 5, in m (double)}

\item{MaxMainTrailWidth}{Maximum main trail width. Default = 6, in m (double)}

\item{ScndTrailWidth}{2nd trail width. Default = 4, in m (double)}

\item{BigTrees}{Minimum DBH of trees to be avoided by trails. Default =
50, in cm (double)}

\item{CableLength}{Cable length. Default = 40, in m (double)}

\item{GrappleLength}{Grapple length. Default = 6, in m (double)}

\item{IsolateTreeMinDistance}{Minimum distance to consider a tree "isolated"
from other trees of its species, in the aggregative species case
(\code{\link{SpeciesCriteria}}, 'Aggregative' column).
Default = 100, in m (double)}

\item{FutureTreesMinDiameter}{Future trees minimum diameter. Default = 35, in
cm (double)}

\item{TreefallSuccessProportion}{Proportion of successful directional felling
events. Default = 0.6 (double)}

\item{MinTreefallOrientation}{Minimum orientation of the tree fall to the
trail. Default = 30, in degree (double)}

\item{MaxTreefallOrientation}{Maximum orientation of the tree fall to the
trail. Default = 45, in degree (double)}

\item{TreeHollowPartForFuel}{Proportion of hollow trees used as fuel wood
. Default = 1/3 (double)}

\item{Purge}{Part of the log no used for timber, can be used for fuel wood. Default = 0.14, in m^3 of purge/m^3 of
volume of timber harvested. (double)}

\item{MaxTrailDensity}{Maximum trail density. Default = 200, in m/ha (double)
(has no impact on the simulation. A message will be sent to inform if
this threshold has been exceeded)}

\item{MaxLandingArea}{Maximum landing area. Default = 1500) in square meters
(double) (has no impact on the simulation. A message will be sent to inform if
this threshold has been exceeded)}

\item{TreeHarvestableVolumeAllometry}{By default, allometry of tree
harvestable volume, French Guiana ONF formula: aCoef + bCoef * (DBH/100)^2,
aCoef and bCoef depend on the forest location, stored in
\code{\link{ForestZoneVolumeParametersTable}}, DBH in cm. (function)}

\item{TrunkHeightAllometry}{Allometry of trunk height, based on the cylinder
volume formula: CylinderVolume = pi ((DBH/100)/2)^2 * H, with the height (H)
in m and the DBH in cm (function)}

\item{TreeHeightAllometry}{By default, allometry parameters estimated from
Guyanese data with the BIOMASS package: ln(H) = 0.07359191 + 1.34241216 *
ln(DBH) -0.12282344 * ln(DBH)^2, with the height (H) in m and the DBH in
cm (function)}

\item{CrownDiameterAllometry}{ln(DBH) = 𝜶+ 𝜷 ln(H*CD) + 𝜺, with 𝜺~N(0,σ^2)
and mean σ^2 = 0.0295966977 with the crown diameter (CD), the tree height
(H) in m, and the DBH in cm. (Aubry-Kientz et al.2019)(function)}

\item{RottenModel}{Estimates the tree probability of being probed hollow
(default: 1 / (1 + exp(-(-5.151 + 0.042  DBH))) with DBH in cm) (function)}

\item{VisiblyDefectModel}{Estimates the tree probability to have visible
defects. Default: 1 / (1 + exp(-(-3.392 + 0.357 * ln(DBH)))) with DBH in cm
(function)}
}
\value{
A named list of 30 objects.
}
\description{
Advanced parameters of the logging simulator
}
\examples{
loggingparameters(MinDBHValue = 5)

}
