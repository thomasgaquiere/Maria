#' HarvestableAreaDefinition
#'
#' @param plot Studied plots (sf data.frame)
#' @param dtm Digital terrain model (raster)
#' @param verticalcreekheight Relative elevation from nearest channel network (raster)
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return A collection of polygons defined as 1 : harvestable area / 0 : non-harvestable area
#'
#' @export
#'
#' @importFrom  sf st_as_sf st_cast st_set_crs
#' @importFrom  raster mask terrain rasterFromXYZ rasterToPolygons rasterToPoints
#' @importFrom  dplyr as_tibble left_join rename mutate if_else
#' @importFrom  magrittr %>%
#'
#' @examples
#' data(Plots)
#' data(DTMParacou)
#' data(VerticalCreekHeight)
#'
#'
#'HarvestablePolygons <- HarvestableAreaDefinition(plot = Plots,
#'                                                 dtm = DTMParacou,
#'                                                 verticalcreekheight = VerticalCreekHeight)
#'

HarvestableAreaDefinition <- function(plot,
                                      dtm,
                                      verticalcreekheight,
                                      advancedloggingparameters = loggingparameters()) {

  # Variables
  PlotSlope <- PlotSlopePoint <- CreekVHeightPlotPoint <- PlotTib <- NULL
  SlpCrit <- PlotSlopeCreekVHeight <- RasterHarvestable <- PolygonHarvestable <- NULL
  sf_PolygonHarvestable <- HarvestablePolygons <- CreekVHeight<- slope <-  NULL


  # Mask rasters by plot
  #PlotTopo <- raster::mask(x = dtm,
                        #   mask = plot) # Mask topography raster by plot
  #CreekVHeightPlot <- raster::mask(x = verticalcreekheight,
                           #   mask = plot) # Mask verticalcreekheight raster by plot

  # Slope Calculation
  PlotSlope <- terrain(dtm,
               opt = "slope",
               units = 'radians',
               neighbors = 8)
  # RastersToPoints

  PlotSlopePoint <-
    as_tibble(rasterToPoints(PlotSlope))

  CreekVHeightPlotPoint <-
    as_tibble(rasterToPoints(verticalcreekheight))

  # Join tibbles by x and y
  PlotTib <-
    left_join(PlotSlopePoint, CreekVHeightPlotPoint, by = c('x', 'y'))

    SlpCrit <- atan(advancedloggingparameters$MaxAreaSlope/100)

  PlotTib %>% rename("CreekVHeight" = names(PlotTib[4]))  %>%
    mutate(Harvestable = if_else(
      condition = CreekVHeight > 2 &
        slope <= SlpCrit ,
      true = 1,
      false = 0
    )) -> PlotSlopeCreekVHeight # Identify harvestable area (1) /  non-harvestable area (0) by slope and Creek Vertical Height




  # transform tibble to raster
  RasterHarvestable <-
    rasterFromXYZ(PlotSlopeCreekVHeight, crs = raster::crs(dtm)) # set crs to WGS84 UTM 22N

  # raster to polygon
  PolygonHarvestable <-
    rasterToPolygons(x = RasterHarvestable$Harvestable,
                     n = 16,
                     dissolve = TRUE)



  sf_PolygonHarvestable <- st_as_sf(PolygonHarvestable) # transform PolygonExploit to an sf object

  # Disaggregate PolygonExploit

  HarvestablePolygons <-
    st_cast(x = sf_PolygonHarvestable, to = "POLYGON", warn=FALSE)



  return(HarvestablePolygons)

}

