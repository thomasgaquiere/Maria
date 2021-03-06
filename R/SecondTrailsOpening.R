#' secondtrailsopening
#'
#' @param DTM Digital terrain model (raster)
#' @param plots Studied plots (SpatialPolygonsDataFrame)
#' @param treeselectionoutputs A list with:
#'  - your inventory with: "DistCrit", "Slope", "SlopeCrit",
#'  "LoggingStatus", "Selected", "Up", "VolumeCumSum", "ProbedHollowProba",
#'  "ProbedHollow" new columns (see the outputs metadata in the \code{\link{vignette}}).
#'  - the objective volume with or without a bonus (if hollow trees
#'  exploitation)
#'  - 6 sets of spatial points: harvestable, selected, future and
#'  reserve, hollow and energy wood trees
#' @param verticalcreekheight Relative elevation from nearest channel network
#' (Large RasterLayer)
#' @param CostMatrix List of list defining conditional weight over binned slopes values
#' @param scenarios The chosen scenario from scenariosparameters function
#' @param fact Aggregation factor of cost raster resolution to initial DTM
#' @param advancedloggingparameters Advanced parameters of the logging simulator
#'
#' @return Secondary trails polygons according to scenario conditions.
#'
#' @importFrom  sf st_cast st_as_sf st_intersection st_union st_sample st_join st_buffer as_Spatial st_centroid st_set_precision st_make_valid st_set_agr st_geometry st_area st_is_empty st_set_crs st_crs
#' @importFrom dplyr mutate row_number select as_tibble left_join if_else filter arrange desc
#' @importFrom raster raster extend extent focal res crs mask crop rasterize rasterToPoints rasterToPolygons rasterFromXYZ aggregate values ncell values<-
#' @importFrom sp  proj4string<- coordinates<-
#'
#' @export
#'
#' @examples
#' data(DTMParacou)
#' data(Paracou6_2016)
#' data(HarvestablePolygons)
#' data(MainTrails)
#' data(Plots)
#' data(PlotSlope)
#' data("SpeciesCriteria")
#' data(VerticalCreekHeight)
#'
#' scenarios <- scenariosparameters(scenario = "RIL3")
#'
#' inventory <- ONFGuyafortaxojoin(addtreedim(inventorycheckformat(Paracou6_2016),
#'  volumeparameters = ForestZoneVolumeParametersTable),SpeciesCriteria)
#'
#' AccessPolygons <- FilterAccesExplArea(harvestablepolygons = HarvestablePolygons,
#' maintrails = MainTrails,
#' advancedloggingparameters = loggingparameters())
#'
#' treeselectionoutputs <- treeselection(inventory,
#' topography = DTMParacou, plotslope = PlotSlope, maintrails = MainTrails,
#' harvestablepolygons = HarvestablePolygons,
#' speciescriteria = SpeciesCriteria, objective = scenarios$objective,
#' scenario = "RIL3",
#' objectivelax = TRUE,
#' advancedloggingparameters = loggingparameters())
#'
#' secondtrails <- secondtrailsopening(DTM = DTMParacou,
#'    plots = Plots,
#'
#'    treeselectionoutputs = treeselectionoutputs,
#'    verticalcreekheight = VerticalCreekHeight,
#'    CostMatrix = list(list(list(Slope = 3, Cost = 3),
#'                            list(Slope = 5, Cost = 5),
#'                            list(Slope = 12, Cost = 20),
#'                            list(Slope = 22, Cost = 60),
#'                            list(Slope = 27, Cost = 600),
#'                            list(Slope = Inf, Cost = 1000)),
#'                       list(list(CostType = "Initial", CostValue = 1000),
#'                            list(CostType = "Access", CostValue = 10000),
#'                            list(CostType = "BigTrees", CostValue = 500),
#'                            list(CostType = "Reserves", CostValue = 500),
#'                            list(CostType = "Futures", CostValue = 50),
#'                            list(CostType = "maintrails", CostValue = 1E-4),
#'                            list(CostType = "SecondTrails", CostValue = 0.1))),
#'  scenarios = scenarios,
#'  fact = 3,
#'  advancedloggingparameters = loggingparameters())
#'
secondtrailsopening <- function(DTM,
                                plots,
                                verticalcreekheight,
                                treeselectionoutputs,
                                #maintrails,
                                CostMatrix = list(list(list(Slope = 3, Cost = 3),
                                                       list(Slope = 5, Cost = 5),
                                                       list(Slope = 12, Cost = 20),
                                                       list(Slope = 22, Cost = 60),
                                                       list(Slope = 27, Cost = 600),
                                                       list(Slope = Inf, Cost = 1000)),
                                                  list(list(CostType = "Initial", CostValue = 1000),
                                                       list(CostType = "Access", CostValue = 1000),
                                                       list(CostType = "BigTrees", CostValue = 500),
                                                       list(CostType = "Reserves", CostValue = 500),
                                                       list(CostType = "Futures", CostValue = 50),
                                                       list(CostType = "maintrails", CostValue = 1E-4),
                                                       list(CostType = "SecondTrails", CostValue = 0.1))),
                                scenarios = NULL,
                                fact = 3,
                                advancedloggingparameters = loggingparameters()) {

  # Arguments check

  if(!inherits(treeselectionoutputs, "list"))
    stop("The 'treeselectionoutputs' arguments of the 'secondtrailsopening' function must be list following treeselection output format")

  if(!any(unlist(lapply(list(plots), inherits, "SpatialPolygonsDataFrame"))))
  stop("The 'plots' argument of the 'secondtrailsopening' function must be SpatialPolygonsDataFrame")

  # if(!any(unlist(lapply(list(maintrails), inherits, "sf" ))))
  #   stop("The 'maintrails' argument of the 'secondtrailsopening' function must be sf polygon")

  if(!any(unlist(lapply(list(DTM), inherits, "RasterLayer"))))
    stop("The 'DTM' argument of the 'secondtrailsopening' function must be raster")

  if (st_is_empty(treeselectionoutputs$SelectedTreesPoints)[1]) {
    stop("The 'treeselectionoutputs' argument does not contain any selected tree.")
  }

  # Global Variables
  slope <- x <- y <- Harvestable <- idTree <- ID <- type <- ptAcc <- plotslope <- NULL
  EstCost <- n.overlaps <- TypeAcc <- IDpts <- Logged <- harvestablepolygons<- HarvestableAreaDefintionOutputs <- NULL

  ##################################

# Transformation of the DTM so that the maintrails are outside the plot

  DTMExtExtended <- raster::extend(DTM, c(1,1)) # extend the extent

  fill.boundaries <- function(x, i=5) { # function to be integrated in the focal
    if( is.na(x[i]) ) {
      return( round(mean(x, na.rm=TRUE),3) )
    } else {
      return( x[i] )
    }
  }

  DTMExtended <- raster::focal(DTMExtExtended,
                               matrix(1,3,3),
                               fun=fill.boundaries,
                               na.rm=F, pad=T)

# Transformation of vertical creek height raster

  VerticalCreekHeightExtExtended <- raster::extend(verticalcreekheight, c(1,1))

  VerticalCreekHeightExtended <- raster::focal(VerticalCreekHeightExtExtended,
                                               matrix(1,3,3),
                                               fun=fill.boundaries,
                                               na.rm=F, pad=T)

# Set maintrails outside the plot

  premaintrails <- DTMExtended > -Inf
  premaintrails<- rasterToPolygons(premaintrails, dissolve=T)
  maintrails <- premaintrails %>% st_as_sf() %>% st_cast(to = 'LINESTRING', warn= FALSE)



# Generate accessible area from HarvestablePolygones and winching choice


  HarvestableAreaDefintionOutputs <- HarvestableAreaDefinition(topography = DTMExtended, verticalcreekheight = VerticalCreekHeightExtended)

  harvestablepolygons <- HarvestableAreaDefintionOutputs[[1]]

  plotslope <- HarvestableAreaDefintionOutputs[[2]]

  AccessPolygons <- FilterAccesExplArea(harvestablepolygons = harvestablepolygons,maintrails = maintrails,advancedloggingparameters = loggingparameters())

  # Generate accessible area from HarvestablePolygones and winching == "0"
  Accessmaintrails <- FilterAccesExplArea(harvestablepolygons = harvestablepolygons,maintrails = maintrails,advancedloggingparameters = loggingparameters()) %>%
    st_cast("POLYGON") %>%
    st_as_sf() %>%
    mutate(ID = paste0("ID_",row_number()))




  # Generate intersections between accessible area and Maintrails (ID = accessible area index)
  Partmaintrails <- st_intersection(st_geometry(maintrails)  ,
                                    st_geometry(Accessmaintrails %>%
                                      st_buffer(dist = raster::res(DTM)))) %>%
    st_buffer(dist = raster::res(DTM)) %>%
    st_union(by_feature = T) %>%
    st_intersection(st_as_sf(plots) %>% st_union()) %>%
    st_cast("MULTIPOLYGON")  %>%
    st_as_sf() %>%
    st_set_agr(value = "constant") %>%
    st_join(Accessmaintrails, join = st_intersects)

  # Generate point access in the intersections between accessible area and Maintrails (ID = accessible area index)
  AccessPoint <- Partmaintrails  %>%
    st_sample( rep(1,dim(Partmaintrails)[1]) ,type = "random", by_polygon=TRUE) %>% st_as_sf() %>%
    st_join(Partmaintrails, join = st_intersects) %>%
    mutate(idTree = NA)

 # Generate Cost raster --> cf CostMatrix
  CostRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(CostRaster) <- CostMatrix[[2]][[1]]$CostValue
  CostRaster <- mask(CostRaster, plots)
  CostRaster <- crop(CostRaster, plots)


  #Generate weight according to slope conditions
  # RastersToPoints

  plotslopePoint <- as_tibble(rasterToPoints(plotslope))

  CostRasterPoint <- as_tibble(rasterToPoints(CostRaster))

  # left join par x et y
  plotTib <-
    left_join(plotslopePoint, CostRasterPoint, by = c('x', 'y'))

  CostSlopeRaster <- plotTib %>%
    mutate(Harvestable = if_else(
      slope <= atan(CostMatrix[[1]][[1]]$Slope/100),
      true = CostMatrix[[1]][[1]]$Cost,
      false = if_else(
        slope <= atan(CostMatrix[[1]][[2]]$Slope/100),
        true = CostMatrix[[1]][[2]]$Cost,
        false = if_else(
          slope <= atan(CostMatrix[[1]][[3]]$Slope/100),
          true = CostMatrix[[1]][[3]]$Cost,
          false = if_else(
            slope <= atan(CostMatrix[[1]][[4]]$Slope/100),
            true = CostMatrix[[1]][[4]]$Cost,
            false = if_else(
              slope <= atan(CostMatrix[[1]][[5]]$Slope/100),
              true = CostMatrix[[1]][[5]]$Cost,
              false = CostMatrix[[1]][[6]]$Cost)
          )
        )
      )
    )) %>% select(x,y,Harvestable)

  CostRaster <- rasterFromXYZ(CostSlopeRaster, crs = crs(DTMExtended)) # Update weights from plotTib tibble

  #Generate accessible weights raster
  AccessRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(AccessRaster) <- CostMatrix[[2]][[2]]$CostValue

  AccessRaster <- rasterize(x = as_Spatial(AccessPolygons %>%
                                             st_buffer(dist =  -advancedloggingparameters$ScndTrailWidth/2)),
                            y = AccessRaster ,
                            field = 0,
                            update = TRUE)
  AccessRaster <- crop(AccessRaster,  CostRaster)
  AccessRaster <- mask(AccessRaster, plots)

  #Update Cost Raster with accessible weights raster
  CostRaster<- CostRaster + AccessRaster

  #Generate protection buffer for Big Trees (dist : ScndTrailWidth/2 + 2 m)



  BigTreesRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(BigTreesRaster) <- 0
  BigTreesRaster <- rasterize(x = as_Spatial(treeselectionoutputs$BigTreesPoints %>%
                                               st_buffer(dist =  advancedloggingparameters$ScndTrailWidth/2 + 2) ),
                              y = BigTreesRaster ,
                              field = CostMatrix[[2]][[3]]$CostValue,
                              update = TRUE)
  BigTreesRaster <- crop(BigTreesRaster,  CostRaster)

  #Generate protection buffer for Reserve Trees (dist : ScndTrailWidth/2 + 2 m)

  ReserveRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(ReserveRaster) <- 0
  ReserveRaster <- rasterize(x = as_Spatial(treeselectionoutputs$HarvestableTreesPoints %>%
                                              st_buffer(dist =  advancedloggingparameters$ScndTrailWidth/2 + 2) ),
                             y = ReserveRaster ,
                             field = CostMatrix[[2]][[4]]$CostValue,
                             update = TRUE)
  ReserveRaster <- crop(ReserveRaster,  CostRaster)

  #Generate protection buffer for Futures Trees (dist : ScndTrailWidth/2 + 2 m)
  FutureRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(FutureRaster ) <- 0
  FutureRaster  <- rasterize(x = as_Spatial(treeselectionoutputs$HarvestableTreesPoints %>%
                                              st_buffer(dist =  advancedloggingparameters$ScndTrailWidth/2 +2) ),
                             y = FutureRaster  ,
                             field = CostMatrix[[2]][[5]]$CostValue,
                             update = TRUE)
  FutureRaster  <- crop(FutureRaster ,  CostRaster)

  #Update Cost Raster with protection buffers

  #Generate protection buffer for selected Trees (dist : ScndTrailWidth/2 + 2 m)
  SelectedRaster <- raster(extent(DTMExtended),resolution = res(DTMExtended), crs = crs(DTMExtended))
  values(SelectedRaster ) <- 0
  SelectedRaster  <- rasterize(x = as_Spatial(treeselectionoutputs$SelectedTreesPoints %>%
                                              st_buffer(dist =  advancedloggingparameters$ScndTrailWidth/2 +2) ),
                             y = SelectedRaster  ,
                             field = CostMatrix[[2]][[3]]$CostValue/2,
                             update = TRUE)
  SelectedRaster  <- crop(SelectedRaster ,  CostRaster)

  #Update Cost Raster with protection buffers

  CostRaster<- CostRaster + BigTreesRaster + ReserveRaster + FutureRaster + SelectedRaster

  #Generate Slope accessibility for grapple machine
  if (scenarios$winching == "2") {

    plotslopePointGrpl <- as_tibble(rasterToPoints(plotslope))

    CostRasterPointGrpl <- as_tibble(rasterToPoints(CostRaster))

    # left join par x et y
    plotTibGrpl <-
      left_join(plotslopePointGrpl, CostRasterPointGrpl, by = c('x', 'y'))

    CostSlopeRasterGrpl <- plotTibGrpl %>%
      mutate(Harvestable = if_else(
        slope <= atan(advancedloggingparameters$GrappleMaxslope/100),
        true = 0,
        false = Inf
      )) %>% select(x,y,Harvestable)

    CostRasterGrpl <- rasterFromXYZ(CostSlopeRasterGrpl, crs = crs(DTM))

    CostRasterGrpl <- CostRaster + CostRasterGrpl

    CostRasterGrpl <- rasterize(x = as_Spatial(Partmaintrails),
                                y = CostRasterGrpl ,
                                field = CostMatrix[[2]][[6]]$CostValue,
                                update = TRUE)

    CostRasterMeanGrpl <- aggregate(CostRasterGrpl, fact=fact, fun=mean)
    CostRasterMeanGrpl <- crop(CostRasterMeanGrpl,  CostRaster)
    CostRasterMeanGrpl <- mask(CostRasterMeanGrpl, plots)

  }


  #Generate maintrail intersection cost raster
  CostRaster <- rasterize(x = as_Spatial(Partmaintrails),
                          y = CostRaster ,
                          field = CostMatrix[[2]][[6]]$CostValue,
                          update = TRUE)

  #Aggregation each raster to selected resolution
  CostRasterMean <- aggregate(CostRaster, fact=fact, fun=mean)
  CostRasterMean <- crop(CostRasterMean,  CostRaster)
  CostRasterMean <- mask(CostRasterMean, plots)

  DTMExtended <- crop(DTMExtended,  CostRasterMean)
  DTMExtended <- mask(DTMExtended, plots)

  DTMmean <- aggregate(DTMExtended, fact=fact, fun=mean)
  DTMmean <- crop(DTMmean,  CostRasterMean)
  DTMmean <- mask(DTMmean, plots)


  #Compute conductance raster
  CondSurf <- 1/CostRasterMean


  pathLines <- list() #Initialize storage pathlines
  Lines <- list() #Initialize storage logged trees
  k <- 1 #Initialize pathlines index

  #Generate appropriate selected trees points format
  pts <- treeselectionoutputs$SelectedTreesPoints %>%
    select(idTree) %>%
    st_cast("POINT") %>%
    mutate(ID = NA) %>%
    mutate(type = "Tree") %>%
    select(ID,type,idTree)


  winching <- scenarios$winching



  ########### Compute LCP algorithm ###############

  if (winching == "0") {
    # winching 0 #########

    #Compute adjacent transition layer according to slope conditions (winching = "0")
    SlopeCond <- SlopeRdCond(DTM = DTMmean,advancedloggingparameters = loggingparameters())


    pts <- st_set_crs(pts, st_crs(AccessPoint))# set crs

    pts <- rbind(AccessPoint %>%
                   mutate(type = "Access") %>%
                   mutate(IPpts = paste0("A.",row_number())),
                 pts %>%
                   mutate(IPpts = paste0("T.",idTree)))



    PointAcc <- pts %>%
      filter(type == "Access") %>%
      mutate(EstCost = NA)



    #Compute Cost between points and Access points
    CostDistEst <- AdjTopoLCP(costSurface = CondSurf,
                              DTM = DTMmean ,
                              pts = pts %>%
                                as_Spatial(),
                              slopeRdCond = SlopeCond,
                              paths = FALSE) [,1:dim(AccessPoint)[1]]
    CostDistEst <- CostDistEst[(dim(PointAcc)[1]+1):dim(CostDistEst)[1],1:dim(PointAcc)[1]]

    CostDistEst <- matrix(CostDistEst,ncol = dim(PointAcc)[1] )

    #Attribute a least cost point access to each points
    PointTree <- pts %>% filter(type == "Tree") %>%
      mutate(ptAcc = max.col(-CostDistEst))
    PointTree$ID <- as.factor(PointTree$ptAcc)
    levels(PointTree$ID) <- as.character(AccessPoint$ID)

    pts <- rbind(AccessPoint %>%
                   mutate(type = "Access") %>%
                   mutate(ptAcc = row_number()) %>%
                   mutate(IPpts = paste0("A.",row_number())),
                   PointTree %>%
                   mutate(IPpts = paste0("T.",idTree)))


    PointAcc <- pts %>%
      filter(type == "Access") %>%
      mutate(EstCost = NA)

    #Attribute a least cost point access to each points
    PointTree <- pts %>% filter(type == "Tree") %>%
      mutate(ptAcc = max.col(-CostDistEst))
    if (dim(PointAcc)[1] == 1) {
      PointTree <- PointTree %>% mutate(EstCost = CostDistEst)
    }else{PointTree <- PointTree %>% mutate(EstCost = apply(CostDistEst,1, min))}
    selectedPoints <- rbind(PointAcc,PointTree)

    for (accessPtId in 1:dim(PointAcc)[1]) {
      TmpSelectedPts <- selectedPoints %>%
        filter(ptAcc == accessPtId)

      TmpAccPts <- TmpSelectedPts %>%
        filter(type == "Access")

      #Select Costliest selected tree
      TmpTreePts <- TmpSelectedPts %>%
        filter(type == "Tree") %>%
        arrange(desc(EstCost))

      if (dim(TmpTreePts)[1] != 0) {
        for (TreeId in 1:dim(TmpTreePts)[1]){

          #Compute Least cost path polygons to the WIP selected tree
          TmpPtsWIP <- rbind(TmpAccPts,TmpTreePts[TreeId,])
          TmpPathWIP <-  AdjTopoLCP(costSurface = CondSurf,DTM = DTMmean , pts = TmpPtsWIP %>% as_Spatial(),
                                    slopeRdCond = SlopeCond,paths = TRUE)

          #Update Cost raster with LCP second trail
          CostRasterMean  <- rasterize(x = TmpPathWIP[[2]] ,
                                       y = CostRasterMean  ,
                                       field = CostMatrix[[2]][[7]]$CostValue,update =  TRUE)
          CondSurf <- 1/CostRasterMean

          #Store pathline
          pathLines[[k]] <- TmpPathWIP[[2]]
          pathLines[[k]]@lines[[1]]@ID <- paste("Path", TmpPtsWIP$idTree[2], sep = ".")

          Lines[[k]] <- list("LineID" = k,"LoggedTrees" = TmpPtsWIP$idTree[2])

          k <- k +1

        }
      }
    }
  }else{
    # winching 1/2 #########

    # Select intersection points from buffer polygons

    if (winching == "2") {
      #Compute adjacent transition layer according to slope conditions (winching = "2")
      SlopeCondGrpl <- SlopeRdCond(DTM = DTMmean,advancedloggingparameters = loggingparameters(),grapple = TRUE)
    }
    #Compute adjacent transition layer according to slope conditions (winching = "1")
    SlopeCond <- SlopeRdCond(DTM = DTMmean,advancedloggingparameters = loggingparameters())




    PointAcc <- AccessPoint %>% #def Access Point
      mutate(type = "Access") %>%
      mutate(IDpts = paste0("A.",row_number())) %>%
      mutate(n.overlaps = NA, origins = NA) %>%
      select(-ID)

    TreePts <- pts %>% filter(type == "Tree")

    if (winching == "2") {
      ptsGrpl <- TreePts %>% #def Grpl point
        st_buffer(dist = advancedloggingparameters$GrappleLength) %>%
        st_set_precision(1) %>%
        st_intersection() %>%
        st_make_valid()

      for (OriginI in 1:length(ptsGrpl$origins)) {
        for (OriginJ in 1:length(ptsGrpl$origins[[OriginI]])) {
          IndexPts <- ptsGrpl$origins[[OriginI]][OriginJ]
          ptsGrpl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
        }
      }

      #Filter polygons which intersect accessible area to second trails

      ptsGrpl<- st_set_crs(ptsGrpl,st_crs(Accessmaintrails)) # set crs from Accessmaintrails

      ptsGrpl <- ptsGrpl %>%  filter(st_intersects(st_geometry(ptsGrpl), st_geometry(Accessmaintrails %>% st_union()), sparse = FALSE)) %>%
        mutate(IDpts = paste0("I.",row_number()))

      ptsWIP <- ptsGrpl %>% #def Grpl point as WIP point
        st_set_agr("constant") %>%
        st_centroid() %>%
        select(-ID)

      ptsCbl <- TreePts %>% #def cbl polygons
        st_buffer(dist = advancedloggingparameters$CableLength) %>%
        st_set_precision(1) %>%
        st_intersection() %>%
        st_make_valid()



      for (OriginI in 1:length(ptsCbl$origins)) {
        for (OriginJ in 1:length(ptsCbl$origins[[OriginI]])) {
          IndexPts <- ptsCbl$origins[[OriginI]][OriginJ]
          ptsCbl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
        }
      }


      ptsCbl <- st_set_crs(ptsCbl,st_crs(Accessmaintrails)) #set crs from Accessmaintrails

      ptsCbl <- ptsCbl %>% #Filter polygons which intersect accessible area to second trails
        filter(st_intersects(st_geometry(ptsCbl) ,st_geometry(Accessmaintrails %>% st_union()) ,sparse = FALSE)) %>%
        mutate(IDpts = paste0("I.",row_number()))



      ptsWIPCbl <-  ptsCbl %>%#Convert polygons to centroid
        st_set_agr("constant") %>%
        st_centroid() %>%
        select(-ID)

      ptsWIP <- ptsWIP %>%
        arrange(desc(n.overlaps))

      RemainTree <- dim(ptsWIP %>% filter(type == "Tree"))[1] + dim(ptsWIPCbl %>% filter(type == "Tree"))[1]

    }else{


      ptsCbl <- TreePts %>% #def cbl point
        st_buffer(dist = advancedloggingparameters$CableLength) %>%
        st_set_precision(1) %>%
        st_intersection() %>%
        st_make_valid() %>%
        mutate(IDpts = paste0("I.",row_number()))

      ptsCbl <- st_set_crs(ptsCbl,st_crs(Accessmaintrails)) #set crs from Accessmaintrails

      for (OriginI in 1:length(ptsCbl$origins)) {
        for (OriginJ in 1:length(ptsCbl$origins[[OriginI]])) {
          IndexPts <- ptsCbl$origins[[OriginI]][OriginJ]
          ptsCbl$origins[[OriginI]][OriginJ] <- TreePts$idTree[IndexPts]
        }
      }

      ptsWIP <-  ptsCbl %>%#def cbl point as WIP point
        st_set_agr("constant") %>%
        st_centroid() %>%
        select(-ID)

      ptsWIP <- ptsWIP %>%  #filter cbl intersection centroid point out plot
        filter(st_intersects(st_geometry(ptsWIP) ,st_geometry(plots %>% st_as_sf()),sparse = FALSE)) %>%
        arrange(desc(n.overlaps))

      RemainTree <- dim(ptsWIP %>% filter(type == "Tree"))[1]


    }


    #Define a switch from grapple accessible tree exploitation to cable exploitation

    if (winching == "2") {
      WinchingSwitch <- FALSE
    }else{
      WinchingSwitch <- TRUE
    }





    #Loop over possible intersection
    while (RemainTree !=0L) {

    #Switch from grpl to cbl exploitation when grapple accessible tree != 0
        Grpl2CblFlag <- FALSE



      ptsWIPmax <- rbind(PointAcc,ptsWIP %>%
                           filter(n.overlaps == max(ptsWIP$n.overlaps))) %>%
        mutate(TypeAcc = NA) %>%
        mutate(EstCost = NA)  %>%
        mutate(ptsAcc = NA)

      #Compute Cost between points and Access points in cbl exploitation
      CostDistEstCbl <- AdjTopoLCP(costSurface = CondSurf,
                                   DTM = DTMmean ,
                                   pts = ptsWIPmax %>%
                                     as_Spatial(),
                                   slopeRdCond = SlopeCond,
                                   paths = FALSE) [,1:dim(PointAcc)[1]]

      CostDistEstCbl <- CostDistEstCbl[(dim(PointAcc)[1]+1):dim(CostDistEstCbl)[1],1:dim(PointAcc)[1]]

      CostDistEstCbl <- matrix(CostDistEstCbl,ncol = dim(PointAcc)[1] )

      #Attribute a least cost point access to each points
      PointTree <- ptsWIPmax %>% filter(type == "Tree") %>%
        mutate(ptAccCbl = max.col(- CostDistEstCbl,ties.method = "first"))


      if (dim(PointAcc)[1] == 1) {
        PointTree <- PointTree %>% mutate(EstCostCbl = CostDistEstCbl)
      }else{
        PointTree <- PointTree %>% mutate(EstCostCbl = apply(CostDistEstCbl,1, min))
      }


      if (winching == "2") {

        CondSurfGrpl <- 1/CostRasterMeanGrpl

        #Compute Cost between points and Access points in grpl exploitation
        CostDistEstGrpl <- AdjTopoLCP(costSurface = CondSurfGrpl,
                                      DTM = DTMmean ,
                                      pts = ptsWIPmax %>%
                                        as_Spatial(),
                                      slopeRdCond = SlopeCondGrpl,
                                      paths = FALSE) [,1:dim(PointAcc)[1]]

        CostDistEstGrpl <- CostDistEstGrpl[(dim(PointAcc)[1]+1):dim(CostDistEstGrpl)[1],1:dim(PointAcc)[1]]
        CostDistEstGrpl <- matrix(CostDistEstGrpl,ncol = dim(PointAcc)[1] )


        #Attribute a least cost point access to each points
        PointTree <- PointTree %>%
          mutate(ptAccGpl = max.col(-matrix(CostDistEstGrpl,ncol = dim(PointAcc)[1] ),ties.method = "first"))

        if (dim(PointAcc)[1] == 1) {
          PointTree <- PointTree %>% mutate(EstCostGrpl = CostDistEstGrpl)
        }else{
          PointTree <- PointTree %>% mutate(EstCostGrpl = apply(matrix(CostDistEstGrpl,ncol = dim(PointAcc)[1] ),1, min))}

        #Prioritize grpl exploitation if possible
        CostDistEstGrpl[CostDistEstGrpl != Inf] <- 0
        CostDistEstGrpl[CostDistEstGrpl == Inf] <- 1

        for (j in 1:dim(CostDistEstGrpl)[1]) {
          PointTree[j,"TypeAcc"] <-  as.character(prod(CostDistEstGrpl[j,]))
        }
        PointTree$TypeAcc[PointTree$TypeAcc == "0"] <- "Grpl"
        PointTree$TypeAcc[PointTree$TypeAcc == "1"] <- "Cbl"

        PointTree$ptsAcc[PointTree$TypeAcc == "Grpl"] <- PointTree$ptAccGpl[PointTree$TypeAcc == "Grpl"]
        PointTree$ptsAcc[PointTree$TypeAcc != "Grpl"] <- PointTree$ptAccCbl[PointTree$TypeAcc != "Grpl"]

        PointTree$EstCost[PointTree$TypeAcc == "Grpl"] <- PointTree$EstCostGrpl[PointTree$TypeAcc == "Grpl"]
        PointTree$EstCost[PointTree$TypeAcc != "Grpl"] <- PointTree$EstCostCbl[PointTree$TypeAcc != "Grpl"]


        PointTree <- PointTree %>% arrange(desc(TypeAcc),EstCost)




        #Define WIP point according to possible exploitation type
        if (PointTree$TypeAcc[1] == "Grpl") {
          #Reconstruct access points + selected tree in grpl exploitation
          TmpPtsWIP <- ptsGrpl %>%
            filter(IDpts == PointTree$IDpts[1])  %>%
            st_union() %>%
            st_cast("POINT") %>%
            st_as_sf() %>%
            mutate(type = "Overlay") %>%
            mutate(ptsAcc = PointTree$ptsAcc[1]) %>%
            mutate(IDpts = PointTree$IDpts[1]) %>%
            mutate(origins = PointTree$origins[1])%>%
            mutate(n.overlaps = PointTree$n.overlaps[1])

          #Select adjacent grpl graph
          SlopeCondRd <- SlopeCondGrpl

        }else{
          #if the selected point is not a grpl accessible point BUT not all grpl accessible point are done

          #Shift to cbl exploitation
          Grpl2CblFlag <- TRUE

          ptsWIP$Logged<- FALSE


          for (j in 1:dim(ptsWIP)[1]) {
            ptsWIP$Logged[j] <- prod(as.integer(PointTree$origins[[1]][1] %in% ptsWIP$origins[[j]])+1) != 1
          }



          ptsWIP[ptsWIP$Logged == TRUE,"Logged"] <- NA
          ptsWIP <- na.exclude(ptsWIP)

          ptsWIP <- ptsWIP %>%
            select(-Logged) %>%
            arrange(desc(n.overlaps))


          #Reconstruct access points + selected tree in cbl exploitation
          TmpPtsWIP <- ptsCbl %>%
            filter(IDpts == PointTree$IDpts[1])  %>%
            st_union() %>%
            st_cast("POINT") %>%
            st_as_sf() %>%
            mutate(type = "Overlay") %>%
            mutate(ptsAcc = PointTree$ptsAcc[1]) %>%
            mutate(IDpts = PointTree$IDpts[1]) %>%
            mutate(origins = PointTree$origins[1])%>%
            mutate(n.overlaps = PointTree$n.overlaps[1])

          #Select adjacent cbl graph
          SlopeCondRd <- SlopeCond
        }

      }else{

        #Reconstruct access points + selected tree in cbl exploitation
        TmpPtsWIP <- ptsCbl %>%
          filter(IDpts == PointTree$IDpts[1])  %>%
          st_union() %>%
          st_cast("POINT") %>%
          st_as_sf() %>%
          mutate(type = "Overlay") %>%
          mutate(ptsAcc = PointTree$ptsAcc[1]) %>%
          mutate(IDpts = PointTree$IDpts[1]) %>%
          mutate(origins = PointTree$origins[1]) %>%
          mutate(n.overlaps = PointTree$n.overlaps[1])



        #Select adjacent cbl graph
        SlopeCondRd <- SlopeCond

      }

      #filter WIP points in accessible scd trail area
     # TmpPtsWIP <- st_set_crs(TmpPtsWIP,st_crs(ptsCbl)) # set crs from ptsCbl

      TmpPtsWIP <- TmpPtsWIP %>%
        filter(st_intersects(st_geometry(TmpPtsWIP),st_geometry(Accessmaintrails %>% st_union()),sparse = FALSE))


      #Reconstruct access points + selected points
      TmpPtsWIP <- rbind( PointAcc %>%
                           st_union() %>%
                           st_cast("POINT")%>%
                           st_as_sf() %>%
                           mutate(type = "Access") %>%
                           mutate(ptsAcc = NA ) %>%
                           mutate(IDpts = NA) %>%
                           mutate(origins = NA)%>%
                           mutate(n.overlaps = NA),TmpPtsWIP)


      #Compute Cost between all points and Access points
      CostDistEstWIP <-  AdjTopoLCP(costSurface = CondSurf,DTM = DTMmean , pts = TmpPtsWIP %>% as_Spatial(),
                                    slopeRdCond = SlopeCondRd,paths = FALSE)[,1:dim(PointAcc)[1]]

      CostDistEstWIP <- CostDistEstWIP[(dim(PointAcc)[1]+1):dim(CostDistEstWIP)[1],]

      CostDistEstWIP <- matrix(CostDistEstWIP, ncol = dim(PointAcc)[1])

      if (dim(PointAcc)[1] == 1) {
        PointTreeWIP <- TmpPtsWIP %>%
          filter(type == "Overlay") %>%
          mutate(EstCost = CostDistEstWIP) %>%
          arrange(EstCost)
      }else{

        PointTreeWIP <- TmpPtsWIP %>%
          filter(type == "Overlay") %>%
          mutate(EstCost = apply(CostDistEstWIP,1, min)) %>%
          arrange(EstCost)

      }


      TmpPtsWIP <- rbind(TmpPtsWIP %>% filter(type == "Access") %>% mutate(EstCost = NA),PointTreeWIP[1,])

      TmpPathWIP <- AdjTopoLCP(costSurface = CondSurf,DTM = DTMmean , pts = TmpPtsWIP %>% as_Spatial(),
                               slopeRdCond = SlopeCondRd,paths = FALSE)

      TmpPathWIPCost <- TmpPathWIP[1:dim(PointAcc)[1],dim(PointAcc)[1]+1]

      LCPathWIP <- max.col(t(-TmpPathWIPCost))

      TmpPtsWIP <- rbind(TmpPtsWIP[LCPathWIP,],PointTreeWIP[1,])

      TmpPathWIP <- AdjTopoLCP(costSurface = CondSurf,DTM = DTMmean , pts = TmpPtsWIP %>% as_Spatial(),
                               slopeRdCond = SlopeCondRd,paths = TRUE)

      if (TmpPathWIP[[1]][2,1] != 0) {
        CostRasterMean  <- rasterize(x = TmpPathWIP[[2]] ,
                                     y = CostRasterMean  ,
                                     field = CostMatrix[[2]][[7]]$CostValue,update =  TRUE)
      }

      pathLines[[k]] <- TmpPathWIP[[2]]
      pathLines[[k]]@lines[[1]]@ID <- paste("Path",
                                            "A",
                                            LCPathWIP,
                                            "NTree",
                                            length(PointTreeWIP$origins[[1]]),
                                            "T",
                                            paste(as.character(unlist(PointTreeWIP$origins[[1]])),
                                                  collapse='-'),
                                            sep = ".")

      Lines[[k]] <- list("LineID" = k,"LoggedTrees" = PointTreeWIP$origins[[1]], "winching" = winching)

      k <- k +1




      if (Grpl2CblFlag & winching == "2") {

        ptsWIPCbl$Logged<- FALSE
        for (j in 1:dim(ptsWIPCbl)[1]) {
          ptsWIPCbl$Logged[j] <- prod(as.integer(PointTreeWIP$origins[[1]] %in% ptsWIPCbl$origins[[j]])+1) != 1
        }
        ptsWIPCbl[ptsWIPCbl$Logged == TRUE,"Logged"] <- NA
        ptsWIPCbl <- na.exclude(ptsWIPCbl)
        ptsWIPCbl <- ptsWIPCbl %>%
          select(-Logged) %>%
          arrange(desc(n.overlaps))
      }else{


        ptsWIP$Logged<- FALSE



        for (j in 1:dim(ptsWIP)[1]) {
          ptsWIP$Logged[j] <- prod(as.integer(PointTreeWIP$origins[[1]] %in% ptsWIP$origins[[j]])+1) != 1
        }

        ptsWIP[ptsWIP$Logged == TRUE,"Logged"] <- NA
        ptsWIP <- na.exclude(ptsWIP)

        ptsWIP <- ptsWIP %>%
          select(-Logged) %>%
          arrange(desc(n.overlaps))

        if (winching == "2") {


          ptsWIPCbl$Logged<- FALSE
          for (j in 1:dim(ptsWIPCbl)[1]) {
            ptsWIPCbl$Logged[j] <- prod(as.integer(PointTreeWIP$origins[[1]] %in% ptsWIPCbl$origins[[j]])+1) != 1
          }
          ptsWIPCbl[ptsWIPCbl$Logged == TRUE,"Logged"] <- NA
          ptsWIPCbl <- na.exclude(ptsWIPCbl)
          ptsWIPCbl <- ptsWIPCbl %>%
            select(-Logged) %>%
            arrange(desc(n.overlaps))
        }

      }




      if (dim(ptsWIP %>% filter(type == "Tree"))[1] == 0 & WinchingSwitch == FALSE & winching == "2") {
        winching <- "1"
        ptsWIP <- ptsWIPCbl
        WinchingSwitch <- TRUE
      }


      if (winching == "2") {
        RemainTree <- dim(ptsWIP %>% filter(type == "Tree"))[1] + dim(ptsWIPCbl %>% filter(type == "Tree"))[1]
      }else{
        RemainTree <- dim(ptsWIP %>% filter(type == "Tree"))[1]
      }



    }


  }
  ptsHarvested <- pts %>%
    filter(type == "Tree")

  paths <- do.call(rbind, pathLines)


  ptsHarvested <- st_set_crs(ptsHarvested, st_crs(Partmaintrails)) #set ptsHarvested crs

  lines <- do.call(rbind, Lines)

  secondtrails <- smoothtrails(paths,
                               pts = ptsHarvested,
                               plots,
                               partmaintrails = Partmaintrails,
                               advancedloggingparameters = loggingparameters())


  AllTrees <- treeselectionoutputs$inventory

  coordinates(AllTrees) <- ~Xutm + Yutm
  proj4string(AllTrees) <- crs(DTM)

  AllTrees <- st_as_sf(AllTrees)

  DeathInter <-  st_intersects(st_geometry(AllTrees),st_geometry(secondtrails),sparse = FALSE)
  DeathInter[DeathInter == TRUE] <-  "2ndTrail"
  DeathInter[DeathInter == FALSE] <- NA

  inventory <- treeselectionoutputs$inventory %>%
    mutate(DeathCause = DeathInter)

  secondtrails <- list(paths,lines,secondtrails,inventory,CostRasterMean)

  return(secondtrails)


}
