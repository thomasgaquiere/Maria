test_that("felling1tree", {

  # Test data
  data(Paracou6_2016)
  data(MainTrails)
  data("HarvestablePolygons")
  # Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:2000)
  MatrixInventory <- as.matrix(Paracou6_2016)

  MainTrail <- sf::st_linestring(matrix(c(286400, 582945,
                                          286400, 583250,
                                          286655, 583250,
                                          286655, 582945,
                                          286400, 582945) # the return
                                        ,ncol=2, byrow=TRUE))

  pol1 <- list(matrix(c(286503, 582950,
                        286503, 583240,
                        286507, 583240,
                        286507, 582950,
                        286503, 582950)
                      ,ncol=2, byrow=TRUE))
  pol2 <- list(matrix(c(286650, 582950,
                        286650, 583240,
                        286654, 583240,
                        286654, 582950,
                        286650, 582950)
                      ,ncol=2, byrow=TRUE))

  PolList = list(pol1,pol2)
  ScndTrail <- sf::st_multipolygon(PolList)


  inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
                          volumeparameters = ForestZoneVolumeParametersTable)
  inventory <- suppressMessages(treeselection(inventory, objective = 20, scenario ="manual",
                                              fuel = "0", diversification = TRUE, specieslax = FALSE,
                                              objectivelax = TRUE, topography = DTMParacou,
                                              maintrails = MainTrails, harvestablepolygons = HarvestablePolygons,
                                              plotslope = PlotSlope,
                                              speciescriteria = SpeciesCriteria,
                                              advancedloggingparameters = loggingparameters())$inventory)

  FutureReserveCrowns <- inventory %>% # create an object with future/reserve crowns only
    dplyr::filter(LoggingStatus == "future" | LoggingStatus == "reserve") %>%
    createcanopy() %>% # create all inventory crowns in the 'Crowns' column
    getgeometry(Crowns)


  inventory <- inventory %>%
    dplyr::filter(Selected == "1") %>%
    dplyr::select(idTree,DBH,TrunkHeight,TreeHeight,CrownHeight,
                  CrownDiameter,Selected, Xutm, Yutm)

  dat <- inventory[1,] %>% # just 1 row (1 tree)
    tibble::add_column(TreeFellingOrientationSuccess = "1") # force the orientation success for the test

  # Grapple case (tree < 6 m from the trail)
  dat$Xutm <- 286508
  dat$Yutm <- 582950

  Rslt2grapple <- felling1tree(dat,
                               fuel = "2", directionalfelling = "2",
                               MainTrail = MainTrail, ScndTrail = ScndTrail,
                               FutureReserveCrowns = FutureReserveCrowns,
                               advancedloggingparameters = loggingparameters())

  # Cable case (tree > 6 m from the trail)
  dat$Xutm <- 286537
  dat$Yutm <- 582960

  Rslt2cable <- felling1tree(dat,
                             fuel = "2", directionalfelling = "2",
                             MainTrail = MainTrail, ScndTrail = ScndTrail,
                             FutureReserveCrowns = FutureReserveCrowns,
                             advancedloggingparameters = loggingparameters())


  # Other cases
  Rslt <- felling1tree(dat,
                       fuel = "0", directionalfelling = "1",
                       MainTrail = MainTrail, ScndTrail = ScndTrail,
                       FutureReserveCrowns = FutureReserveCrowns,
                       advancedloggingparameters = loggingparameters())

  RsltList <- list(

    Rslt1 <- felling1tree(dat,
                          fuel = "0", directionalfelling = "2",
                          MainTrail = MainTrail, ScndTrail = ScndTrail,
                          FutureReserveCrowns = FutureReserveCrowns,
                          advancedloggingparameters = loggingparameters()),

    Rslt2cable <- Rslt2cable
  )


  # Check the function arguments
  expect_error(felling1tree(MatrixInventory,
                            fuel = "0", directionalfelling = "2",
                            advancedloggingparameters = loggingparameters()),
               regexp = "The 'dat' argument of the 'felling1tree' function must be data.frame")

  expect_error(felling1tree(Paracou6_2016),
               regexp = "the data.frame given in the 'dat' argument
         of the 'felling1tree' function must contain only 1 row")

  expect_error(felling1tree(dat, directionalfelling = "2",
                            advancedloggingparameters = loggingparameters(),
                            fuel = TRUE),
               regexp = "The 'fuel' argument of the 'felling1tree' function must be '0', '1', '2' or NULL")

  expect_error(felling1tree(dat, fuel = "2",
                            advancedloggingparameters = loggingparameters(),
                            directionalfelling = TRUE),
               regexp = "The 'directionalfelling' argument of the 'felling1tree' function must be '0', '1', '2' or NULL")

  expect_error(felling1tree(dat,
                            fuel = "2", directionalfelling = "2",
                            advancedloggingparameters = loggingparameters(),
                            MainTrail = sf::st_as_text(MainTrail), ScndTrail = sf::st_as_text(ScndTrail)),
               regexp = "The 'MainTrail' and 'ScndTrail' arguments of the 'felling1tree' function must be sfg")


  expect_error(felling1tree(dat, fuel = "2",
                            directionalfelling = "2", MainTrail = MainTrail, ScndTrail = ScndTrail,
                            advancedloggingparameters = as.matrix(loggingparameters())),
               regexp = "The 'advancedloggingparameters' argument of the 'felling1tree' function must be a list")

  # Results class:
  ## $Foot is a POINT
  expect_s3_class(Rslt$Foot, "POINT")

  ## $NearestPoints is a LINESTRING,
  expect_s3_class(Rslt$NearestPoints, "sfc_LINESTRING")

  ## $TrailPt is a POINT,
  expect_s3_class(Rslt$TrailPt, "POINT")

  ## $FallenTree is multipolygons
  expect_s3_class(Rslt$FallenTree, "sfc_MULTIPOLYGON")

  # Check the angle between the tree and the trail

  for(rslt in RsltList){

    Arrival <- sf::st_point(as.numeric(unlist( # sfc to sfg
      sf::st_centroid(rslt$FallenTree))))

    OrientationA <- as.numeric(matlib::angle(c(rslt$TrailPt[1] - rslt$TrailPt[1], (rslt$TrailPt[2]+10) - rslt$TrailPt[2]),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE)) # vertical trail

    OrientationB <- as.numeric(matlib::angle(c(rslt$TrailPt[1] - rslt$TrailPt[1], rslt$TrailPt[2] - (rslt$TrailPt[2]+10)),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE)) # vertical trail

    OrientationC <- as.numeric(matlib::angle(c((rslt$TrailPt[1]+10) - rslt$TrailPt[1], rslt$TrailPt[2] - rslt$TrailPt[2]),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE)) # horizontal trail

    OrientationD <- as.numeric(matlib::angle(c(rslt$TrailPt[1] - (rslt$TrailPt[1]+10), rslt$TrailPt[2] - rslt$TrailPt[2]),
                                             c(Arrival[1] - rslt$Foot[1], Arrival[2] - rslt$Foot[2]),
                                             degree = TRUE)) # horizontal trail



    expect_true((OrientationA >= 29.9 & OrientationA <= 45) | (OrientationB >= 29.9 & OrientationB <= 45)|
                  (OrientationC >= 29.9 & OrientationC <= 45) | (OrientationD >= 29.9 & OrientationD <= 45))
  }



  Arrival <- sf::st_point(as.numeric(unlist( # sfc to sfg
    sf::st_centroid(Rslt2grapple$FallenTree))))

  OrientationA <- as.numeric(matlib::angle(c(Rslt2grapple$TrailPt[1] - Rslt2grapple$TrailPt[1], (Rslt2grapple$TrailPt[2]+10) - Rslt2grapple$TrailPt[2]),
                                           c(Arrival[1] - Rslt2grapple$Foot[1], Arrival[2] - Rslt2grapple$Foot[2]),
                                           degree = TRUE))

  OrientationB <- as.numeric(matlib::angle(c(Rslt2grapple$TrailPt[1] - Rslt2grapple$TrailPt[1], Rslt2grapple$TrailPt[2] - (Rslt2grapple$TrailPt[2]+10)),
                                           c(Arrival[1] - Rslt2grapple$Foot[1], Arrival[2] - Rslt2grapple$Foot[2]),
                                           degree = TRUE))

  OrientationC <- as.numeric(matlib::angle(c((Rslt2grapple$TrailPt[1]+10) - Rslt2grapple$TrailPt[1], Rslt2grapple$TrailPt[2] - Rslt2grapple$TrailPt[2]),
                                           c(Arrival[1] - Rslt2grapple$Foot[1], Arrival[2] - Rslt2grapple$Foot[2]),
                                           degree = TRUE))

  OrientationD <- as.numeric(matlib::angle(c(Rslt2grapple$TrailPt[1] - (Rslt2grapple$TrailPt[1]+10), Rslt2grapple$TrailPt[2] - Rslt2grapple$TrailPt[2]),
                                           c(Arrival[1] - Rslt2grapple$Foot[1], Arrival[2] - Rslt2grapple$Foot[2]),
                                           degree = TRUE))


  expect_true((OrientationA > 0 & OrientationA < 180) | (OrientationB > 0 & OrientationB < 180)|
                (OrientationC > 0 & OrientationC < 180) | (OrientationD > 0 & OrientationD < 180))


})


# check args errors
# results:
# $Foot is a POINT,
# $NearestPoints is a LINESTRING,
# $TrailPt is a POINT,
# $Trail is a multipolygon,
# $FallenTree is multipolygons
# Check the angle between the tree and the trail

# library(ggplot2)
# ggplot() +
#   geom_sf(data = Rslt1$Foot) +
#   geom_sf(data = Rslt1$Trail) +
#   geom_sf(data = Rslt1$NearestPoints) +
#   geom_sf(data = Rslt1$TrailPt) +
#   geom_sf(data = Rslt1$FallenTree) +
#   geom_sf(data = FutureReserveCrowns)
#   geom_sf(data = Arrival)


