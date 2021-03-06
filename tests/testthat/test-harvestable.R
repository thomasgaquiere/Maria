test_that("harvestable", {

  # Check the function arguments
  data("Paracou6_2016")
  data("MainTrails")
  data("HarvestablePolygons")
  Paracou6_2016 <- dplyr::slice(Paracou6_2016, 1:1000)

  data(DTMParacou)
  data(PlotSlope)

  MatrixInventory <- as.matrix(Paracou6_2016)
  expect_error(harvestable(MatrixInventory), regexp = "The 'inventory' argument of the 'harvestable' function must be a data.frame")

  expect_error(harvestable(Paracou6_2016, diversification = "1", specieslax = 2),
               regexp = "The 'diversification' and 'specieslax' arguments of the 'harvestable' function must be logical")

  expect_error(harvestable(Paracou6_2016, diversification = TRUE, specieslax = FALSE,
                           topography = NULL, plotslope = NULL))

  # LoggingStatus column exist and have no NA
  ## Test data preparation
  inventory <- addtreedim(inventorycheckformat(Paracou6_2016), volumeparameters = ForestZoneVolumeParametersTable)
  inventory <- ONFGuyafortaxojoin(inventory, SpeciesCriteria)

  Outputs1 <- harvestable(inventory,
                          diversification = T, specieslax = F, topography = DTMParacou, plotslope = PlotSlope,
                          maintrails = MainTrails, harvestablepolygons = HarvestablePolygons)
  Outputs2 <- harvestable(inventory,
                          diversification = F, specieslax = T, topography = DTMParacou, plotslope = PlotSlope,
                          maintrails = MainTrails, harvestablepolygons = HarvestablePolygons)
  Outputs3 <- harvestable(inventory,
                          diversification = F, specieslax = F, topography = DTMParacou, plotslope = PlotSlope,
                          maintrails = MainTrails, harvestablepolygons = HarvestablePolygons)

  testinventory1 <- Outputs1$inventory
  testinventory2 <- Outputs2$inventory
  testinventory3 <- Outputs3$inventory


  expect_false(any(is.na(testinventory1$LoggingStatus)))
  expect_false(any(is.na(testinventory2$LoggingStatus)))

  # Commercial == "0" are  LoggingStatus =="non-harvestable"
  TestCommercial <- testinventory1 %>%
    filter(Commercial == "0")

  expect_true(all(TestCommercial$LoggingStatus =="non-harvestable"))

  TestCommercial <- testinventory2 %>%
    filter(Commercial == "0")

  expect_true(all(TestCommercial$LoggingStatus =="non-harvestable"))

  # "harvestable": DBH >= MinFD & DBH <= MaxFD

  # "harvestable": Commercial == "1" ou "2" if diversification=T, or  diversification=F & specieslax=T
  testinventory1a <- testinventory1 %>%
    filter(Commercial == "2")
  testinventory2a <- testinventory2 %>%
    filter(Commercial == "2")

  expect_true(any(testinventory1a$LoggingStatus =="harvestable"))
  expect_true(any(testinventory2a$LoggingStatus =="harvestable2nd"))


  # "harvestable": Commercial == "1"  diversification=F & specieslax=F
  testinventory3a <- testinventory3 %>%
    filter(Commercial != "1")
  expect_true(all(testinventory3a$LoggingStatus == "non-harvestable"))


  # "harvestable": check spatial!! "DistCrit", "Slope", "SlopeCrit"

  StatialTable <- testinventory1 %>%
    filter(DBH >= MinFD & DBH <= MaxFD)

  ## Aggregative species individuals are checked for the distance between individuals of the same species
  AggregativeTable <- StatialTable %>%
    filter(Aggregative)

  expect_true(all(!is.na(AggregativeTable$DistCrit)))
  expect_true(all(!is.na(StatialTable$Slope)))
  expect_true(all(!is.na(StatialTable$SlopeCrit)))


  # HVinit = sum of "TreeHarvestableVolume" values of "harvestable" trees.
  ## Test data preparation
  HVinit1 <- Outputs1$HVinit
  HVinit2 <- Outputs2$HVinit
  HVinit3 <- Outputs3$HVinit

  HarvestableTable1 <- testinventory1 %>%
    filter(LoggingStatus == "harvestable")
  HarvestableTable2 <- testinventory2 %>%
    filter(LoggingStatus == "harvestable")
  HarvestableTable3 <- testinventory3 %>%
    filter(LoggingStatus == "harvestable")

  expect_true(HVinit1 == sum(HarvestableTable1$TreeHarvestableVolume))
  expect_true(HVinit2 == sum(HarvestableTable2$TreeHarvestableVolume))
  expect_true(HVinit3 == sum(HarvestableTable3$TreeHarvestableVolume))

  # All healthy:
  expect_true(all(HarvestableTable1$VisibleDefect == "0"))
  expect_true(all(HarvestableTable2$VisibleDefect == "0"))
  expect_true(all(HarvestableTable3$VisibleDefect == "0"))

# Harvestables have the good maximum slope, and good distance between individuals for aggregative species
  expect_true(all(!HarvestableTable1$DistCrit %in% FALSE))
  expect_true(all(!HarvestableTable1$DistCrit %in% FALSE))
  expect_true(all(!HarvestableTable1$DistCrit %in% FALSE))

  expect_true(all(HarvestableTable1$SlopeCrit %in% TRUE))
  expect_true(all(HarvestableTable1$SlopeCrit %in% TRUE))
  expect_true(all(HarvestableTable1$SlopeCrit %in% TRUE))


})


# Checker que:
# - la classe des arguments (inventory (dataframe), diversification, specieslax (logical))
# - colonne LoggingStatus existe et qu'il n'y a pas de NA
# - les Commercial == "0"sont codés "non-harvestable"
# - les "harvestable" ont leur DBH >= MinFD & DBH <= MaxFD
# - les "harvestable" sont codés commercial = "1" ou "2" si diversification=T ou si diversification=F & specieslax=T
# - les "harvestable" sont codés commercial = "1" si diversification=F & specieslax=F
# - les "harvestable" : check spatial!!
# - si specieslax=T : il y a des harvestable2nd
# - HVinit = somme des valeurs de "TreeHarvestableVolume " des arbres dont "LoggingStatus" = "harvestable".


# + Étiqueter "harvestable", dans une colonne "LoggingStatus", sélection des individus :
#   Essences :
#   - Si diversification=T ou si diversification=F & specieslax=T : = "1" & "2" dans colonne "Commercial"
#   - Sinon (diversification=F & specieslax=F): = "1" dans colonne "Commercial"
#   Diamètre exploitable (speciescriteria data): individus dont le DBH est compris entre la valeur dans colonne "MinFD" pour leur sp, de la table speciescriteria , et le MaxFD.
#   Distribution spatiale:
#     - Arbres sur <22% de pente et arbres sur pente >22% si à 40 m maximum d’une zone <22%.
#   - arbre non isolé : élimination des arbres à >100m des autres individus de la même espèce.
#   - hors des pistes principales : dont les coordonnées n'appartiennent pas aux multilignes "MainTrail".
# + Si diversification=F & specieslax=T, étiqueter "harvestable2nd" à la place d’"harvestable", les "Commercial"= "2" dans la colonne "LoggingStatus".
# + HVinit= somme des valeurs de "TreeHarvestableVolume " des arbres dont "LoggingStatus" = "harvestable".
