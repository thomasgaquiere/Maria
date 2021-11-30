test_that("HarvestableAreaDefinition", {
  data(Plots)
  data(DTMParacou)
  data(VerticalCreekHeight)

  test_HarvestablePolygons <- HarvestableAreaDefinition(plot = Plots,
                                                     dtm = DTMParacou,
                                                     verticalcreekheight = VerticalCreekHeight)


  expect_s3_class(test_HarvestablePolygons, class = 'data.frame')
  expect_true((all(0 <= test_HarvestablePolygons$Harvestable &  test_HarvestablePolygons$Harvestable <= 1)))
})
