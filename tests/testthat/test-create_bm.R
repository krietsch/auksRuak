context('create_bm')


test_that('create_bm', {

  # create table with two points
  DT = data.table(name = c('NARL', 'Utqiagvik'),
                  lat  = c(71.320854, 71.290246),
                  lon  = c(-156.648210, -156.788622))

  # change projection
  st_transform_DT(DT)

  # create base map
  bm = create_bm(DT)

  expect_s3_class( bm, 'ggplot' )

})







