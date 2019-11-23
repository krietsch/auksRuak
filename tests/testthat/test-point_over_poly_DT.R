context('point_over_poly_DT')


test_that('point_over_poly_DT', {

  # create table with one inside and one outside the study site
  DT = data.table(name = c('NARL', 'Utqiagvik', 'BARC'),
                  lat  = c(71.320854, 71.290246, 71.324966),
                  lon  = c(-156.648210, -156.788622, -156.667220))

  # change projection
  st_transform_DT(DT)
  point_over_poly_DT(DT, lat = 'lat', lon = 'lon', poly = study_site)

  expect_s3_class( DT, 'data.table' )

})
