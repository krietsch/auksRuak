context('prop_seen')


test_that('prop_seen', {

  dt = data.table(category  = c(rep('M', 100), rep('F', 100)),
                  ID_unique = as.character(c(1:200)),
                  time_seen = sample(x = c(1:40), size = 200, replace = TRUE) )

  ds = prop_seen(dt, 'category', 'ID_unique', 'time_seen')

  expect_s3_class( ds, 'data.table' )

})
