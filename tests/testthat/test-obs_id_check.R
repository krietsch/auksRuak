context('obs_id_check')


test_that('obs_id_check', {

  x = obs_id_check('18_2_221')

  expect_type( x,  typeof('character') )

})

