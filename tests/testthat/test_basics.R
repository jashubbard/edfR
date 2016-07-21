context('basics')


test_that("loading events", {

  ev <- edf.events(edf.data('edf'))
  expect_is(ev,'data.frame')
  expect_gt(nrow(ev),1)

})


test_that("loading messages", {

  msg <- edf.messages(edf.data('edf'))
  expect_is(msg,'data.frame')
  expect_gt(nrow(msg),1)

})


test_that("loading recordings", {

  rec <- edf.recordings(edf.data('edf'))
  expect_is(rec,'data.frame')
  expect_gt(nrow(rec),1)

})


test_that("loading samples", {
  sam <- edf.samples(edf.data('edf'))
  expect_is(sam,'data.table')
  expect_gt(nrow(sam),1)
})
