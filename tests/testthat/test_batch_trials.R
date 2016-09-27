context('batch and trials')


test_that("testing edf.all", {

  tr <- edf.all(edf.data('edf'))
  expect_is(tr,'list')
  expect_true(all(names(tr) %in% c('blinks', 'saccades','fixations','messages')))
  expect_false('eyetrial' %in% names(tr$fixations))
  expect_gt(length(tr$fixations),0)

})

test_that("testing edf.trials", {

  tr <- edf.trials(edf.data('edf'))
  expect_is(tr,'list')
  expect_true(all(names(tr) %in% c('header','fixations','saccades','blinks','messages')))
  expect_true('eyetrial' %in% names(tr$fixations))
  expect_gt(length(tr$fixations),0)

})


test_that("loading batch and combine.eyedata", {

  alldata <- edf.batch(edf.data('edf'),do.plot = F)
  expect_is(alldata,'list')
  expect_equal(length(alldata),length(edf.data('edf')))
  expect_equal(names(alldata[[1]]),c( "header","fixations", "saccades", "blinks", "messages", "ID", "filename"))

  comb <- combine.eyedata(alldata)
  expect_is(comb$fixations,'data.frame')
  expect_gt(length(comb$fixations),0)

})
