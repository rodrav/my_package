library(testthat)
library(mipackage)
#Test of make_filename function

test_that('Test of make_filename function',{expect_that(make_filename(2015), is_identical_to("accident_2015.csv"))})

test_that('Test of make_filename function',{expect_that(make_filename(2014), is_identical_to("accident_2014.csv"))})

test_that('Test of make_filename function',{
  expect_that(make_filename(2013), is_identical_to("accident_2013.csv"))
  })

#end_test
