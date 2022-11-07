library(testthat)

#Test of make_filename function

test_that('Test of make_filename function',{expect_that(make_filename(2015), is_identical_to("accident_2015.csv.bz2"))})

test_that('Test of make_filename function',{expect_that(make_filename(2014), is_identical_to("accident_2014.csv.bz2"))})

test_that('Test of make_filename function',{expect_that(make_filename(2015), is_identical_to("accident_2015.csv.bz2"))})

#end_test
