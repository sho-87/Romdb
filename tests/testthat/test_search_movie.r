test_that("search_movie() returns a dataframe", {
	test_movie <- search_movie("frozen")
	expect_is(test_movie, "data.frame")
})

test_that("Movie not found results in error", {
	test_movie <- search_movie("fdkjfhsdkvo")
	expect_identical(test_movie, "Movie not found. Please try again...")
})

test_that("No parameter passed", {
	expect_error(search_movie())
})

test_that("Empty search string passed", {
	test_movie <- search_movie("")
	expect_identical(test_movie, "Please supply a search term")
})
