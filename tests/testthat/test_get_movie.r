test_that("get_movie() returns a list", {
	test_movie <- get_movie("frozen")
	expect_is(test_movie, "list")
})

test_that("Movie not found results in error", {
	test_movie <- get_movie("fdkjfhsdkvo")
	expect_identical(test_movie, "Movie not found. Please try again...")
})

test_that("Empty movie name passed", {
	test_movie <- get_movie("")
	expect_identical(test_movie, "Please supply a movie name")
})

test_that("No parameter passed", {
	expect_error(get_movie())
})
