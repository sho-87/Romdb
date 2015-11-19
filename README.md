<!-- README.md is generated from README.Rmd. Please edit that file -->
R Open Movie Database API
=========================

Ever wanted to get movie information in R?

This is a test package for UBC STAT547, but is fully functional and allows you to extract movie information (e.g., plot summary, ratings, actors) using the [Open Movie Database API](http://www.omdbapi.com).

Note: requires an internet connection to use.

Usage
-----

Install and load the Romdb package from Github:

`install_github("sho-87/Romdb") library(Romdb)`

### Search Movie

If you're unsure about the specific name for a movie, you can search the database:

`results <- search_movie("matrix")`

The above will return a dataframe containing movies with a matching name, plus any relevant information about the movie.

### Get Movie

If you know the specific movie name you're interested in, you can query the database for just that film and extract all information associated with it:

`movie <- get_movie("Frozen")`

This returns a list containing information about the movie.
