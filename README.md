<!-- README.md is generated from README.Rmd. Please edit that file -->
R Open Movie Database API
=========================

Ever wanted to get movie information in R?

This is a test package for UBC STAT547, but is fully functional and allows you to extract movie information (e.g., plot summary, ratings, actors) using the [Open Movie Database API](http://www.omdbapi.com).

Note: requires an active internet connection to use.

Usage
-----

Install the package from Github:

`install_github("sho-87/Romdb")`

If it complains about packages not being found (e.g. stringi, curl), you'll need to manually install those packages before running `install_github`:

`install.packages("curl")`

`install.packages("stringi")`

Load it into your R session:

`library(Romdb)`

### Search Movie

You can search OMDb with a search string to see movies that share the same/a similar name:

`results <- search_movie("matrix")`

Note: a string must be passed to the function.

The above will return a dataframe containing movies with a matching name, plus any relevant information about the movie.

The returned dataframe contains:

-   **Title**
    -   Name of the movie
-   **Type**
    -   Media type of that item. In addition to movies, OMDb also stores information about TV Shows and Video games
-   **Year**
    -   Year of release
-   **imdbID**
    -   The IMDB id of the selected item
-   **Poster**
    -   A link to the movie poster

### Get Movie

If you know the specific movie name you're interested in, you can query the database for just that film and extract all information associated with it:

`movie <- get_movie("Frozen")`

Note: a string must be passed to the function.

This returns a list containing information about the movie.

The following information is available for each movie:

-   **Title**
    -   Name of the movie
-   **Year**
    -   Year of release
-   **Rated**
    -   The IMDB id of the selected item
-   **Released**
    -   Release data
-   **Runtime**
    -   Total movie runtime
-   **Genre**
    -   Genre(s) of the movie
-   **Director**
    -   Movie director(s)
-   **Writer**
    -   Movie writer(s)
-   **Actors**
    -   Movie actor(s)
-   **Plot**
    -   Brief plot summary
-   **Language**
    -   Original movie language
-   **Country**
    -   Country of origin
-   **Awards**
    -   Awards nominated for/won
-   **Poster**
    -   Link to the movie poster
-   **Metascore**
    -   Movie metascore, aggregated over multiple movie rating websites
-   **imdbRating**
    -   Movie rating on IMDB
-   **imdbVotes**
    -   Number of votes received on IMDB
-   **imdbID**
    -   Identifier for IMDB
-   **Type**
    -   Type of media
-   **tomatoMeter**
    -   Percentage of critics that gave movie a positive review
-   **tomatoImage**
    -   Review image type on rotten tomatoes
-   **tomatoRating**
    -   Movie rating by critics out of 10
-   **tomatoReviews**
    -   Number of critic reviews
-   **tomatoFresh**
    -   Number of positive reviews
-   **tomatoRotten**
    -   Number of negative reviews
-   **tomatoConsensus**
    -   General consensus about the movie by reviewers
-   **tomatoUserMeter**
    -   Percentage of users that liked the movie
-   **tomatoUserRating**
    -   User score out of 5
-   **tomatoUserReviews**
    -   Number of user reviews
-   **DVD**
    -   DVD release date
-   **BoxOffice**
    -   Box office gross
-   **Production**
    -   Production company
-   **Website**
    -   Movie website

To extract only specific variables from the list use the `$` syntax in R

Examples
--------

``` r
library(Romdb)

searchResults <- search_movie("hunger games")

hungerGamesMovie <- get_movie("The Hunger Games")

hungerGamesMovie$Plot

hungerGamesMovie$tomatoRating

hungerGamesMovie$Metascore
```
