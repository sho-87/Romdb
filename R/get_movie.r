#' Get information for a specific movie
#'
#' @param title string
#'
#' @return list
#' @export
#' @examples
#' movieInfo <- get_movie("The Matrix")
get_movie <- function(title){
	#replace spaces with %20 for the URL
	title <- gsub(" ", "%20", title)
	#combine the URL and movie name
	url <- paste("http://www.omdbapi.com/?t=", title, "&tomatoes=true", sep="")

	#return the results from the GET request
	movieInfo <- httr::content(httr::GET(url))

	#If there is a match, return the info in a list. Else, return a message saying no movie was found
	if (movieInfo$Response == "True") {
		return(movieInfo)
	} else {
		return("Movie not found. Please try again...")
	}
}
