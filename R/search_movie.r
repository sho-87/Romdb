#' Search for a movie in the database
#'
#' @param search_string string
#'
#' @return tbl_df
#' @export
#' @examples
#' search_movie("The Matrix")
search_movie <- function(search_string){
	#replace spaces with %20 for the URL
	search_string <- gsub(" ", "%20", search_string)
	#combine the URL and search string
	url <- paste("http://www.omdbapi.com/?s=", search_string, sep="")

	#return the results from the GET request
	searchResults <- httr::content(httr::GET(url))

	#create a dataframe for the results
	df <- data.frame(searchResults)

	#tidy up the column names
	colnames(df) <- gsub("^Search.", "", colnames(df))
	colnames(df) <- gsub(".[0-9]", "", colnames(df))

	#reshape the dataframe into long format
	df <- as.data.frame(lapply(split(as.list(df), names(df)), unlist))

	#select only the movies
	df <- subset(df, Type == "movie")

	#reorder the dataframe
	columns <- c("Title", "Type", "Year", "imdbID", "Poster")
	df <- df[columns]

	return(df)
}
