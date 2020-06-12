library(tidyverse)

read_scorecard <- function(){
	# test for presence of file in directory
	data_dir <- "data/scorecard.csv"
	if(file.exists(data_dir)){
		print(paste("Reading scorecard data from", data_dir))
		df <- read_csv(data_dir)
		return(df)
	}else{
		if(!dir.exists('data')){
			dir.create(file.path('data'), showWarnings = FALSE)
		}
		data_url <- "https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv"
		print(paste0("Downloading scorecard data from ", data_url, ". This might take a while."))
		df <- read_csv(data_url)
		df %>% 
			write_csv('data/scorecard.csv')
		return(df)
	} 
}

# For getting the list of schools with confirmed URLS
read_grade_urls <- function(){
	print(paste("Reading urls for confirmed grading policies"))
	df <- read_csv("data/gradePolicyURLs.csv")
	return(df)
}