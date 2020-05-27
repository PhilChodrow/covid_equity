library(tidyverse)
library(rvest)
library(stringr)

#' Attempts to find the official website of an American institution of higher education given a search term identifying that institution. 
#' Technically, this function (a) searches for the specified term on Bing and then (b) returns the url of the first page which ends with ".edu" 
edufy <- function(name){
	out <- name %>% 
		str_replace_all(" ", "+") %>% 
		paste0('https://www.bing.com/search?q=', .) %>% 
		read_html() %>% 
		html_nodes("cite") %>% 
		xml_text() %>% 
		purrr::keep(str_detect(., 'edu'))
	
	if(length(out) == 0){
		return(0)
	}
	else{
		return(out[1])
	}
} 

google_searchify <- function(edu, search_terms){
	domain_prefix <- "https://www.google.com/search?q=site%3A"
	search_terms <- search_terms %>% 
		str_replace_all(" ", "+")
	paste0(domain_prefix, edu, "+", search_terms)
}

# example usage
edufy("Hamilton College") %>% 
	google_searchify("fall semester COVID")
