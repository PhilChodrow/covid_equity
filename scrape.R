library(tidyverse)
library(rvest)
source("R/get_data.R")


SEARCH_PREFIX <- "http://www.bing.com/search?q="
SEARCH_TERMS  <- "covid grading semester policy" # review scrape_cht.R if more advanced operators are required. 

# from R/get_data.R. Retrieves the data set from the web and scores it in the data/ subdirectory, or reads it from that directory if it already exists. 
schools <- read_scorecard() 

# basic filtering. Replicates CHT's subset commands

schools <- schools %>% 
	filter(CCBASIC >= 15,
				 !(CCUGPROF %in% 0:4),
				   HIGHDEG  %in% 3:4,       
				   CCSIZSET %in% 6:17,    # Eliminate graduate-only institutions
				 !(CCBASIC  %in% 24:32), # Eliminate special focus schools
				 !(CONTROL==3),         # Eliminate for-profit schools
				 CURROPER==1) 			    # Eliminate institutions that are no longer operating

# clean out columns we don't need anymore
schools <- schools %>% 
	select(INSTNM, URL = INSTURL)

# construct urls + domains for different institutions
schools <- schools %>% 
	mutate(URL      = tolower(URL),
				 URL      = str_replace_all(URL, ".edu.*",".edu"),
				 URL      = str_replace_all(URL, "/$",""),
				 has_http = grepl("^http", URL),
				 URL      = ifelse(has_http, URL, paste0("http://", URL)),
				 domain   = map_chr(URL, ~str_split(.,"https*://")[[1]][[2]]),
				 domain   = str_replace_all(domain, "w+\\.|w+[1-9]\\.","")
				 ) %>% 
	select(-has_http)

# construct search URLS

schools <- schools %>% 
	mutate(search = SEARCH_PREFIX,
				 search = paste0(search, "site:", domain, "+"),
				 search = paste0(search, str_replace_all(SEARCH_TERMS, " ", "+")), 
				 search = map_chr(search, URLencode))

# Let's test gathering some links
get_page <- function(url){
	url %>% 
		read_html() %>% 
		html_nodes("cite") %>% 
		xml_text()
}

test <- schools %>% 
	head(10) %>% 
	mutate(hits = map(search, get_page)) %>% 
	unnest(c(hits)) %>% 
	rename(hits = `c(hits)`)

test # seems ok! 
