library(tidyverse)
library(rvest)
library(pbmcapply)
source("R/get_data.R")

SEARCH_PREFIX <- "http://www.bing.com/search?q="
SEARCH_TERMS  <- "covid coronavirus spring 2020 grading policy undergraduate academic" # review scrape_cht.R if more advanced operators are required. 

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
	url <- url %>% 
		read_html() %>% 
		html_nodes("h2 a") %>% # All hyperlinks immediately in an h2 tag are results
		xml_attr("href") # Grab only the link
	
	#Sys.sleep(3)
	
	return(url)
}

# from R/get_data.R. Retrieves confirmed URLs for grading policy
grade_urls <- read_grade_urls() %>%
	filter(!is.na(grades_url)) %>%
	mutate(url      = tolower(url), #Get matching urls
				 url      = str_replace_all(url, ".edu.*",".edu"),
				 url      = str_replace_all(url, "/$",""),
				 has_http = grepl("^http", url),
				 url      = ifelse(has_http, url, paste0("http://", url)),
				 domain   = map_chr(url, ~str_split(.,"https*://")[[1]][[2]]),
				 domain   = str_replace_all(domain, "w+\\.|w+[1-9]\\.","")
	) %>% 
	select(-has_http)

# Collect schools from search list that have reference pages in grade_urls
school_urls <- filter(schools, INSTNM %in% grade_urls$institution, domain %in% grade_urls$domain)

school_urls <- school_urls %>%
	mutate(hits = pbmclapply(school_urls$search, get_page)) %>%
	unnest(c(hits))

# List of colleges that the correct page is in the collected hits (24/44)
grade_urls_in_bing <- filter(grade_urls, grades_url %in% school_urls$hits)

# Ranking results by order
school_order <- school_urls %>%
	select(INSTNM, domain, hits) %>%
	group_by(domain) %>%
	mutate(rank = row_number(INSTNM))

school_order_in_bing <- filter(school_order, hits %in% grade_urls$grades_url)

grade_urls_in_bing