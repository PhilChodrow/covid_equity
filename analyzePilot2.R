# Make sample for PASS20 data

# Load libraries
library(tidyverse)
library(fuzzyjoin)

# Set path
# path <- "~/Google Drive/Research/Projects/Current Research/PASS20/"

# Load data

googledata <- read_csv("data/pilotGoogleFormdata2.csv")
mturkdata <- read_csv("data/pilotOutputData2.csv")


# Clean data in google workerID
names(googledata)[2] <- "WorkerId"
names(googledata)[3] <- "Input.INSTNM"

# Merge Google and Mturk info
commonnames <- c("WorkerId","Input.INSTNM")
googleleftovernames <- paste0(commonnames,".x")
mturkleftovernames <- paste0(commonnames,".y")
data <- merge(mturkdata,googledata)

# See what is in googledata that didn't make it in to data
googleleftovers  <- anti_join(googledata,data)

# See what is in mturkdata that didn't make it in to data
mturkleftovers  <- anti_join(mturkdata,data)

# Add guess-y data made from leftovers
guessydata <- merge(googleleftovers,mturkleftovers,by="Input.INSTNM")
names(guessydata)[names(guessydata)=="WorkerId.y"] <- "WorkerId"
thevars <- names(data)
moredata <- subset(guessydata,select=names(data))
data <- rbind(data,moredata)

# Select only needed columns in the good data
#names(data)
keepcols <- c(3,1,2,25,44:66)
data <- data[keepcols]

# Give more convenient column names
names(data)
names(data)[1] <- "hitID"
names(data)[2] <- "workerID"
names(data)[3] <- "institution"
names(data)[4] <- "worktime"
names(data)[5] <- "prepandemic"
names(data)[6] <- "prepandemicconfidence"
names(data)[7] <- "studentchoice"
names(data)[8] <- "studentchoiceconfidence"
names(data)[9] <- "studentnochoiceconfidence"
names(data)[10] <- "prepandemicdefaultcompare"
names(data)[11] <- "prepandemicdifferentconfidence"
names(data)[12] <- "prepandemicsameconfidence"
names(data)[13] <- "pandemicdefault"
names(data)[14] <- "pandemicdefaultconfidence"
names(data)[15] <- "pandemicoptin"
names(data)[16] <- "pandemicoptinconfidence"
names(data)[17] <- "howappliesstudentchoice"
names(data)[18] <- "howappliesstudentchoiceconfidence"
names(data)[19] <- "deadline"
names(data)[20] <- "deadlineconfidence"
names(data)[21] <- "prepandemicpandemiccompare"
names(data)[22] <- "pandemicsameconfidence"
names(data)[23] <- "pandemicdifferentconfidence"
names(data)[24] <- "pandemic"
names(data)[25] <- "pandemicconfidence"
names(data)[26] <- "howappliesinstructorchoice"
names(data)[27] <- "howappliesinstructorchoiceconfidence"

# Replace any empty strings with NA
data[data==""] <- NA

# Recode factors

data <- data %>% 
	tibble() %>% 
	# recode prepandemic
	mutate(prepandemic = factor(prepandemic),
				 prepandemic = fct_recode(prepandemic,
					 	cannotdetermine = "Cannot determine",
					 	letter          = "Letter grades (typically, A/B/C/D/F, or a close variation thereof)",
					 	pf              = "Pass/Fail or High Pass/Pass/Fail or Satisfactory/Unsatisfactory (or a close variation thereof)",
					 	pnc             = "Pass/NoCredit or High Pass/Pass/No Credit (or a close variation thereof)"
				 													)
	) %>% 
	# recode studentchoice
	mutate(studentchoice = factor(studentchoice),
				 studentchoice = fct_recode(studentchoice,
				 		nochoice = "No, despite the COVID pandemic, students were not given more choice than usual",
				 		choice   = "Yes, students were given more choice than usual due to the COVID pandemic"
				 														)
	) %>% 
 	# make studentchoiceconfidence
	rowwise() %>% 
 	mutate(studentchoiceconfidence = sum(studentchoiceconfidence, studentnochoiceconfidence, na.rm = T)) %>% 
	ungroup() %>% 
	select(-studentnochoiceconfidence) %>% 
	# recode prepandemicdefaultcompare
	mutate(
		prepandemicdefaultcompare = factor(prepandemicdefaultcompare),
		prepandemicdefaultcompare = fct_recode(prepandemicdefaultcompare,
				 cannotdetermine = "Cannot determine",
				 same            = "The pandemic default method was the same as the usual pre-pandemic method",
				 different       = "The pandemic default method was different from the usual pre-pandemic method"
																					 )
	) %>% 
	# Make prepandemicdefaultcompareconfidence
	rowwise() %>% 
	mutate(prepandemicdefaultcompareconfidence = sum(prepandemicdifferentconfidence, prepandemicsameconfidence, na.rm = T)) %>% 
	ungroup() %>% 
	select(-prepandemicdifferentconfidence, -prepandemicsameconfidence) %>% 
	# Recode pandemicdefault
	mutate(pandemicdefault = factor(pandemicdefault),
				 pandemicdefault = fct_recode(pandemicdefault,
				 														 letter = "Averages are computed in grade points. Each graded semester hour of academic credit carries a corresponding number of grade points.",
				 														 cannotdetermine = "Cannot determine",
				 														 letter = "Letter grades (typically, A/B/C/D/F, or a close variation thereof)",
				 														 letter = "Letter grades with the inclusion of a Late Withdrawal (W) and No Credit (NC)",
				 														 pf = "Pass/Fail or High Pass/Pass/Fail or Satisfactory/Unsatisfactory (or a close variation thereof)",
				 														 pnc = "Pass/NoCredit or High Pass/Pass/No Credit (or a close variation thereof)"
				 														 )
				 ) %>% 
# Recode pandemicoptin 
	mutate(pandemicoptin = factor(pandemicoptin),
				 pandemicoptin = fct_recode(pandemicoptin, 
				 													 cannotdetermine = "Cannot determine",
				 													 cw = "Covid Withdrawal",
				 													 pf = "For Spring 2020, students will have two weeks after grades are published to submit a petition to change a class from standard letter grade (A,B,C,D,F) to Pass/D/Fail.",
				 													 cwnc = "Late Withdrawal (W) and No Credit (NC)",
				 													 letter = "Letter grades (typically, A/B/C/D/F, or a close variation thereof)",
				 													 pf = "Pass/Fail",
				 													 pf = "Pass/Fail or High Pass/Pass/Fail or Satisfactory/Unsatisfactory (or a close variation thereof)",
				 													 pnc = "Pass/NoCredit or High Pass/Pass/No Credit (or a close variation thereof)",
				 													 pf = "Satisfactory/Satisfactory D/Fail"
				 													 )
	) %>% 
	# Recode howappliesstudentchoice
	mutate(howappliesstudentchoice = ifelse(howappliesstudentchoice == "The student doesn't have a choice", NA, howappliesstudentchoice),
		howappliesstudentchoice = factor(howappliesstudentchoice),
				 howappliesstudentchoice = fct_recode(howappliesstudentchoice,
				 																		 cannotdetermine = "Cannot determine",
				 																		 singlechoice = "No: the student was required to make a single choice (pandemic default or pandemic opt-in) which was applied to all courses",
				 																		 partialpercourse = "Partially: Some courses allowed students to make a choice between the pandemic default method and the pandemic opt-in method, while for other courses, the instructor or institution chose the grading method",
				 																		 other = "Some classes moved to a pass / fail grading system, but students have to log in and see if their particular class is affected. In order to access this information a person needs a log in and password.",
				 																		 partialpercourse = "Students could only select up to two courses to change to pass/fail",
				 																		 partialpercourse = "The students could only change up to two of their Spring Semester 2020 courses to Pass/Fail within a given time period.",
				 																		 partialpercourse = "They could make up to two courses Pass/Fail",
				 																		 fullchoice = "Yes: the student could choose between the pandemic default method and pandemic opt-in method for each of their courses, independently"
				 																		 )) %>% 
# Recode deadline (NEED TO FIX THIS)
	mutate(deadline = ifelse(deadline %in% c(
		"After they saw what their grade would be using the pandemic default method",
		"Before they saw what their grade would be under the pandemic default method",
		"Cannot determine"), deadline, NA),
		deadline = factor(deadline),
				 deadline = fct_recode(deadline,
				 		after = "After they saw what their grade would be using the pandemic default method",
				 		before = "Before they saw what their grade would be under the pandemic default method",
				 		cannotdetermine = "Cannot determine",
				 											)
				) %>% 
	# Recode prepandemicpandemiccompare
	mutate(prepandemicpandemiccompare = factor(prepandemicpandemiccompare),
				 prepandemicpandemiccompare = fct_recode(prepandemicpandemiccompare,
				 																				cannotdetermine = "Cannot determine",
				 																				same = "The pandemic grading method was the same as the usual pre-pandemic grading method",
				 																				different = "The pandemic grading method was different from the usual pre-pandemic grading method"
				 																				)
	) %>% 
	rowwise() %>% 
	mutate(prepandemicpandemiccompareconfidence = sum(pandemicdifferentconfidence, pandemicsameconfidence, na.rm = T)) %>% 
	ungroup() %>% 
	select(-pandemicdifferentconfidence, -pandemicsameconfidence) %>% 
# Recode pandemic
	mutate(
		pandemic = ifelse(pandemic %in% c(
			"Cannot determine",
			"Letter grades (typically, A/B/C/D/F, or a close variation thereof)",
			"Pass/Fail or High Pass/Pass/Fail or Satisfactory/Unsatisfactory (or a close variation thereof)",
			"Pass/NoCredit or High Pass/Pass/No Credit (or a close variation thereof)"), 
			pandemic, NA),
		pandemic = factor(pandemic),
		pandemic = fct_recode(pandemic,
													cannotdetermine = "Cannot determine",
													letter = "Letter grades (typically, A/B/C/D/F, or a close variation thereof)",
													pf = "Pass/Fail or High Pass/Pass/Fail or Satisfactory/Unsatisfactory (or a close variation thereof)",
													pnc = "Pass/NoCredit or High Pass/Pass/No Credit (or a close variation thereof)",
		)
	) %>% 
# Recode howappliesinstructorchoice
	mutate(howappliesinstructorchoice = factor(howappliesinstructorchoice),
				 howappliesinstructorchoice = fct_recode(howappliesinstructorchoice,
				 																				cannotdetermine = "Cannot determine",
				 																				instructorchoice = "Each course instructor could choose if the pandemic grading method would apply to their course",
				 																				institutionchoice = "Instructors had no choice, meaning that one single pandemic grading method chosen by the college/university automatically applied to all courses"
				 																				)
				 )

data %>% write_csv("throughput/pilot3.csv")



# Put columns in order
data <- data[,c(1:9,23,10:18,24,19:22)]

# Analyze worker time
median(data$worktime)
mean(data$worktime)
# Median is 555 sec, mean is 598 sec, let's go with 600 sec, which is 10 min
# 10 min/HIT is equivalent to 6 HITs/hr
# To achieve rate of $10.50/hr, we need to pay $1.75/HIT

# Get list of schools
schools <- unique(data$institution)

# Write generic analysis function
analyzeVar <- function(school,myvar,myvarconf,thresh,minbest,mincount){
  dat <- subset(data,institution==school)
  vals <- subset(dat,select=myvar)[,1]
  confidence <- subset(dat,select=myvarconf)[,1]
  key <- !is.na(confidence)
  tab <- table(rep(vals[key],confidence[key]))
  ind <- which.max(tab)
  finalval <- NA
  if (tab[ind] >= thresh & max(confidence,na.rm=TRUE) >= minbest & max(table(vals)) >= mincount){
    finalval <- names(tab[ind])
  }
  return(finalval)
}

# Set up for analysis
thresh <- 7
minbest <- 2
mincount <- 2
resultscols <- c(seq(from=5,to=23,by=2))
results <- vector(mode = "list", length = length(resultscols))

# Analyze
count <- 1
for (col in resultscols){
  results[[count]]  <- sapply(schools,analyzeVar,names(data)[col],names(data)[col+1],thresh,minbest,mincount)
  count <- count + 1
}

# Transform results into nice data frame
results <- do.call(cbind,results)
results <- cbind(schools,results)
results <- as.data.frame(results)
names(results) <- names(data)[c(3,seq(from=5,by=2,to=23))]

# Consolidate data as needed - create default postpandemic
postpandemic <- rep(NA,nrow(results))
ind <- results$studentchoice == "nochoice" & results$prepandemicpandemiccompare == "same"
ind[is.na(ind)] <- FALSE
postpandemic[ind] <- results[ind,]$prepandemic
ind <- results$studentchoice == "nochoice" & results$prepandemicpandemiccompare == "different"
ind[is.na(ind)] <- FALSE
postpandemic[ind] <- results[ind,]$pandemic
ind <- results$studentchoice == "choice" & results$prepandemicdefaultcompare == "same"
ind[is.na(ind)] <- FALSE
postpandemic[ind] <- results[ind,]$prepandemic
ind <- results$studentchoice == "choice" & results$prepandemicdefaultcompare == "different"
ind[is.na(ind)] <- FALSE
postpandemic[ind] <- results[ind,]$pandemicdefault
results2 <- data.frame(institution=results$institution,prepandemic=results$prepandemic,pandemic=postpandemic,pandemicoptin=results$pandemicoptin,studentchoice=results$stu)

