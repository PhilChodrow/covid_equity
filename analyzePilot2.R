# Make sample for PASS20 data

# Load libraries
library(tidyverse)
library(fuzzyjoin)

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

# Recode prepandemic
data$prepandemic <- as.factor(data$prepandemic)
thelevels <- levels(data$prepandemic)
data$prepandemic <- fct_recode(data$prepandemic, cannotdetermine = "Cannot determine")
data$prepandemic <- fct_recode(data$prepandemic, letter = "Letter grades (typically, A/B/C/D/F, or a close variation thereof)")
data$prepandemic <- fct_recode(data$prepandemic, pf = "Pass/Fail or High Pass/Pass/Fail or Satisfactory/Unsatisfactory (or a close variation thereof)")
data$prepandemic <- fct_recode(data$prepandemic, pnc = "Pass/NoCredit or High Pass/Pass/No Credit (or a close variation thereof)")

# Recode studentchoice
data$studentchoice <- as.factor(data$studentchoice)
thelevels <- levels(data$studentchoice)
data$studentchoice <- fct_recode(data$studentchoice, nochoice = "No, despite the COVID pandemic, students were not given more choice than usual")
data$studentchoice <- fct_recode(data$studentchoice, choice = "Yes, students were given more choice than usual due to the COVID pandemic")

# Make studentchoiceconfidence
data$studentchoiceconfidence <- rowSums(cbind(data$studentchoiceconfidence,data$studentnochoiceconfidence),na.rm=TRUE)
drops <- c("studentnochoiceconfidence")
data <- subset(data,select=setdiff(names(data),drops))

# Recode prepandemicdefaultcompare
data$prepandemicdefaultcompare <- as.factor(data$prepandemicdefaultcompare)
thelevels <- levels(data$prepandemicdefaultcompare)
data$prepandemicdefaultcompare <- fct_recode(data$prepandemicdefaultcompare, cannotdetermine = "Cannot determine")
data$prepandemicdefaultcompare <- fct_recode(data$prepandemicdefaultcompare, same = "The pandemic default method was the same as the usual pre-pandemic method")
data$prepandemicdefaultcompare <- fct_recode(data$prepandemicdefaultcompare, different = "The pandemic default method was different from the usual pre-pandemic method")

# Make prepandemicdefaultcompareconfidence
data$prepandemicdefaultcompareconfidence <- rowSums(cbind(data$prepandemicdifferentconfidence,data$prepandemicsameconfidence),na.rm=TRUE)
drops <- c("prepandemicdifferentconfidence","prepandemicsameconfidence")
data <- subset(data,select=setdiff(names(data),drops))

# Recode pandemicdefault
data$pandemicdefault <- as.factor(data$pandemicdefault)
thelevels <- levels(data$pandemicdefault)
data$pandemicdefault <- fct_recode(data$pandemicdefault, letter = "Averages are computed in grade points. Each graded semester hour of academic credit carries a corresponding number of grade points.")
data$pandemicdefault <- fct_recode(data$pandemicdefault, cannotdetermine = "Cannot determine")
data$pandemicdefault <- fct_recode(data$pandemicdefault, letter = "Letter grades (typically, A/B/C/D/F, or a close variation thereof)")
data$pandemicdefault <- fct_recode(data$pandemicdefault, letter = "Letter grades with the inclusion of a Late Withdrawal (W) and No Credit (NC)")
data$pandemicdefault <- fct_recode(data$pandemicdefault, pf = "Pass/Fail or High Pass/Pass/Fail or Satisfactory/Unsatisfactory (or a close variation thereof)")
data$pandemicdefault <- fct_recode(data$pandemicdefault, pnc = "Pass/NoCredit or High Pass/Pass/No Credit (or a close variation thereof)")

# Recode pandemicoptin
data$pandemicoptin <- as.factor(data$pandemicoptin)
thelevels <- levels(data$pandemicoptin)
data$pandemicoptin <- fct_recode(data$pandemicoptin, cannotdetermine = "Cannot determine")
data$pandemicoptin <- fct_recode(data$pandemicoptin, cw = "Covid Withdrawal")
data$pandemicoptin <- fct_recode(data$pandemicoptin, pf = "For Spring 2020, students will have two weeks after grades are published to submit a petition to change a class from standard letter grade (A,B,C,D,F) to Pass/D/Fail.")
data$pandemicoptin <- fct_recode(data$pandemicoptin, cwnc = "Late Withdrawal (W) and No Credit (NC)")
data$pandemicoptin <- fct_recode(data$pandemicoptin, letter = "Letter grades (typically, A/B/C/D/F, or a close variation thereof)")
data$pandemicoptin <- fct_recode(data$pandemicoptin, pf = "Pass/Fail")
data$pandemicoptin <- fct_recode(data$pandemicoptin, pf = "Pass/Fail or High Pass/Pass/Fail or Satisfactory/Unsatisfactory (or a close variation thereof)")
data$pandemicoptin <- fct_recode(data$pandemicoptin, pnc = "Pass/NoCredit or High Pass/Pass/No Credit (or a close variation thereof)")
data$pandemicoptin <- fct_recode(data$pandemicoptin, pf = "Satisfactory/Satisfactory D/Fail")

# Recode howappliesstudentchoice
data$howappliesstudentchoice <- as.factor(data$howappliesstudentchoice)
thelevels <- levels(data$howappliesstudentchoice)
data$howappliesstudentchoice <- fct_recode(data$howappliesstudentchoice, cannotdetermine = "Cannot determine")
data$howappliesstudentchoice <- fct_recode(data$howappliesstudentchoice, singlechoice = "No: the student was required to make a single choice (pandemic default or pandemic opt-in) which was applied to all courses")
data$howappliesstudentchoice <- fct_recode(data$howappliesstudentchoice, partialpercourse = "Partially: Some courses allowed students to make a choice between the pandemic default method and the pandemic opt-in method, while for other courses, the instructor or institution chose the grading method")
data$howappliesstudentchoice <- fct_recode(data$howappliesstudentchoice, other = "Some classes moved to a pass / fail grading system, but students have to log in and see if their particular class is affected. In order to access this information a person needs a log in and password." )
data$howappliesstudentchoice <- fct_recode(data$howappliesstudentchoice, partialpercourse = "Students could only select up to two courses to change to pass/fail")
data$howappliesstudentchoice <- fct_recode(data$howappliesstudentchoice, partialpercourse = "The students could only change up to two of their Spring Semester 2020 courses to Pass/Fail within a given time period.")
data$howappliesstudentchoice <- fct_recode(data$howappliesstudentchoice, partialpercourse = "They could make up to two courses Pass/Fail")
data$howappliesstudentchoice <- fct_recode(data$howappliesstudentchoice, fullchoice = "Yes: the student could choose between the pandemic default method and pandemic opt-in method for each of their courses, independently")
data$howappliesstudentchoice[data$howappliesstudentchoice=="The student doesn't have a choice"] <- NA

# Recode deadline
data$deadline <- as.factor(data$deadline)
thelevels <- levels(data$deadline)
data$deadline <- fct_recode(data$deadline, after = "After they saw what their grade would be using the pandemic default method")
data$deadline <- fct_recode(data$deadline, before = "Before they saw what their grade would be under the pandemic default method")
data$deadline <- fct_recode(data$deadline, cannotdetermine = "Cannot determine")
goodvals <- c("after","before","cannotdetermine")
data$deadline[!data$deadline %in% goodvals] <- NA
data$deadline <- factor(data$deadline)

# Recode prepandemicpandemiccompare
data$prepandemicpandemiccompare <- as.factor(data$prepandemicpandemiccompare)
thelevels <- levels(data$prepandemicpandemiccompare)
data$prepandemicpandemiccompare <- fct_recode(data$prepandemicpandemiccompare, cannotdetermine = "Cannot determine")
data$prepandemicpandemiccompare <- fct_recode(data$prepandemicpandemiccompare, same = "The pandemic grading method was the same as the usual pre-pandemic grading method" )
data$prepandemicpandemiccompare <- fct_recode(data$prepandemicpandemiccompare, different = "The pandemic grading method was different from the usual pre-pandemic grading method")

# Make prepandemicpandemiccompareconfidence
data$prepandemicpandemiccompareconfidence <- rowSums(cbind(data$pandemicdifferentconfidence,data$pandemicsameconfidence),na.rm=TRUE)
drops <- c("pandemicdifferentconfidence","pandemicsameconfidence")
data <- subset(data,select=setdiff(names(data),drops))

# Recode pandemic
data$pandemic <- as.factor(data$pandemic)
thelevels <- levels(data$pandemic)
data$pandemic <- fct_recode(data$pandemic, cannotdetermine = "Cannot determine")
data$pandemic <- fct_recode(data$pandemic, letter = "Letter grades (typically, A/B/C/D/F, or a close variation thereof)")
data$pandemic <- fct_recode(data$pandemic, pf = "Pass/Fail or High Pass/Pass/Fail or Satisfactory/Unsatisfactory (or a close variation thereof)")
data$pandemic <- fct_recode(data$pandemic, pnc = "Pass/NoCredit or High Pass/Pass/No Credit (or a close variation thereof)")
goodvals <- c("cannotdetermine","letter","pf","pnc")
data$pandemic[!data$pandemic %in% goodvals] <- NA
data$pandemic <- factor(data$pandemic)

# Recode howappliesinstructorchoice
data$howappliesinstructorchoice <- as.factor(data$howappliesinstructorchoice)
thelevels <- levels(data$howappliesinstructorchoice)
data$howappliesinstructorchoice <- fct_recode(data$howappliesinstructorchoice, cannotdetermine = "Cannot determine")
data$howappliesinstructorchoice <- fct_recode(data$howappliesinstructorchoice, instructorchoice = "Each course instructor could choose if the pandemic grading method would apply to their course")
data$howappliesinstructorchoice <- fct_recode(data$howappliesinstructorchoice, institutionchoice = "Instructors had no choice, meaning that one single pandemic grading method chosen by the college/university automatically applied to all courses")


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

