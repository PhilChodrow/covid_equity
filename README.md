# Equity in Grading Policies in Response to COVID19

This repository hosts code for studying the grading policies of American colleges and universities in response to the COVID-19 pandemic. 

## Getting Data

One of our primary data sets in this project is the [College Scorecard](https://catalog.data.gov/dataset/college-scorecard/resource/77d2e376-c5bb-46d7-a985-e214e009e36e) compiled by the US Department of Education. Instead of manually downloading the data and figuring out where to put it, we've implemented an easy-to-use function called `read_scorecard()`.
To use it, it is necessary to open the project `covid_equity.Rproj` in order to ensure that your working directory is appropriately handled. Then, run: 

```r
source("R/get_data.R")
data <- read_scorecard()
```

If you're running this command for the first time, it will create a `data` directory and then download the data from USDE (this might take a little while). It will then load the data into your R session. If you run the command again, it will instead load the data from the `data` directory. It is not recommended to place the `data` folder under version control, and this is enforced in the `.gitignore` file. 

## Data Documentation

Several documentation resources, including a data dictionary for the columns contained in the College Scorecard data, are available [here](https://collegescorecard.ed.gov/data/documentation/). 

## Software Requirements

Tidyr version greater than 1.0.0 is required.