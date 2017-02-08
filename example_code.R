## Example code to run exact matching and output descriptives
library(data.table)
library(readr)     
library(stringr)
source("functions/createcensoc.R")
source("functions/getmatchdescriptives.R")


system.time(res <- create.censoc(census.file = "OR.txt",
                     ssn.min = 540, ssn.max = 544, 
                     socsec.file.list = c("ssdm3"),
                     counts.file.name = "match_metrics/OR_counts.txt", 
                     descriptives.file.name = "match_metrics/OR_descriptives.csv",
                     matched.file.name = "matched_datasets/OR_matched.csv"))


