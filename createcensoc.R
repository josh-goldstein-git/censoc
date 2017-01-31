create.censoc <- function(census.file = "/home/ipums/josh-ipums/mydata/my1940/CA_clean.txt",
                          names.file = "/home/ipums/monica-ipums/censoc/tmp.txt",
                          socsec.file = "/home/ipums/josh-ipums/progs/ssdm/ssdm3",
                          ssn.min, ssn.max, # http://www.stevemorse.org/ssn/ssn.html
                          sex.to.keep = "Male",
                          counts.file.name = "counts.txt",
                          descriptives.file.name = "descriptives.csv",
                          return.unmatched = FALSE){
  
  ######### 1. READ IN DATA ######### 
  cat("Reading census data.\n")
  ## read in census
  census <- fread(census.file)
  names.census <- fread(names.file, nrows = 10)
  setnames(census, names(names.census))
  
  cat("Reading socsec data.\n")
  ## read in socsec
  tt <- readr::read_fwf(socsec.file,
                        fwf_widths(c(1,9, 20, 4, 15, 15, 1, 8, 8),
                                   col_names = c("mode", "ssn", "lname",
                                                 "name_suffix", "fname", "mname",
                                                 "vorpcode", "dod", "dob")))
  socsec <- as.data.table(tt) 
  rm(tt)
  ## restrict to the SSN geography codes of interest
  socsec[, "ssn.geo" := as.numeric(substr(ssn, 1, 3))] # creates a new column with geo codes
  socsec <- socsec[ssn.geo >= ssn.min & ssn.geo <= ssn.max,]
  
  ## METRIC: raw data counts
  n.census.raw <- nrow(census)
  n.socsec.raw <- nrow(socsec)
  
  ######### 2. CLEAN AND CREATE KEYS #########
  
  ## A. clean the socsec data
  ## shorten names by removing blank space at end
  socsec[,lname := gsub(pattern = "\\s*$",
                        replacement = "", x = lname)]
  socsec[,fname := gsub(pattern = "\\s*$",
                        replacement = "", x = fname)]
  ## now get birth and death year
  socsec[,"byear" := as.numeric(substr(dob, 5, 9))]
  socsec[,"dyear" := as.numeric(substr(dod, 5, 9))]
  ## birth and death month
  socsec[,"bmonth" := as.numeric(substr(dob, 1, 2))]
  socsec[,"dmonth" := as.numeric(substr(dob, 1, 2))]
  ## now get census_age (hopefully census was on April 1)
  socsec[,"census_age" := ifelse(bmonth < 4,
                                 1940 - byear,
                                 1939 - byear)]
  
  ## METRIC: number socsec born after 1940
  n.socsec.post.1940 <- nrow(socsec[which(socsec$census_age<0),])
  
  ## get rid of these
  socsec.missing.age <- socsec[socsec$census_age==1940|socsec$census_age==1939,]
  ## METRIC: number socsec missing age info
  n.socsec.age.missing <- nrow(socsec.missing.age)
  socsec <- socsec[socsec$census_age!=1940&socsec$census_age!=1939,]
  socsec <- socsec[socsec$census_age>=0,]
  
  # create key
  socsec[,"tmp_key" := paste0(lname, fname, census_age)]
  socsec[,"n_tmp_key" := .N, by = tmp_key]
  socsec[,"clean_key" := clean.key(tmp_key),]
  socsec[,"n_clean_key" := .N, by = clean_key]
  
  ## B. clean the census data
  census[,"fname" := str_to_upper(self_empty_name_given)]
  census[,"lname" := str_to_upper(self_empty_name_surname)]
  census[,"age" := as.numeric(self_residence_info_age)]
  census[,"census_age" := age]
  census[,"sex" := self_empty_info_gender]
  census[,"fname" := get.first.word(fname)]
  # income (just to take an example of one covariate)
  census[,"income" := general_income,]
  census[,"income" := gsub(",", "", income)]
  census[,"income" := as.numeric(income)]
  ## HHID_NUMERIC is unique HHID for each household
  ## HHORDER is the order of the person in unique HH
  census[,"hhid" := HHID_NUMERIC]
  census[,"recno" := HHORDER]
  
  census <- census[,.(hhid, recno, fname, lname, age, census_age, sex,
                      income)]
  
  # remove those without ages
  census.missing.age <- census[is.na(census$census_age),]
  ## METRIC
  n.census.age.missing <- nrow(census.missing.age)
  census <- census[!is.na(census$census_age),]

  # remove those with no name info 
  ## remove blanks or anything that has question marks (for now)
  census.missing.names <- census[(grepl("\\?", census$fname)|grepl("\\?", census$lname)|census$lname==""),]
  ## METRIC
  n.census.names.missing <- nrow(census.missing.names)
  census <- census[!(grepl("\\?", census$fname)|grepl("\\?", census$lname)|census$lname==""),]

  # create key
  census[,"tmp_key" := paste0(lname, fname, census_age)]
  census[,"n_tmp_key" := .N, by = tmp_key]
  census[,"clean_key" := clean.key(tmp_key),]
  census[,"n_clean_key" := .N, by = clean_key]

  ######### 3. MATCH #########
  
  # METRICS
  n.census <- nrow(census)
  n.socsec <- nrow(socsec)
  
  socsec.uniq <- socsec[n_clean_key == 1,]
  census.uniq <- census[n_clean_key == 1,]
  # METRICS
  n.socsec.uniq <- nrow(socsec.uniq)
  n.census.uniq <- nrow(census.uniq)
  
  ## Drop if not male
  census.uniq.male <- census.uniq[census.uniq$sex == sex.to.keep,]
  # METRIC
  n.census.sex.uniq <- nrow(census.uniq.male)
  ## Reset keys
  setkey(socsec.uniq, clean_key)
  setkey(census.uniq.male, clean_key)
  
  ## Merge on unique keys
  #out <- socsec.uniq[census.uniq.male]
  out <- merge(census.uniq.male, socsec.uniq, on = clean_key)
  # METRIC
  n.censoc <- nrow(out)
  censoc <- out
  
  ############ 4. DESCRIPTIVES #############
  
  census.uniq.unmatched <- census.uniq.male[!(census.uniq.male$clean_key %in% censoc$clean_key),]
  socsec.uniq.unmatched <- socsec.uniq[!(socsec.uniq$clean_key %in% censoc$clean_key),]
  census.nonuniq.unmatched <- census[census$n_clean_key>1&census$sex==sex.to.keep,]
  
  # number of unique keys not matched
  ## summaries for matched, non-matched unique, non-matched non-unique
  
  # median age
  med.age.matched <- median(censoc$census_age.x)
  iqr.age.matched <- quantile(censoc$census_age.x, 0.75) - quantile(censoc$census_age.x, 0.25)
  
  med.age.unmatched.census.uniq <- median(census.uniq.unmatched$census_age)
  iqr.age.unmatched.census.uniq <- quantile(census.uniq.unmatched$census_age, 0.75) - quantile(census.uniq.unmatched$census_age, 0.25)
  
  med.age.unmatched.socsec.uniq <- median(socsec.uniq.unmatched$census_age)
  iqr.age.unmatched.socsec.uniq <- quantile(socsec.uniq.unmatched$census_age, 0.75) - quantile(socsec.uniq.unmatched$census_age, 0.25)
  
  med.age.unmatched.all.uniq <- median(c(census.uniq.unmatched$census_age, socsec.uniq.unmatched$census_age))
  iqr.age.unmatched.all.uniq <- quantile(c(census.uniq.unmatched$census_age, socsec.uniq.unmatched$census_age), 0.75) - quantile(c(census.uniq.unmatched$census_age, socsec.uniq.unmatched$census_age), 0.25)
  
  med.age.unmatched.census.nonuniq <- median(census.nonuniq.unmatched$census_age)
  iqr.age.unmatched.census.nonuniq <- quantile(census.nonuniq.unmatched$census_age, 0.75) - quantile(census$census_age[census$n_clean_key>1], 0.25)
  
  med.age.unmatched.socsec.nonuniq <- median(socsec$census_age[socsec$n_clean_key>1])
  iqr.age.unmatched.socsec.nonuniq <- quantile(socsec$census_age[socsec$n_clean_key>1], 0.75) - quantile(socsec$census_age[socsec$n_clean_key>1], 0.25)
  
  med.age.unmatched.all.nonuniq <- median(c(census.nonuniq.unmatched$census_age,
                                            socsec$census_age[socsec$n_clean_key>1]))
  iqr.age.unmatched.all.nonuniq <- quantile(c(census.nonuniq.unmatched$census_age,
                                              socsec$census_age[socsec$n_clean_key>1]), 0.75) - quantile(c(census.nonuniq.unmatched$census_age,
                                                                                                           socsec$census_age[socsec$n_clean_key>1]), 0.25)
  
  ## age at death
  censoc[, age.at.death := dyear - byear]
  socsec[, age.at.death := dyear - byear]
  socsec.uniq.unmatched[, age.at.death := dyear - byear]
  
  med.aad.matched <- median(censoc$age.at.death)
  iqr.aad.matched <- quantile(censoc$age.at.death, 0.75) - quantile(censoc$age.at.death, 0.25)
  
  med.aad.unmatched.uniq <- median(socsec.uniq.unmatched$age.at.death)
  iqr.aad.unmatched.uniq <- quantile(socsec.uniq.unmatched$age.at.death, 0.75) - quantile(socsec.uniq.unmatched$age.at.death, 0.25)
  
  med.aad.unmatched.nonuniq <- median(socsec$age.at.death[socsec$n_clean_key>1])
  iqr.aad.unmatched.nonuniq <- quantile(socsec$age.at.death[socsec$n_clean_key>1], 0.75) - quantile(socsec$age.at.death[socsec$n_clean_key>1], 0.25)
  
  ## median income
  
  med.income.matched <- median(censoc$income, na.rm = T)
  prop.income.missing.matched <- sum(is.na(censoc$income))/nrow(censoc)
  prop.income.zero.matched <- sum(censoc$income==0, na.rm=T)/nrow(censoc)
  
  med.income.unmatched.uniq <- median(census.uniq.unmatched$income, na.rm=T)
  prop.income.missing.unmatched.uniq <- sum(is.na(census.uniq.unmatched$income))/nrow(census.uniq.unmatched)
  prop.income.zero.unmatched.uniq <- sum(census.uniq.unmatched$income==0, na.rm=T)/nrow(census.uniq.unmatched)
  
  med.income.unmatched.nonuniq <- median(census.nonuniq.unmatched$income, na.rm=T)
  prop.income.missing.unmatched.nonuniq <- sum(is.na(census.nonuniq.unmatched$income))/nrow(census.nonuniq.unmatched)
  prop.income.zero.unmatched.nonuniq <- sum(census.nonuniq.unmatched$income==0, na.rm=T)/nrow(census.nonuniq.unmatched)
  
  ## Put everything in a dataframe
  
  df <- data.frame(data = c("Matched", 
                            "Unmatched unique census", "Unmatched unique socsec", "Unmatched unique all", 
                            "Unmatched non-unique census", "Unmatched non-unique socsec", "Unmatched non-unique all"),
                   no.obs = rep(NA,7),
                   med.age = rep(NA, 7), iqr.age = rep(NA,7), 
                   med.aad = rep(NA, 7), iqr.aad = rep(NA,7),
                   med.income = rep(NA,7), prop.income.missing = rep(NA,7), prop.income.zero = rep(NA,7))
  
  df[1,2:9] <- c(nrow(censoc),med.age.matched, iqr.age.matched, med.aad.matched, iqr.aad.matched, med.income.matched, prop.income.missing.matched, prop.income.zero.matched)
  df[2,2:9] <- c(nrow(census.uniq.unmatched),med.age.unmatched.census.uniq, iqr.age.unmatched.census.uniq, NA, NA, med.income.unmatched.uniq, prop.income.missing.unmatched.uniq, prop.income.zero.unmatched.uniq)
  df[3,2:9] <- c(nrow(socsec.uniq.unmatched),med.age.unmatched.socsec.uniq, iqr.age.unmatched.socsec.uniq, med.aad.unmatched.uniq, iqr.aad.unmatched.uniq,NA, NA, NA)
  df[4,2:9] <- c(nrow(census.uniq.unmatched)+nrow(socsec.uniq.unmatched),med.age.unmatched.all.uniq, iqr.age.unmatched.all.uniq, NA,NA,NA, NA, NA)
  df[5,2:9] <- c(nrow(census.nonuniq.unmatched),med.age.unmatched.census.nonuniq, iqr.age.unmatched.census.nonuniq, NA, NA, med.income.unmatched.nonuniq, prop.income.missing.unmatched.nonuniq, prop.income.zero.unmatched.nonuniq)
  df[6,2:9] <- c(nrow(socsec[socsec$n_clean_key>1]),med.age.unmatched.socsec.nonuniq, iqr.age.unmatched.socsec.nonuniq, med.aad.unmatched.nonuniq, iqr.aad.unmatched.nonuniq,NA, NA, NA)
  df[7,2:9] <- c(nrow(census.nonuniq.unmatched)+nrow(socsec[socsec$n_clean_key>1]),med.age.unmatched.all.uniq, iqr.age.unmatched.all.uniq, NA,NA,NA, NA, NA)
  
  ######### 5. SAVE #############
  
  diagnostics <- c(paste0("Raw number of people in census: ", n.census.raw, "\n"),
                   paste0("Raw number of people in socsec: ", n.socsec.raw, "\n"),
                   paste0("Number of people born after 1940 in socsec: ", n.socsec.post.1940, "\n"),
                   paste0("Number of people with age info missing in socsec: ", n.socsec.age.missing, "\n"),
                   paste0("Number of people with age info missing in census: ", n.census.age.missing, "\n"),
                   paste0("Number of people with name info missing in census: ", n.census.names.missing, "\n"),
                   paste0("Number in census after cleaning: ", n.census, "\n"),
                   paste0("Number in census after cleaning: ", n.socsec, "\n"),
                   paste0("Number of unique keys in census: ", n.census.uniq, "\n"),
                   paste0("Number of unique keys in socsec: ", n.socsec.uniq, "\n"),
                   paste0("Number of unique keys in census for ", sex.to.keep, ": ", n.census.sex.uniq, "\n"),
                   paste0("Number of matches: ", n.censoc, "\n")
  )
  
  cat("Saving counts of matched and unmatched datasets.\n")
  fileConn<-file(counts.file.name)
  writeLines(c(capture.output(cat(diagnostics))), fileConn)
  close(fileConn)
  
  cat("Saving descriptives of matched and unmatched datasets.\n")
  write.csv(df, file = descriptives.file.name, row.names = F)

  if(return.unmatched){
    to.return <- list(censoc = out, census = census, socsec = socsec)
  }
  else{
    to.return <- list(censoc = censoc)
  }
  return(to.return)
}





########### Helpers -----------------------------------------------------------------


get.first.word <- function(x)   
{
  x.split <- strsplit(x, split = " ")
  x1.list <- lapply(x.split,`[`, 1) # returns NA if no name
  x1 <- unlist(x1.list)
  return(x1)
}

## function to clean keys
clean.key <- function(tmp.key){
  return(gsub(" +|[[:punct:]]", "", tmp.key))
}


