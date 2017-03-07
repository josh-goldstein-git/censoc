create.censoc <- function(census.file = "/home/ipums/josh-ipums/mydata/my1940/CA_clean.txt",
                          names.file = "/home/ipums/monica-ipums/censoc/tmp.txt",
                          socsec.file.list = c("/home/ipums/josh-ipums/progs/ssdm/ssdm1", 
                                               "/home/ipums/josh-ipums/progs/ssdm/ssdm2",
                                               "/home/ipums/josh-ipums/progs/ssdm/ssdm3"),
                          ssn.min, ssn.max, # http://www.stevemorse.org/ssn/ssn.html
                          sex.to.keep = "Male",
                          counts.file.name = "counts.txt",
                          descriptives.file.name = "descriptives.csv",
                          matched.file.name = "matched.csv",
                          des.covs = c("income","race", "renter", "rural", "ssn", "hh_head"),
                          condition.ages = c(20, 25, 30, 35, 40),
                          return.unmatched = FALSE){
  
  ######### 1. READ IN DATA ######### 
  cat("Reading census data.\n")
  ## read in census
  census <- fread(census.file)
  names.census <- fread(names.file, nrows = 2)
  setnames(census, names(names.census))
  ## only keep variables of interest for now
  ## state, own/rent, name, household position, gender, race, age, schooling, citizenship, city/rural, income, ssn, HHID, HHORDER
  census <- census[,c(3,22,26,27,31,32,33,34,37,41,42, 56, 65, 84,85)]
  
  cat("Reading socsec data.\n")
  ## read in socsec
  socsec.list<- list()
  for(i in 1:length(socsec.file.list)){
    socsec.file <- socsec.file.list[i]
    tt <- readr::read_fwf(socsec.file,
                          fwf_widths(c(1,9, 20, 4, 15, 15, 1, 8, 8),
                                     col_names = c("mode", "ssn", "lname",
                                                   "name_suffix", "fname", "mname",
                                                   "vorpcode", "dod", "dob")))
    assign(paste0("socsec",i) , as.data.table(tt))
    socsec.list[[i]] <-eval(parse(text = paste0("socsec",i)))
    rm(tt)
  }
  
  socsec <- rbindlist(socsec.list)
  rm(socsec.list)
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
  socsec[,"dmonth" := as.numeric(substr(dod, 1, 2))]
  ## now get census_age (hopefully census was on April 1)
  socsec[,"census_age" := ifelse(bmonth < 4,
                                 1940 - byear,
                                 1939 - byear)]
  
  ## METRIC: number socsec born after 1940
  n.socsec.post.1940 <- nrow(socsec[which(socsec$census_age<0),])
  
  ## METRIC: number deaths before 1975
  n.socsec.pre.1975 <- nrow(socsec[which(socsec$dyear<1975),])
  
  ## METRIC: number socsec missing age info
  socsec.missing.age <- socsec[socsec$census_age==1940|socsec$census_age==1939,]
  n.socsec.age.missing <- nrow(socsec.missing.age)
  
  ## get rid of these
  socsec <- socsec[socsec$census_age!=1940&socsec$census_age!=1939,]
  socsec <- socsec[socsec$census_age>=0,]
  socsec <- socsec[socsec$dyear>=1975,]
  
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
  census[,"hh_position":=self_empty_info_relationtohead]
  ## covariates
  # income 
  census[,"income" := general_income,]
  census[,"income" := gsub(",", "", income)]
  census[,"income" := as.numeric(income)]
  # race recode
  census[,"race":=self_empty_info_race]
  census[race=="Negro",race:="Black"]
  census[!(race %in% c("Black", "White", "Chinese", "Japanese", "Filipino", "")), race:="Other"]
  census[race=="", race:=NA]
  # own/rent recode
  census[,"own_rent":=general_homeownrent]
  census[!(own_rent%in%c("Owned", "Rented", "")), own_rent:="Other"]
  census[own_rent=="", own_rent:=NA]
  # rural/urban recode
  census[,"rural":=NA]
  census[self_residence_place_city_multiple_1=="Rural",rural:=TRUE]
  census[!(self_residence_place_city_multiple_1 %in% c("Rural", "")), rural:=FALSE]
  # ssn recode
  census[,"ssn_census":=general_SSN]
  census[!(ssn_census %in% c("Yes", "No")),ssn.census:=""]
  # hh head recode
  census[,"hh_head":="No"]
  census[hh_position=="Head",hh_head:="Yes"]
  census[hh_position=="", hh_head:=""]
  ## HHID_NUMERIC is unique HHID for each household
  ## HHORDER is the order of the person in unique HH
  census[,"hhid" := HHID_NUMERIC]
  census[,"recno" := HHORDER]
  
  census <- census[,.(hhid, recno, fname, lname, age, census_age, sex, hh_head, race, own_rent, rural, ssn_census, income)]
  
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
  
  ## Drop if not required sex
  census.uniq.sex <- census.uniq[census.uniq$sex == sex.to.keep,]
  # METRIC
  n.census.sex.uniq <- nrow(census.uniq.sex)
  ## Reset keys
  setkey(socsec.uniq, clean_key)
  setkey(census.uniq.sex, clean_key)
  
  ## Merge on unique keys
  #out <- socsec.uniq[census.uniq.male]
  out <- merge(census.uniq.sex, socsec.uniq, on = clean_key)
  # METRIC
  n.censoc <- nrow(out)
  censoc <- out
  rm(out)
  mode.age <- as.numeric(names(sort(table(censoc$census_age.x), decreasing = T)[1]))
  
  ############ 4. DESCRIPTIVES #############
  
  census.uniq.unmatched <- census.uniq.sex[!(census.uniq.sex$clean_key %in% censoc$clean_key),]
  socsec.uniq.unmatched <- socsec.uniq[!(socsec.uniq$clean_key %in% censoc$clean_key),]
  census.nonuniq.unmatched <- census[census$n_clean_key>1&census$sex==sex.to.keep,]

  ## summaries for matched, non-matched unique, non-matched non-unique
  df.all.ages <- c()
  for(i in 1:length(condition.ages)){
    df <- get.match.descriptives(censoc, socsec, 
                                 census.uniq.unmatched, census.nonuniq.unmatched, 
                                 socsec.uniq.unmatched, des.covs, condition.age = condition.ages[i])
    df.all.ages <- rbind(df.all.ages, df)
  }
  
  # remove duplicates
  df.all.ages <- df.all.ages[!duplicated(df.all.ages), ]

  
  ######### 5. SAVE #############
  
  match.rates.ages <- c()
  for(i in 1:length(condition.ages)){
    match.rates.ages <- c(match.rates.ages, paste0("Match rate at age ", 
                                                   condition.ages[i],": ", 
                                                   round(nrow(censoc[censoc$census_age.x==condition.ages[i]])/nrow(census.uniq.sex[census.uniq.sex$census_age==condition.ages[i]]), 3), "\n"))
  }
  
  diagnostics <- c(paste0("Raw number of people in census: ", n.census.raw, "\n"),
                   paste0("Raw number of people in socsec: ", n.socsec.raw, "\n"),
                   paste0("Number of people born after 1940 in socsec: ", 
                          n.socsec.post.1940, " (proportion:", round(n.socsec.post.1940/n.socsec.raw, 3), ")", "\n"),
                   paste0("Number of people with age info missing in socsec: ", 
                          n.socsec.age.missing, " (proportion:", round(n.socsec.age.missing/n.socsec.raw, 3), ")","\n"),
                   paste0("Number of deaths before 1975 in socsec: ", 
                          n.socsec.pre.1975, " (proportion:", round(n.socsec.pre.1975/n.socsec.raw, 3), ")","\n"),
                   paste0("Number of people with age info missing in census: ", 
                          n.census.age.missing, " (proportion:", round(n.census.age.missing/n.census.raw, 3), ")", "\n"),
                   paste0("Number of people with name info missing in census: ", 
                          n.census.names.missing, " (proportion:", round(n.census.names.missing/n.census.raw, 3), ")","\n"),
                   paste0("Number in census after cleaning: ", 
                          n.census, " (proportion:", round(n.census/n.census.raw, 3), ")", "\n"),
                   paste0("Number in census after cleaning: ", 
                          n.socsec, " (proportion:", round(n.socsec/n.socsec.raw, 3), ")","\n"),
                   paste0("Number of unique keys in census: ", 
                          n.census.uniq, " (proportion:", round(n.census.uniq/n.census, 3), ")","\n"),
                   paste0("Number of unique keys in socsec: ", 
                          n.socsec.uniq, " (proportion:", round(n.socsec.uniq/n.socsec, 3), ")","\n"),
                   paste0("Number of unique keys in census for ", 
                          sex.to.keep, ": ", n.census.sex.uniq, "\n"),
                   paste0("Number of matches: ", n.censoc, "\n"),
                   paste0("Match rate: ", round(n.censoc/n.census.sex.uniq, 3), "\n"),
                   paste0("Modal age of matched people: ", mode.age, "\n"),
                   match.rates.ages
  )
  
  cat("Saving counts of matched and unmatched datasets.\n")
  fileConn<-file(counts.file.name)
  writeLines(c(capture.output(cat(diagnostics))), fileConn)
  close(fileConn)
  
  cat("Saving descriptives of matched and unmatched datasets.\n")
  write.csv(df.all.ages, file = descriptives.file.name, row.names=F)
  
  ## just want to keep unique identifier, byear, dyear, bmonth, dmonth
  censoc[,"id":=as.numeric(paste0(hhid, recno))]
  censoc <- censoc[,.(id, census_age.x,byear, bmonth, dyear, dmonth)]
  
  cat("Saving matched dataset (ID and birth/death info). \n")
  write.csv(censoc, file = matched.file.name, row.names = F)
  
  if(return.unmatched){
    to.return <- list(censoc = censoc, census = census, socsec = socsec)
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


