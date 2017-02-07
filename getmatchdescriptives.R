get.match.descriptives <- function(censoc, 
                                   census.uniq.unmatched,
                                   census.nonuniq.unmatched,
                                   socsec.uniq.unmatched,
                                   covariates = c("income","race", "renter", "rural")){
  
  ## create a vector for each type of data set (to cbind at the end)
  des.matched <- c()
  des.unmatched.census.uniq <- c()
  des.unmatched.socsec.uniq <- c()
  des.unmatched.all.uniq <- c()
  des.unmatched.census.nonuniq <- c()
  des.unmatched.socsec.nonuniq <- c()
  des.unmatched.all.nonuniq <- c()
  
  des.rownames <- c("median age", "IQR age", "median AAD", "IQR AAD")
    
  ##### AGE CHARACTERISTICS
  # median age
  med.age.matched <- median(censoc$census_age.x)
  iqr.age.matched <- quantile(censoc$census_age.x, 0.75) - quantile(censoc$census_age.x, 0.25)
  des.matched <- rbind(des.matched, med.age.matched, iqr.age.matched)
  
  med.age.unmatched.census.uniq <- median(census.uniq.unmatched$census_age)
  iqr.age.unmatched.census.uniq <- quantile(census.uniq.unmatched$census_age, 0.75) - quantile(census.uniq.unmatched$census_age, 0.25)
  des.unmatched.census.uniq <- rbind(des.unmatched.census.uniq, med.age.unmatched.census.uniq, iqr.age.unmatched.census.uniq)
  
  med.age.unmatched.socsec.uniq <- median(socsec.uniq.unmatched$census_age)
  iqr.age.unmatched.socsec.uniq <- quantile(socsec.uniq.unmatched$census_age, 0.75) - quantile(socsec.uniq.unmatched$census_age, 0.25)
  des.unmatched.socsec.uniq <- rbind(des.unmatched.socsec.uniq, med.age.unmatched.socsec.uniq, iqr.age.unmatched.socsec.uniq)
  
  med.age.unmatched.all.uniq <- median(c(census.uniq.unmatched$census_age, socsec.uniq.unmatched$census_age))
  iqr.age.unmatched.all.uniq <- quantile(c(census.uniq.unmatched$census_age, socsec.uniq.unmatched$census_age), 0.75) - quantile(c(census.uniq.unmatched$census_age, socsec.uniq.unmatched$census_age), 0.25)
  des.unmatched.all.uniq <- rbind(des.unmatched.all.uniq, med.age.unmatched.all.uniq, iqr.age.unmatched.all.uniq)
  
  med.age.unmatched.census.nonuniq <- median(census.nonuniq.unmatched$census_age)
  iqr.age.unmatched.census.nonuniq <- quantile(census.nonuniq.unmatched$census_age, 0.75) - quantile(census$census_age[census$n_clean_key>1], 0.25)
  des.unmatched.census.nonuniq <- rbind(des.unmatched.census.nonuniq, med.age.unmatched.census.nonuniq, iqr.age.unmatched.census.nonuniq)
  
  med.age.unmatched.socsec.nonuniq <- median(socsec$census_age[socsec$n_clean_key>1])
  iqr.age.unmatched.socsec.nonuniq <- quantile(socsec$census_age[socsec$n_clean_key>1], 0.75) - quantile(socsec$census_age[socsec$n_clean_key>1], 0.25)
  des.unmatched.socsec.nonuniq <- rbind(des.unmatched.socsec.nonuniq, med.age.unmatched.socsec.nonuniq, iqr.age.unmatched.socsec.nonuniq)
  
  med.age.unmatched.all.nonuniq <- median(c(census.nonuniq.unmatched$census_age,
                                            socsec$census_age[socsec$n_clean_key>1]))
  iqr.age.unmatched.all.nonuniq <- quantile(c(census.nonuniq.unmatched$census_age,
                                              socsec$census_age[socsec$n_clean_key>1]), 0.75) - quantile(c(census.nonuniq.unmatched$census_age,
                                                                                                           socsec$census_age[socsec$n_clean_key>1]), 0.25)
  des.unmatched.all.nonuniq <- rbind(des.unmatched.all.nonuniq, med.age.unmatched.all.uniq, iqr.age.unmatched.all.nonuniq)
  
  ## age at death
  censoc[, age.at.death := dyear - byear]
  socsec[, age.at.death := dyear - byear]
  socsec.uniq.unmatched[, age.at.death := dyear - byear]
  
  med.aad.matched <- median(censoc$age.at.death)
  iqr.aad.matched <- quantile(censoc$age.at.death, 0.75) - quantile(censoc$age.at.death, 0.25)
  des.matched <- rbind(des.matched, med.aad.matched, iqr.aad.matched)
  
  med.aad.unmatched.uniq <- median(socsec.uniq.unmatched$age.at.death)
  iqr.aad.unmatched.uniq <- quantile(socsec.uniq.unmatched$age.at.death, 0.75) - quantile(socsec.uniq.unmatched$age.at.death, 0.25)
  des.unmatched.socsec.uniq <- rbind(des.unmatched.socsec.uniq, med.aad.unmatched.uniq, med.aad.unmatched.uniq)
  
  med.aad.unmatched.nonuniq <- median(socsec$age.at.death[socsec$n_clean_key>1])
  iqr.aad.unmatched.nonuniq <- quantile(socsec$age.at.death[socsec$n_clean_key>1], 0.75) - quantile(socsec$age.at.death[socsec$n_clean_key>1], 0.25)
  des.unmatched.socsec.nonuniq <- rbind(des.unmatched.socsec.nonuniq, med.aad.unmatched.nonuniq, med.aad.unmatched.nonuniq)
  
  des.unmatched.census.uniq <- rbind(des.unmatched.census.uniq, NA, NA)
  des.unmatched.census.nonuniq <- rbind(des.unmatched.census.nonuniq, NA, NA)
  des.unmatched.all.uniq <- rbind(des.unmatched.all.uniq, NA, NA)
  des.unmatched.all.nonuniq <- rbind(des.unmatched.all.nonuniq, NA, NA)
  
  ######## INCOME
  
  if("income" %in% covariates){
    
    des.rownames <- c(des.rownames, "median income", "med inc modal age", "prop inc missing", "prop inc zero")
    
    med.income.matched <- median(censoc$income, na.rm = T)
    prop.income.missing.matched <- sum(is.na(censoc$income))/nrow(censoc)
    prop.income.zero.matched <- sum(censoc$income==0, na.rm=T)/nrow(censoc)
    
    med.income.unmatched.uniq <- median(census.uniq.unmatched$income, na.rm=T)
    prop.income.missing.unmatched.uniq <- sum(is.na(census.uniq.unmatched$income))/nrow(census.uniq.unmatched)
    prop.income.zero.unmatched.uniq <- sum(census.uniq.unmatched$income==0, na.rm=T)/nrow(census.uniq.unmatched)
    
    med.income.unmatched.nonuniq <- median(census.nonuniq.unmatched$income, na.rm=T)
    prop.income.missing.unmatched.nonuniq <- sum(is.na(census.nonuniq.unmatched$income))/nrow(census.nonuniq.unmatched)
    prop.income.zero.unmatched.nonuniq <- sum(census.nonuniq.unmatched$income==0, na.rm=T)/nrow(census.nonuniq.unmatched)
    
    ## condition income calculates on mode age of matched dataset
    mode.age <- as.numeric(names(sort(table(censoc$census_age.x), decreasing = T)[1]))
    med.income.matched.mode <- median(censoc$income[censoc$census_age.x==mode.age], na.rm = T)
    med.income.unmatched.uniq.mode <- median(census.uniq.unmatched$income[census.uniq.unmatched$census_age==mode.age], na.rm=T)
    med.income.unmatched.nonuniq.mode <- median(census.nonuniq.unmatched$income[census.nonuniq.unmatched$census_age==mode.age], na.rm=T)
  
    des.matched <- rbind(des.matched, med.income.matched, med.income.matched.mode, prop.income.missing.matched, prop.income.zero.matched)
    des.unmatched.census.uniq <- rbind(des.unmatched.census.uniq, med.income.unmatched.uniq, med.income.unmatched.uniq.mode, prop.income.missing.unmatched.uniq, prop.income.zero.unmatched.uniq)
    des.unmatched.census.nonuniq <- rbind(des.unmatched.census.nonuniq, med.income.unmatched.nonuniq, med.income.unmatched.nonuniq.mode, prop.income.missing.unmatched.nonuniq, prop.income.zero.unmatched.nonuniq)
    des.unmatched.socsec.uniq <- rbind(des.unmatched.socsec.uniq, NA, NA, NA, NA)
    des.unmatched.all.uniq <- rbind(des.unmatched.all.uniq, NA, NA, NA, NA)
    des.unmatched.socsec.nonuniq <- rbind(des.unmatched.socsec.nonuniq, NA, NA, NA, NA)
    des.unmatched.all.nonuniq <- rbind(des.unmatched.all.nonuniq, NA, NA, NA, NA)
  }
  
  ######## RACE
  
  if("race" %in% covariates){
    des.list <- list(des.matched, des.unmatched.census.uniq, des.unmatched.socsec.uniq, des.unmatched.all.uniq, 
                     des.unmatched.census.nonuniq, des.unmatched.socsec.nonuniq, des.unmatched.all.nonuniq)
    
    des.rownames <- c(des.rownames, "Prop white", "Prop black", "Prop other", "Prop race missing")
    df.names <- c("censoc", "census.uniq.unmatched", "socsec.uniq.unmatched", "unmatched.uniq.all", 
                  "census.nonuniq.unmatched", "socsec.uniq.unmatched", "unmatched.nonuniq.all")
    
    for(i in 1:length(df.names)){
      if(!grepl("all|socsec", df.names[i])){
        this.df <- eval(parse(text = df.names[i]))
        white <- sum(this.df$race=="White", na.rm = T)/nrow(this.df)
        black <- sum(this.df$race=="Black", na.rm = T)/nrow(this.df)
        race.missing <- sum(is.na(this.df$race))/nrow(this.df)
        other.race <- 1- sum(white, black, race.missing)
      }
      else{
        white <- NA
        black <- NA
        race.missing <- NA
        other.race <- NA
      }
      des.list[[i]] <- rbind(des.list[[i]], white, black, race.missing, other.race)
    }
  }
  
  ## Put everything in a dataframe
  
  des.df <- round(as.data.frame(do.call(cbind, des.list)), 3)
  
  rownames(des.df) <- des.rownames
  colnames(des.df) <- c("Matched", 
                        "Unmatched unique census", "Unmatched unique socsec", "Unmatched unique all", 
                        "Unmatched non-unique census", "Unmatched non-unique socsec", "Unmatched non-unique all")
  
  return(des.df)

}
