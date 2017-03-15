get.match.descriptives <- function(censoc, 
                                   socsec = NULL,
                                   census.uniq.unmatched,
                                   census.nonuniq.unmatched,
                                   socsec.uniq.unmatched = NULL,
                                   covariates = c("income","race", "renter", "rural", "ssn", "hh_head"),
                                   census.only = FALSE,
                                   condition.age = 25){
  
  ## create a vector for each type of data set (to cbind at the end)
  des.matched <- c()
  des.unmatched.census.uniq <- c()
  des.unmatched.census.nonuniq <- c()
  des.rownames <- c("median age", "IQR age", "median AAD", "IQR AAD")
  
  if(census.only==FALSE){
    des.unmatched.socsec.uniq <- c()
    des.unmatched.all.uniq <- c()
    des.unmatched.socsec.nonuniq <- c()
    des.unmatched.all.nonuniq <- c()
    
    des.list <- list(des.matched, des.unmatched.census.uniq, des.unmatched.socsec.uniq, des.unmatched.all.uniq, 
                     des.unmatched.census.nonuniq, des.unmatched.socsec.nonuniq, des.unmatched.all.nonuniq)
    
    df.combinations <- c("censoc", "census.uniq.unmatched", "socsec.uniq.unmatched", 
                         'c(census.uniq.unmatched[,"census_age"],socsec.uniq.unmatched[,"census_age"])', 
                         "census.nonuniq.unmatched", "socsec[socsec$n_clean_key>1]", 
                         'c(census.nonuniq.unmatched[,"census_age"],socsec[socsec$n_clean_key>1,"census_age"])')
  }
  
  if(census.only==TRUE){
    des.list <- list(des.matched, des.unmatched.census.uniq, 
                     des.unmatched.census.nonuniq)
    
    df.combinations <- c("censoc", "census.uniq.unmatched", 
                         "census.nonuniq.unmatched")
  }

  ## rename age in censoc
  censoc[, census_age := census_age.x]
  ## create age at death variable
  censoc[, age.at.death := dyear - byear]
  if(census.only==FALSE){
    socsec[, age.at.death := dyear - byear]
    socsec.uniq.unmatched[, age.at.death := dyear - byear]
  }

  ##### AGE CHARACTERISTICS
  
  for(i in 1:length(df.combinations)){
    this.df <- eval(parse(text = df.combinations[i]))
    med.age <- median(this.df$census_age)
    iqr.age <- quantile(this.df$census_age, 0.75) - quantile(this.df$census_age, 0.25)
    if(!grepl("census", df.combinations[i])){
      med.aad <- median(this.df$age.at.death)
      iqr.aad <- quantile(this.df$age.at.death, 0.75) - quantile(this.df$age.at.death, 0.25)
    }
    else{
      med.aad <- NA
      iqr.aad <- NA
    }
    des.list[[i]] <- rbind(des.list[[i]], med.age, iqr.age, med.aad, iqr.aad)
  }
  
  ######### N OBSERVATIONS
  des.rownames <- c(des.rownames, "# obs")
  for(i in 1:length(df.combinations)){
    this.df <- eval(parse(text = df.combinations[i]))
    if(is.vector(this.df)){
      des.list[[i]] <- rbind(des.list[[i]], length(unlist(this.df)))
    }
    else{
      des.list[[i]] <- rbind(des.list[[i]], nrow(this.df))
    }
  }
  
  ######## INCOME
  
  if("income" %in% covariates){
    des.rownames <- c(des.rownames, "median income", "prop inc missing", "prop inc zero")
    for(i in 1:length(df.combinations)){
      if(!grepl("socsec", df.combinations[i])){
        this.df <- eval(parse(text = df.combinations[i]))
        med.income <- median(this.df$income[this.df$census_age==condition.age], na.rm=T)
        prop.income.missing <- sum(is.na(this.df$income[this.df$census_age==condition.age]))/nrow(this.df[this.df$census_age==condition.age,])
        prop.income.zero <- sum(this.df$income[this.df$census_age==condition.age]==0, na.rm=T)/nrow(this.df[this.df$census_age==condition.age,])
      }
      else{
        med.income <- NA
        prop.income.missing <- NA
        prop.income.zero <- NA
      }
      des.list[[i]] <- rbind(des.list[[i]], med.income, prop.income.missing, prop.income.zero)
    }
  }
  
  ######## RACE
  
  if("race" %in% covariates){
    des.rownames <- c(des.rownames, "prop white", "prop black", "prop other", "prop race missing")
    for(i in 1:length(df.combinations)){
      if(!grepl("socsec", df.combinations[i])){
        this.df <- eval(parse(text = df.combinations[i]))
        white <- sum(this.df$race[this.df$census_age==condition.age]=="White", na.rm = T)/nrow(this.df[this.df$census_age==condition.age])
        black <- sum(this.df$race[this.df$census_age==condition.age]=="Black", na.rm = T)/nrow(this.df[this.df$census_age==condition.age])
        race.missing <- sum(is.na(this.df$race[this.df$census_age==condition.age]))/nrow(this.df[this.df$census_age==condition.age])
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
  
  ######### OWNED/ RENTED
  
  if("renter" %in% covariates){
    des.rownames <- c(des.rownames, "prop own", "prop rent", "prop own/rent missing")
    for(i in 1:length(df.combinations)){
      if(!grepl("socsec", df.combinations[i])){
        this.df <- eval(parse(text = df.combinations[i]))
        own <- sum(this.df$own_rent[this.df$census_age==condition.age]=="Owned", na.rm = T)/nrow(this.df[this.df$census_age==condition.age])
        rent <- sum(this.df$own[this.df$census_age==condition.age]=="Rented", na.rm = T)/nrow(this.df[this.df$census_age==condition.age])
        or.missing <- sum(is.na(this.df$own_rent[this.df$census_age==condition.age]))/nrow(this.df[this.df$census_age==condition.age])
      }
      else{
        own <- NA
        rent <- NA
        or.missing <- NA
      }
      des.list[[i]] <- rbind(des.list[[i]], own, rent, or.missing)
    }
  }
  
  ######### RURAL
  
  if("rural" %in% covariates){
    des.rownames <- c(des.rownames, "prop rural", "prop urban", "prop rural/urban missing")
    for(i in 1:length(df.combinations)){
      if(!grepl("socsec", df.combinations[i])){
        this.df <- eval(parse(text = df.combinations[i]))
        rural <- sum(this.df$rural[this.df$census_age==condition.age]==TRUE, na.rm = T)/nrow(this.df[this.df$census_age==condition.age])
        urban <- sum(this.df$rural[this.df$census_age==condition.age]==FALSE, na.rm = T)/nrow(this.df[this.df$census_age==condition.age])
        ru.missing <- sum(is.na(this.df$rural[this.df$census_age==condition.age]))/nrow(this.df[this.df$census_age==condition.age])
      }
      else{
        rural <- NA
        urban <- NA
        ru.missing <- NA
      }
      des.list[[i]] <- rbind(des.list[[i]], rural, urban, ru.missing)
    }
  }
  
  ######### SSN
  
  if("ssn" %in% covariates){
    des.rownames <- c(des.rownames, "prop ssn yes", "prop ssn no", "prop ssn info missing")
    for(i in 1:length(df.combinations)){
      if(!grepl("socsec", df.combinations[i])){
        this.df <- eval(parse(text = df.combinations[i]))
        ssn.yes <- sum(this.df$ssn_census[this.df$census_age==condition.age]=="Yes")/nrow(this.df[this.df$census_age==condition.age])
        ssn.no <- sum(this.df$ssn_census[this.df$census_age==condition.age]=="No")/nrow(this.df[this.df$census_age==condition.age])
        ssn.info.missing <-sum(this.df$ssn_census[this.df$census_age==condition.age]=="")/nrow(this.df[this.df$census_age==condition.age])
      }
      else{
        ssn.yes <- NA
        ssn.no <- NA
        ssn.info.missing <- NA
      }
      des.list[[i]] <- rbind(des.list[[i]], ssn.yes, ssn.no, ssn.info.missing)
    }
  }
  
  ######### SSN
  
  if("hh_head" %in% covariates){
    des.rownames <- c(des.rownames, "prop head", "prop not head", "prop hh_head missing")
    for(i in 1:length(df.combinations)){
      if(!grepl("socsec", df.combinations[i])){
        this.df <- eval(parse(text = df.combinations[i]))
        head.yes <- sum(this.df$hh_head[this.df$census_age==condition.age]=="Yes")/nrow(this.df[this.df$census_age==condition.age])
        head.no <- sum(this.df$hh_head[this.df$census_age==condition.age]=="No")/nrow(this.df[this.df$census_age==condition.age])
        head.info.missing <-sum(this.df$hh_head[this.df$census_age==condition.age]=="")/nrow(this.df[this.df$census_age==condition.age])
      }
      else{
        head.yes <- NA
        head.no <- NA
        head.info.missing <- NA
      }
      des.list[[i]] <- rbind(des.list[[i]], head.yes, head.no, head.info.missing)
    }
  }
  
  ## Put everything in a dataframe
  
  des.df <- round(as.data.frame(do.call(cbind, des.list)), 3)
  
  rownames(des.df) <- des.rownames
  if(census.only==FALSE){
    colnames(des.df) <- c("Matched", 
                          "Unmatched unique census", "Unmatched unique socsec", "Unmatched unique all", 
                          "Unmatched non-unique census", "Unmatched non-unique socsec", "Unmatched non-unique all")
  }
  
  if(census.only==TRUE){
    colnames(des.df) <- c("Matched", 
                          "Unmatched unique census",  
                          "Unmatched non-unique census")
  }
  des.df$conditionage = condition.age
  des.df$variable = rownames(des.df)
  des.df <- des.df[,c((ncol(des.df)),(ncol(des.df)-1), 1:((ncol(des.df)-2)))]
  #remove condition age for unconditional
  des.df[des.df$variable %in% c("median age", "IQR age",
                                          "median AAD", "IQR AAD", "# obs"), "conditionage"] <- NA
  return(des.df)

}
