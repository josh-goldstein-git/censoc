get.match.descriptives <- function(censoc, 
                                   socsec,
                                   census.uniq.unmatched,
                                   census.nonuniq.unmatched,
                                   socsec.uniq.unmatched,
                                   covariates = c("income","race", "renter", "rural"),
                                   condition.age = 25){
  
  ## create a vector for each type of data set (to cbind at the end)
  des.matched <- c()
  des.unmatched.census.uniq <- c()
  des.unmatched.socsec.uniq <- c()
  des.unmatched.all.uniq <- c()
  des.unmatched.census.nonuniq <- c()
  des.unmatched.socsec.nonuniq <- c()
  des.unmatched.all.nonuniq <- c()
  
  des.rownames <- c("median age", "IQR age", "median AAD", "IQR AAD")

  des.list <- list(des.matched, des.unmatched.census.uniq, des.unmatched.socsec.uniq, des.unmatched.all.uniq, 
                   des.unmatched.census.nonuniq, des.unmatched.socsec.nonuniq, des.unmatched.all.nonuniq)
  
  df.combinations <- c("censoc", "census.uniq.unmatched", "socsec.uniq.unmatched", 
                       'c(census.uniq.unmatched[,"census_age"],socsec.uniq.unmatched[,"census_age"])', 
                       "census.nonuniq.unmatched", "socsec[socsec$n_clean_key>1]", 
                       'c(census.nonuniq.unmatched[,"census_age"],socsec[socsec$n_clean_key>1,"census_age"])')
  
  ## rename age in censoc
  censoc[, census_age := census_age.x]
  ## create age at death variable
  censoc[, age.at.death := dyear - byear]
  socsec[, age.at.death := dyear - byear]
  socsec.uniq.unmatched[, age.at.death := dyear - byear]
    
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
  
  # med.age.matched <- median(censoc$census_age.x)
  # iqr.age.matched <- quantile(censoc$census_age.x, 0.75) - quantile(censoc$census_age.x, 0.25)
  # des.matched <- rbind(des.matched, med.age.matched, iqr.age.matched)
  # 
  # med.age.unmatched.census.uniq <- median(census.uniq.unmatched$census_age)
  # iqr.age.unmatched.census.uniq <- quantile(census.uniq.unmatched$census_age, 0.75) - quantile(census.uniq.unmatched$census_age, 0.25)
  # des.unmatched.census.uniq <- rbind(des.unmatched.census.uniq, med.age.unmatched.census.uniq, iqr.age.unmatched.census.uniq)
  # 
  # med.age.unmatched.socsec.uniq <- median(socsec.uniq.unmatched$census_age)
  # iqr.age.unmatched.socsec.uniq <- quantile(socsec.uniq.unmatched$census_age, 0.75) - quantile(socsec.uniq.unmatched$census_age, 0.25)
  # des.unmatched.socsec.uniq <- rbind(des.unmatched.socsec.uniq, med.age.unmatched.socsec.uniq, iqr.age.unmatched.socsec.uniq)
  # 
  # med.age.unmatched.all.uniq <- median(c(census.uniq.unmatched$census_age, socsec.uniq.unmatched$census_age))
  # iqr.age.unmatched.all.uniq <- quantile(c(census.uniq.unmatched$census_age, socsec.uniq.unmatched$census_age), 0.75) - quantile(c(census.uniq.unmatched$census_age, socsec.uniq.unmatched$census_age), 0.25)
  # des.unmatched.all.uniq <- rbind(des.unmatched.all.uniq, med.age.unmatched.all.uniq, iqr.age.unmatched.all.uniq)
  # 
  # med.age.unmatched.census.nonuniq <- median(census.nonuniq.unmatched$census_age)
  # iqr.age.unmatched.census.nonuniq <- quantile(census.nonuniq.unmatched$census_age, 0.75) - quantile(census.nonuniq.unmatched$census_age, 0.25)
  # des.unmatched.census.nonuniq <- rbind(des.unmatched.census.nonuniq, med.age.unmatched.census.nonuniq, iqr.age.unmatched.census.nonuniq)
  # 
  # med.age.unmatched.socsec.nonuniq <- median(socsec$census_age[socsec$n_clean_key>1])
  # iqr.age.unmatched.socsec.nonuniq <- quantile(socsec$census_age[socsec$n_clean_key>1], 0.75) - quantile(socsec$census_age[socsec$n_clean_key>1], 0.25)
  # des.unmatched.socsec.nonuniq <- rbind(des.unmatched.socsec.nonuniq, med.age.unmatched.socsec.nonuniq, iqr.age.unmatched.socsec.nonuniq)
  # 
  # med.age.unmatched.all.nonuniq <- median(c(census.nonuniq.unmatched$census_age,
  #                                           socsec$census_age[socsec$n_clean_key>1]))
  # iqr.age.unmatched.all.nonuniq <- quantile(c(census.nonuniq.unmatched$census_age,
  #                                             socsec$census_age[socsec$n_clean_key>1]), 0.75) - quantile(c(census.nonuniq.unmatched$census_age,
  #                                                                                                          socsec$census_age[socsec$n_clean_key>1]), 0.25)
  # des.unmatched.all.nonuniq <- rbind(des.unmatched.all.nonuniq, med.age.unmatched.all.uniq, iqr.age.unmatched.all.nonuniq)
  # 
  ## age at death
  # censoc[, age.at.death := dyear - byear]
  # socsec[, age.at.death := dyear - byear]
  # socsec.uniq.unmatched[, age.at.death := dyear - byear]
  # 
  # med.aad.matched <- median(censoc$age.at.death)
  # iqr.aad.matched <- quantile(censoc$age.at.death, 0.75) - quantile(censoc$age.at.death, 0.25)
  # des.matched <- rbind(des.matched, med.aad.matched, iqr.aad.matched)
  # 
  # med.aad.unmatched.uniq <- median(socsec.uniq.unmatched$age.at.death)
  # iqr.aad.unmatched.uniq <- quantile(socsec.uniq.unmatched$age.at.death, 0.75) - quantile(socsec.uniq.unmatched$age.at.death, 0.25)
  # des.unmatched.socsec.uniq <- rbind(des.unmatched.socsec.uniq, med.aad.unmatched.uniq, iqr.aad.unmatched.uniq)
  # 
  # med.aad.unmatched.nonuniq <- median(socsec$age.at.death[socsec$n_clean_key>1])
  # iqr.aad.unmatched.nonuniq <- quantile(socsec$age.at.death[socsec$n_clean_key>1], 0.75) - quantile(socsec$age.at.death[socsec$n_clean_key>1], 0.25)
  # des.unmatched.socsec.nonuniq <- rbind(des.unmatched.socsec.nonuniq, med.aad.unmatched.nonuniq, iqr.aad.unmatched.nonuniq)
  # 
  # des.unmatched.census.uniq <- rbind(des.unmatched.census.uniq, NA, NA)
  # des.unmatched.census.nonuniq <- rbind(des.unmatched.census.nonuniq, NA, NA)
  # des.unmatched.all.uniq <- rbind(des.unmatched.all.uniq, NA, NA)
  # des.unmatched.all.nonuniq <- rbind(des.unmatched.all.nonuniq, NA, NA)
  
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
    des.rownames <- c(des.rownames, "Prop white", "prop black", "prop other", "prop race missing")
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
  
  ## Put everything in a dataframe
  
  des.df <- round(as.data.frame(do.call(cbind, des.list)), 3)
  
  rownames(des.df) <- des.rownames
  colnames(des.df) <- c("Matched", 
                        "Unmatched unique census", "Unmatched unique socsec", "Unmatched unique all", 
                        "Unmatched non-unique census", "Unmatched non-unique socsec", "Unmatched non-unique all")
  des.df$conditionage = condition.age
  return(des.df)

}
