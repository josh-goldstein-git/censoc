## create a table of death counts by age and period from SSDM
## to compare to HMD


## read in all three

library(data.table)
library(readr)
##
## note: we read from different file
tt3 <- readr::read_fwf("/home/ipums/josh-ipums/progs/ssdm/ssdm3",
                fwf_widths(c(1,9, 20, 4, 15, 15, 1, 8, 8),
                           col_names = c("mode", "ssn", "lname",
                                         "name_suffix", "fname", "mname",
                                         "vorpcode", "dod", "dob")))
##
tt2 <- readr::read_fwf("/home/ipums/josh-ipums/progs/ssdm/ssdm2",
                fwf_widths(c(1,9, 20, 4, 15, 15, 1, 8, 8),
                           col_names = c("mode", "ssn", "lname",
                                         "name_suffix", "fname", "mname",
                                         "vorpcode", "dod", "dob")))
##
tt1 <- readr::read_fwf("/home/ipums/josh-ipums/progs/ssdm/ssdm1",
                fwf_widths(c(1,9, 20, 4, 15, 15, 1, 8, 8),
                           col_names = c("mode", "ssn", "lname",
                                         "name_suffix", "fname", "mname",
                                         "vorpcode", "dod", "dob")))

dt1 <- as.data.table(tt1)
dt2 <- as.data.table(tt2)
dt3 <- as.data.table(tt3)

dt <- rbindlist(list(dt1, dt2, dt3))


## now get birth year
dt[,"byear" := as.numeric(substr(dob, 5, 9))]
dt[,"dyear" := as.numeric(substr(dod, 5, 9))]
dt[,"bmonth" := as.numeric(substr(dob, 1, 2))]
dt[,"dmonth" := as.numeric(substr(dod, 1, 2))]
dt[,"bday" := as.numeric(substr(dob, 3, 4))]
dt[,"dday" := as.numeric(substr(dod, 3, 4))]

dt[byear == 0 ,"byear" := NA]

## compute exact age of death (we compare dates by doing month +
## day/10 -- this should preserve ordering)
dt[,"exact_age_death" := ifelse(dmonth + dday/10 >=  bmonth + bday/10,
                               "yes" = dyear - byear,
                               "no" = (dyear - 1) - byear) ] 
death.freq.tab <- dt[, table(exact_age_death, dyear, useNA = "always")]

save(death.freq.tab, file = "death_freq_tab.RData")

