## Let's see if the mortality differential between rich and poor declines within age for a cohort
## and if we can make that decline shrink by looking within an educational group (e.g. controlling for educ)

library(data.table)
library(memisc)
dt <- fread("ca_max_out_matched.csv")
## the "max" data set has all census vars

## define education
## now recode educ
dt[, "grade" := general_gradecompleted] # save typing
sort( table(dt$grade), decre = T)
dt [, educ := NULL]
dt[, educ := -1]
dt[grade == "High School, 1st year", educ := 9]
dt[grade == "High School, 2nd year", educ := 10]
dt[grade == "High School, 3rd year", educ := 11]
dt[grade == "High School, 4th year", educ := 12]
dt[grade == "College, 1st year", educ := 13]
dt[grade == "College, 2nd year", educ := 14]
dt[grade == "College, 3rd year", educ := 15]
dt[grade == "College, 4th year", educ := 16]
dt[grade == "College, 5th or subsequent year", educ := 17]
dt[grade == "Elementary school, 1st grade", educ := 1]
dt[grade == "Elementary school, 2nd grade", educ := 2]
dt[grade == "Elementary school, 3rd grade", educ := 3]
dt[grade == "Elementary school, 4th grade", educ := 4]
dt[grade == "Elementary school, 5th grade", educ := 5]
dt[grade == "Elementary school, 6th grade", educ := 6]
dt[grade == "Elementary school, 7th grade", educ := 7]
dt[grade == "Elementary school, 8th grade", educ := 8]
dt[grade == "None", educ := 0]
dt[grade == "College", educ := 16]
dt[grade == "High School", educ := 12]
dt[educ == -1, educ := NA]
table(dt$educ)
sum(is.na(dt$educ))
## [1] 10309

prop.table(table(is.na(dt$educ)))
##      FALSE       TRUE 
## 0.98228454 0.01771546 
## very few missing.

### other interesting variables
### race, renter, occup, farm
prop.table(table(dt$self_empty_info_race == "White"))
##      FALSE       TRUE 
## 0.02576638 0.97423362 
## basically all White

## renter
dt [, ownrent := NULL]
dt[general_homeownrent == "Owned", ownrent := "own"]
dt[general_homeownrent == "Rented", ownrent := "rent"]
dt[general_homeownrent == "", ownrent := "blank"]
table(dt$ownrent, useNA = "always")
##  blank    own   rent   <NA> 
## 262960 140843 177664    454 
#### ok, we don't know what blank is but very few NA.

### maybe only Heads?

dt [, head := NULL]
dt[self_empty_info_relationtohead == "Head", head := TRUE]
dt[self_empty_info_relationtohead != "Head", head := FALSE]


dt[head == TRUE, table(ownrent, useNA = "always")]
##  blank    own   rent   <NA> 
##   4229 140166 175612    167 
## ok, few Heads are blank.

dt[grade == "High School, 1st year", educ := 9]
dt[grade == "High School, 2nd year", educ := 10]
dt[grade == "High School, 3rd year", educ := 11]
dt[grade == "High School, 4th year", educ := 12]

dt[grade == "College, 1st year", educ := 13]
dt[grade == "College, 2nd year", educ := 14]
dt[grade == "College, 3rd year", educ := 15]
dt[grade == "College, 4th year", educ := 16]
dt[grade == "College, 5th or subsequent year", educ := 17]
dt[grade == "Elementary school, 1st grade", educ := 1]
dt[grade == "Elementary school, 2nd grade", educ := 2]
dt[grade == "Elementary school, 3rd grade", educ := 3]
dt[grade == "Elementary school, 4th grade", educ := 4]
dt[grade == "Elementary school, 5th grade", educ := 5]
dt[grade == "Elementary school, 6th grade", educ := 6]
dt[grade == "Elementary school, 7th grade", educ := 7]
dt[grade == "Elementary school, 8th grade", educ := 8]
dt[grade == "None", educ := 0]
dt[grade == "College", educ := 16]
dt[grade == "High School", educ := 12]
dt[educ == -1, educ := NA]
table(dt$educ)
sum(is.na(dt$educ))



small <- dt[byear %in% 1907:1912 & dyear <= 2005 & dyear >= 1975,
            .(hhid, recno, fname, lname, age, census_age, sex,
              income, ssn, dod, dob, byear, dyear, census_age, educ,
              head, ownrent)]


## let's just look at people born in 1910 (ok, we have 15,687)
## we'll assume we can do reverse survival from 2005

## let's look at disn of age of death
small[, death_age := dyear - byear]
library(txtplot)
txtdensity(small$death_age)
##      +------------+--------------+--------------+--------------+
## 0.04 +                   *****************                     +
##      |               *****                ***                  |
##      |           *****                       ***               |
## 0.03 +         ***                             **              +
##      |        **                                ***            |
##      |       **                                   **           |
## 0.02 +      **                                     **          +
##      |     **                                        *         |
##      |     *                                         **        |
## 0.01 +    *                                           **       +
##      |   **                                            **      |
##      |  **                                              ***    |
##      |                                                    ***  |
##    0 +------------+--------------+--------------+--------------+
##                  70             80             90               

## now let's compute hazards of death using reverse survival

dx <- table(small$death_age)
lx <- rev(cumsum(rev(dx))) ## alive at beginning of the interval

qx <- dx/lx
mx <- -log(1-qx)
x <- as.numeric(names(qx))
txtplot(x, qx)

## ok now let's stratify by income above or below median and educ above or below median

small[, rich := income > median(income, na.rm = T)]
## now recode educ
small[, "hi.educ" := educ >= median(educ, na.rm = T)]


small[, table(hi.educ, rich)]
##        rich
## hi.educ FALSE TRUE
##   FALSE  4409 2679
##   TRUE   3131 4718


## great: so now we have 4 groups of a few thousand each

## we can now do the reverse survival estimate of mx on each of these


get.mx <- function(dx.obs)   {
x.obs <- names(dx.obs)
x <- 65:95
dx <- rep(0, length(x))
names(dx) <- x
dx[names(dx.obs)] <- dx.obs
lx <- rev(cumsum(rev(dx))) ## alive at beginning of the interval
qx <- dx/lx
mx <- -log(1-qx)
# x <- as.numeric(names(qx))
print(mx[paste(x)])
return(mx[paste(x)])
}

x <- 65:95
dx.rich.hi.obs <- small[rich & hi.educ, table(death_age)]
mx.rich.hi <- get.mx(dx.obs = dx.rich.hi.obs)
dx.rich.lo.obs <- small[rich & !hi.educ, table(death_age)]
mx.rich.lo <- get.mx(dx.obs = dx.rich.lo.obs)
dx.poor.hi.obs <- small[!rich & hi.educ, table(death_age)]
mx.poor.hi <- get.mx(dx.obs = dx.poor.hi.obs)
dx.poor.lo.obs <- small[!rich & !hi.educ, table(death_age)]
mx.poor.lo <- get.mx(dx.obs = dx.poor.lo.obs)

cbind(mx.poor.lo, mx.poor.lo, mx.poor.hi, mx.rich.hi)

dx.rich.obs <- small[rich == T , table(death_age)]
dx.poor.obs <- small[rich == F, table(death_age)]
mx.rich <- get.mx(dx.rich.obs)
mx.poor <- get.mx(dx.poor.obs)
txtplot(65:95, mx.poor/mx.rich)
## not obviously increasing with age. 

## but if it were we could look at
txtplot(65:95, mx.poor.lo/mx.rich.lo) ## to see if increasing with age less.

risk.ratio.poor.rich.lo <- mx.poor.lo/mx.rich.lo
x <- 65:95
m.poor.rich.lo <- lm(risk.ratio.poor.rich.lo ~ x)
risk.ratio.poor.rich.hi <- mx.poor.hi/mx.rich.hi
m.poor.rich.hi <- lm(risk.ratio.poor.rich.hi ~ x)
risk.ratio.poor.rich <- mx.poor/mx.rich
x <- 65:95
m.poor.rich <- lm(risk.ratio.poor.rich ~ x)
mtable(m.poor.rich, m.poor.rich.lo, m.poor.rich.hi, summary.stats = c("R-squared", "N"))

##                m.poor.rich  m.poor.rich.lo  
## --------------------------------------------
##   (Intercept)    1.294***      1.028***     
##                 (0.210)       (0.258)       
##   x             -0.003         0.001        
##                 (0.003)       (0.003)       
## --------------------------------------------
##   R-squared        0.0           0.0        
##   N               30            30          
## ============================================

## no weights


## now with larger sample, much clearer effect but it doesn't go waway when we look within lo ed
============================================
               m.poor.rich  m.poor.rich.lo  
--------------------------------------------
  (Intercept)    1.817***      1.605***     
                (0.090)       (0.126)       
  x             -0.010***     -0.007***     
                (0.001)       (0.002)       
--------------------------------------------
  R-squared        0.7           0.4        
  N               31            31          
============================================

============================================================
               m.poor.rich  m.poor.rich.lo  m.poor.rich.hi  
------------------------------------------------------------
  (Intercept)    1.817***      1.605***        1.692***     
                (0.090)       (0.126)         (0.118)       
  x             -0.010***     -0.007***       -0.008***     
                (0.001)       (0.002)         (0.001)       
------------------------------------------------------------
  R-squared        0.7           0.4             0.5        
  N               31            31              31          
============================================================


txtplot(x, risk.ratio.poor.rich)
txtplot(x, risk.ratio.poor.rich.hi)
txtplot(x, risk.ratio.poor.rich.lo)


### now try with poisson regression

## we use the form glm(deaths ~ x, offset = log(exposure), family = poisson)

get.mx.plus <- function(dx.obs)   {
x.obs <- names(dx.obs)
x <- 65:95
dx <- rep(0, length(x))
names(dx) <- x
dx[names(dx.obs)] <- dx.obs
lx <- rev(cumsum(rev(dx))) ## alive at beginning of the interval
qx <- dx/lx
mx <- -log(1-qx)
# x <- as.numeric(names(qx))
out <- list("mx" = mx[paste(x)],
            "dx" = dx[paste(x)],
            "lx" = lx[paste(x)])
return(out)
}

out.rich <- get.mx.plus(dx.rich.obs)
out.poor <- get.mx.plus(dx.poor.obs)

age <- as.numeric(names(out.rich$lx))
rich.df <- as.data.frame(out.rich)
rich.df$rich = T
rich.df$age <- age
poor.df <- as.data.frame(out.poor)
poor.df$rich = F
poor.df$age <- age
out.df <- rbind(rich.df, poor.df)

m.all <- glm(dx ~ age, offset = log(lx), family = poisson, data = out.df)
m.richpoor <- glm(dx ~ age + rich + age:rich, offset = log(lx), family = poisson, data = out.df)
library(memisc)
mtable(m.all, m.richpoor, summary.stats = c("N"))
======================================
                 m.all    m.richpoor  
--------------------------------------
  (Intercept)  -9.738***  -9.417***   
               (0.039)    (0.054)     
  age           0.091***   0.087***   
               (0.000)    (0.001)     
  rich                    -0.654***   
                          (0.078)     
  age x rich               0.007***   
                          (0.001)     
--------------------------------------
  N               62         62       
======================================
## this says that rich have exp(-.654) = .52 as high mortality, at age zero
## but by age 65 this is exp(-.654 + 65 * .007) = .82
## and by age 90 exp(-.654 + 90 * .007) =  0.9762857, near parity.

## let's plot the hazards and see if this seems right
par(mfrow = c(2,2))
plot(age, out.rich$mx)
lines(age, out.poor$mx)
plot(age, out.rich$mx, log = "y")
lines(age, out.poor$mx)
plot(age, out.rich$mx/out.poor$mx)
abline(h = seq(0, 2, .1), col = "grey")
abline(h = 1, col = "black")

## now include education ...

## make dataframe

## run models
