## Get data
# ZA5900: International Social Survey Programme: Family and Changing Gender Roles IV - ISSP 2012

## Install the following packages
# haven
# tidyverse
# forcats
# tableone
# psych

# Set-up
setwd("C:/Users/Joanna/Dropbox/Cohen/Gender Inequality Index")

## Load the libraries
library(haven)
library(tidyverse)

## Import the data
issp12 <- read_dta("C:/Users/Joanna/Dropbox/Data/ISSP/ISSP_2012_Families.dta")

### Look at the data
head(issp12, n = 30)


# Transform the variables

## Select the Variables
data <- select(issp12, MARITAL, PARTLIV,  V41, V3, SEX, AGE, V67, HHCHILDR, HHTODD, DEGREE, MAINSTAT, WRKHRS,
                       V59, V60, V61, V62, V63, V50, V42, V43, V44, V45, V46, V47, V57)

## Rename variables
data <- rename(data, pool	     =	V41,	nation	  =	V3,	  duration =	V67,
                     respmom	 =	V59,	rwork1	  =	V60,	spwork1	 =	V62,
                     rwork2	   =	V61,	spwork2	  =	V63,	relinc	 =	V50,
                     laundry	 =	V42,	repairs	  =	V43,	carewk	 =	V44,
                     groceries =	V45,	cleaning	=	V46,	meals	   =	V47,
                     famlife	 =	V57)
## Clean the variables

# Country
data$nation <- as_factor(data$nation)
data <- data %>% 
  separate(nation, c("Code", "Country"), extra = "merge", fill = "left")

data$Country[data$Country == "W-Germany-West"]    <- "Germany"
data$Country[data$Country == "E-Germany-East"]    <- "Germany"
data$Country[data$Country == "GBN-Great Britain"] <- "Great Britain"

data$Code    <- as_factor(data$Code)
data$Country <- as_factor(data$Country)

# Marital status
data <- data %>%
  mutate(
    marst = case_when(
      MARITAL   == 1 |  MARITAL == 2                                    ~ "Married",
      PARTLIV   == 1 & (MARITAL >= 3 & MARITAL <=6)                     ~ "Cohab",
      PARTLIV   != 1 & (MARITAL >= 3 & MARITAL <=6)                     ~ "Single",
      TRUE                                                              ~  NA_character_
    ))
data$marst <- as_factor(data$marst)

# Income allocation
data <- data %>%
  mutate(
    pool = case_when(
      pool      == 1 | pool == 2                        ~ "One $ Manager",
      pool      == 3                                    ~ "Manage $ Together",
      pool      == 4                                    ~ "Keep Some $ Separate",
      pool      == 5                                    ~ "Keep All $ Separate",
      TRUE                                              ~  NA_character_
    ))

data$pool <- factor(data$pool, levels = c("One $ Manager", "Manage $ Together", "Keep Some $ Separate", "Keep All $ Separate"))

# Sex
data$SEX <- as_factor(data$SEX)
levels(data$SEX)[levels(data$SEX)=='No answer'] <- NA

# Earnings Homogamy
data <- data %>%
  mutate(
    relinc = case_when(
      ((relinc >= 1 & relinc <= 3) & SEX == "Male")    | 
      ((relinc >= 5 & relinc <= 7) & SEX == "Female")         ~ "Man primary-earner",
      ((relinc >= 1 & relinc <= 3) & SEX == "Female")  | 
      ((relinc >= 5 & relinc <= 7) & SEX == "Male")           ~ "Woman primary-earner",      
      relinc ==4                                              ~ "Equal-earners",
      TRUE                                                    ~  NA_character_
    ))
data$relinc <- as_factor(data$relinc)

# Age
data$AGE <- zap_labels(data$AGE)
data$AGE[data$AGE == 999] <- NA

# Duration
# duration only asked of married for US; none for Philippines
data$duration <- zap_labels(data$duration)
data <- data %>%
  mutate(
    duration = case_when(
      duration      == 0 | duration == 1  | duration == 2   ~ "< 3 years",
      duration      >= 3 & duration <= 92                   ~ "3 or more years",
      TRUE                                                  ~  NA_character_
    ))
data <- select(data, -duration)

# Parent
data <- data %>%
  mutate(
    parent = case_when(
       HHCHILDR   == 0 & HHTODD == 0                                      ~ "Nonparent",
      (HHCHILDR  >= 1 & HHCHILDR <= 21)  | (HHTODD >= 1 & HHTODD <= 10)   ~ "Parent",
      TRUE                                                                ~  NA_character_
    ))
data$parent <- as_factor(data$parent)
data <- within(data, parent <- relevel(parent, ref = "Nonparent"))

# Education
data$DEGREE <- as_factor(data$DEGREE)
levels(data$DEGREE)[levels(data$DEGREE)=="No answer, CH: don't know"] <- NA
levels(data$DEGREE) <- c('No formal education', 'Primary school', 'Lower secondary', 'Upper secondary', 'Post secondary', 'Lower level tertiary', 'Upper level tertiary')

# Employment
data$WRKHRS   <- zap_labels(data$WRKHRS)
data$WRKHRS[data$WRKHRS >= 97] <- NA

data <- data %>%
  mutate(
    employ = case_when(
      MAINSTAT   == 1 & (WRKHRS>=35 	& WRKHRS<=96)     ~ "Fulltime",
      MAINSTAT   == 1 & (WRKHRS>=1	  & WRKHRS<=34)     ~ "Parttime",
      MAINSTAT   == 2                                   ~ "Unemployed",
      MAINSTAT   == 3 | MAINSTAT==4                     ~ "Student",
      MAINSTAT   >= 5 & MAINSTAT<=9                     ~ "Not in labor force",
      TRUE                                              ~  NA_character_
    ))
data$employ <- as_factor(data$employ, levels = c("Fulltime", "Parttime", "Unemployed", "Student", "Not in labor force"))

# Respondent's Mother's Work History
data <- data %>%
  mutate(
    respmom = case_when(
      respmom   == 1                                    ~ "Yes, she worked for pay",
      respmom   == 2                                    ~ "No",
      TRUE                                              ~  NA_character_
    ))
data$respmom <- as_factor(data$respmom)

# Previous Homemaker Status
data <- data %>%
  mutate(
    homemaker = case_when(
      ((rwork1  >= 2 & rwork1  <= 3) & SEX == "Female") | 
      ((spwork1 >= 2 & spwork1 <= 3) & SEX == "Male")   |
      ((rwork2  >= 2 & rwork2  <= 3) & SEX == "Female") | 
      ((spwork2 >= 2 & spwork2 <= 3) & SEX == "Male")   ~ "Homemaker",
      TRUE                                              ~ "Other"
    ))
data$homemaker <- as_factor(data$homemaker)
data <- within(data, homemaker <- relevel(homemaker, ref = "Other"))


# Housework
# check Chronbach's alpha
hswrk <- subset(data, select = c("laundry", "repairs", "carewk", "groceries", "cleaning", "meals"))
library(psych)
alpha(hswrk)
remove(hswrk)

data <- data %>%
  mutate_at(.vars = vars(laundry:meals), 
            .funs = funs(case_when(
            . ==3 | . ==6                    ~ "Equal",
          ((. ==1 | . ==2) & SEX=="Female")  | 
          ((. ==4 | . ==5) & SEX=="Male")    ~ "Woman",
          ((. ==1 | . ==2) & SEX=="Male")    | 
          ((. ==4 | . ==5) & SEX=="Female")  ~ "Man",
              TRUE                             ~ NA_character_
            ))) %>%
  mutate_at(.vars = vars(laundry:meals), 
            .funs = funs(factor(., levels = c("Woman", "Equal", "Man"), ordered = TRUE)))


data <- data %>%
  mutate_at(.vars = vars(laundry:meals), 
            .funs = funs(unclass)) # Convert to underlying integer representation of factor

data$hswrk<-rowSums(data[,19:24])-6  # Create Housework Index

# Happiness with Family Life
library(forcats)
data$famlife <- as_factor(data$famlife)
levels(data$famlife)[levels(data$famlife)=="Can't choose"] <- NA
levels(data$famlife)[levels(data$famlife)=="No answer"] <- NA
levels(data$famlife)[levels(data$famlife)=="Not applicable, no partner (BG: 2,3,7 in PARTLIV); NAV:ZA"] <- NA
data$famlife <- fct_rev(data$famlife)
data$famlife <- as.numeric(data$famlife)


## Lower case variable names
colnames(data) <- tolower(colnames(data))

## Apply variable labels
attr(data$relinc, 'label')     <- 'Earnings homogamy'
attr(data$pool, 'label')       <- 'Income allocation arrangement'
attr(data$marst, 'label')      <- 'Marital status'
attr(data$homemaker, 'label')  <- 'Prior homemaker'
attr(data$hswrk, 'label')      <- 'Housework index'

# Create the sample
## Sample of cohabs - keep only if there were at least 30 cohabs in a country
marstfreq <- data %>%
  group_by(country) %>%
  count(marst) %>%
  spread(key = marst, value = n) %>%
  subset(select=c("country", "Cohab")) %>%
  ungroup() # Create table of count of cohabs

data <- data %>% 
  left_join(marstfreq, by = "country") # merge new table with primary table

data %>%
  filter(is.na(Cohab)) %>%
  count(country) #idenitfy which countries didn't identify cohabs

marstfreq %>%
  filter(Cohab <= 30) %>%
  count(country) # identify which countries had fewer than 30 cohabs

data <- data %>%
  group_by(country) %>%
  filter(!is.na(Cohab) & Cohab >= 30) %>%
  ungroup() # keep only countries with more than 30 identified cohabs

remove(marstfreq)

# drop Canada and South Africa
data<-data[!(data$country=="Canada"),] # Remove Canada -- error according to codebook
data<-data[!(data$country=="South Africa"),] # Remove South Africa -- no data on attitudes about satisfaction with family life

# Only partnered respondents
data <- filter(data, marst != "Single")

# Select age range
data <- filter(data, age >= 18 & age <=54)

## Keep analysis variables
data <- select(data, pool, code, country, marst, relinc, sex, age, parent, 
               employ, homemaker, degree, hswrk, respmom, famlife)

#Missing data
colSums(is.na(data)) # For key variables
table(data$country, data$pool,    exclude=NULL )
table(data$country, data$relinc,  exclude=NULL )
table(data$country, data$parent,  exclude=NULL )
table(data$country, data$employ,  exclude=NULL )
table(data$country, data$degree,  exclude=NULL )
table(data$country, data$hswrk,   exclude=NULL )
table(data$country, data$respmom, exclude=NULL )
table(data$country, data$famlife, exclude=NULL )

table(data$country)

mytable <- table(data$country, data$marst, data$relinc) #What's up with Italy?
ftable(mytable)
remove(mytable)

## Listwise deletion
data <- na.omit(data)
table(data$country)

# Add Gender Inequality Index

# Data come from : http://hdr.undp.org/sites/default/files/reports/14/hdr2013_en_complete.pdf (page 156)

# create data-frame from scratch 
country <- c("Argentina",	"Australia",	"Chile",	"Czech Republic",	"Finland",	"France",	"Germany",	"Iceland",	"India",	"Ireland",	"Latvia",	"Lithuania",	"Norway",	"Philippines",	"Poland",	"Spain",	"Sweden",	"Switzerland",	"United States",	"Venezuela")
index <- c(0.38,	0.115,	0.36,	0.122,	0.075,	0.083,	0.075,	0.089,	0.61,	0.121,	0.216,	0.157,	0.065,	0.418,	0.14,	0.103,	0.055,	0.057,	0.256,	0.466)
GII <- data.frame(country, index)

data <- data %>% 
  left_join(GII, by = "country") # merge new table with primary table
remove(country)
remove(index)
# remove(GII)

#Convert country back to factor variable
data$country <- factor(data$country)

class(data)
data = as.data.frame(data) # Make dataframe to get rid of "unknown or uninitialised column errors"
data = as_tibble(data) # For viewing output in R