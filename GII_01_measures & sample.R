#------------------------------------------------------------------------------------
# GENDER INEQUALITY INDEX PROJECT
# GII_02_measures & sample.R
# Joanna Pepin and Philip Cohen
#------------------------------------------------------------------------------------

# This file creates the variables for analysis.

#####################################################################################
# Load the data for analysis
#####################################################################################
## ZA5900: International Social Survey Programme: Family and Changing Gender Roles IV - ISSP 2012
## Data can be accessed here: https://www.gesis.org/issp/modules/issp-modules-by-topic/family-and-changing-gender-roles/2012/
## Data import code assumes the researcher downloaded the Stata data files.

setwd(dataDir) # This will set the working directory to the folder where the data is stored

issp12 <- read_dta("ISSP_2012_Families.dta") # Change this to whatever you named the downloaded data file.

setwd(projDir) # Set working directory back to project directory

## Look at the data---------------------------------------------------------------------
head(issp12, n = 30)

## Select the Variables-----------------------------------------------------------------
data <- select(issp12, MARITAL, PARTLIV,  V41, V3, SEX, AGE, V67, 
               HHCHILDR, HHTODD, DEGREE, MAINSTAT, WRKHRS,
               V59, V60, V61, V62, V63, V50, V42, V43, V44, V45, V46, V47, V57)

## Rename variables---------------------------------------------------------------------
data <- rename(data, 
               pool	     =	V41,	nation	  =	V3,	  duration =	V67,
               respmom	 =	V59,	rwork1	  =	V60,	spwork1	 =	V62,
               rwork2	   =	V61,	spwork2	  =	V63,	relinc	 =	V50,
               laundry	 =	V42,	repairs	  =	V43,	carewk	 =	V44,
               groceries =	V45,	cleaning	=	V46,	meals	   =	V47,
               famlife	 =	V57)

## Save temporary copy of dataset------------------------------------------------------
safe <- data # create a temporary datafile if needed.
# data <- safe # If need to rerun

#####################################################################################
# Prep the data for analysis
#####################################################################################

# Country----------------------------------------------------------------------------
data$nation <- as_factor(data$nation)
data <- data %>% 
  separate(nation, c("Code", "Country"), extra = "merge", fill = "left")

data$Country[data$Country == "W-Germany-West"]    <- "Germany"
data$Country[data$Country == "E-Germany-East"]    <- "Germany"
data$Country[data$Country == "GBN-Great Britain"] <- "Great Britain"

data$Code    <- as_factor(data$Code)
data$Country <- as_factor(data$Country)

# Marital status-------------------------------------------------------------------
data <- data %>%
  mutate(
    marst = case_when(
      MARITAL   == 1 |  MARITAL == 2                                    ~ "Married",
      PARTLIV   == 1 & (MARITAL >= 3 & MARITAL <=6)                     ~ "Cohab",
      PARTLIV   != 1 & (MARITAL >= 3 & MARITAL <=6)                     ~ "Single",
      TRUE                                                              ~  NA_character_
    ))
data$marst <- as_factor(data$marst)

# Income allocation----------------------------------------------------------------
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

# Sex----------------------------------------------------------------------------
data$SEX <- as_factor(data$SEX)
levels(data$SEX)[levels(data$SEX)=='No answer'] <- NA

# Earnings Homogamy--------------------------------------------------------------
data <- data %>%
  mutate(
    dualearn = case_when(
      relinc >  1 & relinc <  7          ~ "Dual-earner",
      relinc == 1 | relinc == 7          ~ "One homemaker",      
      TRUE                                ~  NA_character_ ))

data$dualearn <- as_factor(data$dualearn)

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

# Age--------------------------------------------------------------------------
data$AGE <- zap_labels(data$AGE)
data$AGE[data$AGE == 999] <- NA

# Duration---------------------------------------------------------------------
# duration only asked of married for US; none for Philippines
data$duration <- zap_labels(data$duration)
data <- data %>%
  mutate(
    duration = case_when(
      duration      == 0 | duration == 1  | duration == 2   ~ "< 3 years",
      duration      >= 3 & duration <= 92                   ~ "3 or more years",
      TRUE                                                  ~  NA_character_
    ))

data$duration <- as_factor(data$duration)
data <- select(data, -duration) # Mark this out if running duration sensitivity test

# Use this code to run a sensitivity test including the duration variable
# and excluding the countries that don't have duration data.
  # data <- data %>%
    # subset(Country != "United States" & Country != "Philippines")

# Parent----------------------------------------------------------------------
data <- data %>%
  mutate(
    parent = case_when(
       HHCHILDR   == 0 & HHTODD == 0                                      ~ "Nonparent",
      (HHCHILDR  >= 1 & HHCHILDR <= 21)  | (HHTODD >= 1 & HHTODD <= 10)   ~ "Parent",
      TRUE                                                                ~  NA_character_
    ))
data$parent <- as_factor(data$parent)
data <- within(data, parent <- relevel(parent, ref = "Nonparent"))

# Education-------------------------------------------------------------------
data$DEGREE <- as_factor(data$DEGREE)
levels(data$DEGREE)[levels(data$DEGREE)=="No answer, CH: don't know"] <- NA
levels(data$DEGREE) <- c('No formal education', 'Primary school', 'Lower secondary', 
                         'Upper secondary', 'Post secondary', 'Lower level tertiary', 
                         'Upper level tertiary')

# Employment------------------------------------------------------------------
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
data$employ <- as_factor(data$employ)
levels(data$employ) <- c("Fulltime", "Parttime", "Unemployed", "Student", "Not in labor force")

# Respondent's Mother's Work History-----------------------------------------
data <- data %>%
  mutate(
    respmom = case_when(
      respmom   == 1                                    ~ "Yes, she worked for pay",
      respmom   == 2                                    ~ "No",
      TRUE                                              ~  NA_character_
    ))
data$respmom <- as_factor(data$respmom)

# Previous Homemaker Status--------------------------------------------------
data <- data %>%
  mutate(
    homemaker = case_when(
      ((rwork1  >= 2 & rwork1  <= 3) & SEX == "Female") | 
      ((spwork1 >= 2 & spwork1 <= 3) & SEX == "Male")   |
      ((rwork2  >= 2 & rwork2  <= 3) & SEX == "Female") | 
      ((spwork2 >= 2 & spwork2 <= 3) & SEX == "Male")   ~ "Homemaker",
      TRUE                                              ~ "Other"
    ))

  ## Alternative homemaker coding (any employment vs other)
  # data <- data %>%
  #     mutate(
  #        homemaker = case_when(
  #          ((rwork1  >= 1 & rwork1  <= 2) & SEX == "Female") | 
  #          ((spwork1 >= 1 & spwork1 <= 2) & SEX == "Male")   |
  #          ((rwork2  >= 1 & rwork2  <= 2) & SEX == "Female") | 
  #          ((spwork2 >= 1 & spwork2 <= 2) & SEX == "Male")   ~ "Wife Any Employment",
  #          TRUE                                              ~ "Other"
  #       ))
  
data$homemaker <- as_factor(data$homemaker)
data <- within(data, homemaker <- relevel(homemaker, ref = "Other"))

# Housework-------------------------------------------------------------------
# check Chronbach's alpha
hswrk <- subset(data, select = c("laundry", "repairs", "carewk", "groceries", "cleaning", "meals"))

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

data <- data %>%
  mutate(hswrk = rowSums(select(., laundry:meals))) # Create Housework Index

data$hswrk<- data$hswrk - 6 # Make the scale start at 0.

# Happiness with Family Life-----------------------------------------------

data$famlife <- as_factor(data$famlife)
levels(data$famlife)[levels(data$famlife)=="Can't choose"] <- NA
levels(data$famlife)[levels(data$famlife)=="No answer"] <- NA
levels(data$famlife)[levels(data$famlife)=="Not applicable, no partner (BG: 2,3,7 in PARTLIV); NAV:ZA"] <- NA
data$famlife <- fct_rev(data$famlife)
data$famlife <- as.numeric(data$famlife)

#####################################################################################
# Clean up the variable names and labels
#####################################################################################

## Lower case variable names
colnames(data) <- tolower(colnames(data))

## Apply variable labels
attr(data$relinc,    'label')   <- 'Earnings homogamy'
attr(data$pool,      'label')   <- 'Income allocation arrangement'
attr(data$marst,     'label')   <- 'Marital status'
attr(data$homemaker, 'label')   <- 'Prior homemaker'
attr(data$hswrk,     'label')   <- 'Housework index'

#####################################################################################
# Create the sample
#####################################################################################

# Sample of cohabs-------------------------------------------------------------------
## keep only if there were at least 30 cohabs in a country
marstfreq <- data %>%
  group_by(country) %>%
  count(marst) %>%
  spread(key = marst, value = n)

names(marstfreq)[names(marstfreq) == "<NA>"] <- "Missing" # Change column name to missing
marstfreq[is.na(marstfreq)] <- 0                          # Replace missing with zeros

colnms=c("Married", "Single", "Cohab", "Missing")         # identify column names to be summed
marstfreq$total<-rowSums(marstfreq[,colnms])              # Sum marital status for n count

marstfreq <- transform(marstfreq, percohab = round(Cohab / total, digits=2)) # proportion cohabiting by country

marstfreq <- marstfreq %>%
  subset(select=c("country", "Cohab", "percohab")) %>%
  ungroup() # Create table of count and percent of cohabs

data <- data %>% 
  left_join(marstfreq, by = "country") # merge new table with primary table

data %>%
  filter(Cohab==0) %>%
  count(country) #idenitfy which countries didn't identify cohabs

marstfreq %>%
  filter(Cohab <= 30) %>%
  count(country) # identify which countries had fewer than 30 cohabs

data <- data %>%
  group_by(country) %>%
  filter(Cohab >= 30) %>%
  ungroup() # keep only countries with more than 30 identified cohabs

remove(marstfreq)

# drop Canada and South Africa---------------------------------------------------------

## Remove Canada
data<-data[!(data$country=="Canada"),]  # error according to codebook

##Remove South Africa
data<-data[!(data$country=="South Africa"),] # No data on attitudes about satisfaction with family life

# Only partnered respondents----------------------------------------------------------
data <- filter(data, marst != "Single")

# Select age range--------------------------------------------------------------------
data <- filter(data, age >= 18 & age <=54)

# Keep analysis variables-------------------------------------------------------------
data <- select(data, 
               pool, code, country, marst, relinc, sex, age, parent, 
               employ, homemaker, degree, hswrk, respmom, famlife, dualearn,
               percohab)

# Missing data------------------------------------------------------------------------
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

mytable <- table(data$country, data$marst, data$relinc)
ftable(mytable)
remove(mytable)

## Listwise deletion----------------------------------------------------------------
data <- na.omit(data) %>%
  droplevels()
table(data$country)

#####################################################################################
# Add Gender Inequality Index
#####################################################################################

# Data come from : http://hdr.undp.org/sites/default/files/reports/14/hdr2013_en_complete.pdf (page 156)

# create data-frame from scratch------------------------------------------------------
country <- c("Argentina",	"Australia",	"Chile",	"Czech Republic",	"Finland",	"France",	"Germany",	
             "Iceland",	"India",	"Ireland",	"Latvia",	"Lithuania",	"Norway",	"Philippines",	"Poland",	
             "Spain",	"Sweden",	"Switzerland",	"United States",	"Venezuela")
index <- c(0.38,	0.115,	0.36,	0.122,	0.075,	0.083,	0.075,	0.089,	0.61,	0.121,	0.216,	0.157,	0.065,	
           0.418,	0.14,	0.103,	0.055,	0.057,	0.256,	0.466)

GII <- data.frame(country, index)
GII$index <- GII$index * 100 # Rescale the index because a 1 unit change doesn't really exist.

data <- data %>% 
  left_join(GII, by = "country") # merge new table with primary table
remove(country)
remove(index)
# remove(GII)

#####################################################################################
# Add Country Level Female Labor Force Participation
#####################################################################################

## For Reviewer response
## Data come from : https://data.worldbank.org/indicator/SL.TLF.TOTL.FE.ZS?end=2019&most_recent_year_desc=false&start=1990 (2012 FLFP)

## create data-frame from scratch-----------------------------------------------------
country <- c("Argentina",	"Australia",	"Chile",	"Czech Republic",	"Finland",	"France",	
             "Germany",	"Iceland",	"India",	"Ireland",	"Latvia",	"Lithuania",	"Norway",	
             "Philippines",	"Poland",	"Spain",	"Sweden",	"Switzerland",	"United States",	"Venezuela")
flfp <- c(41.35,	45.43,	40.70,	43.74,	48.07,	47.30,	45.90,	47.53,	21.27,	45.08,	
          49.95,	50.63,	47.27,	39.18,	45.07,	45.38,	47.44,	46.03,	45.98,	39.92)

FLFP <- data.frame(country, flfp)

data <- data %>% 
  left_join(FLFP, by = "country") # merge new table with primary table
remove(country)
remove(flfp)

# remove(FLFP)

#####################################################################################
# Prep dataframe for analysis
#####################################################################################

# Convert country back to factor variable--------------------------------------------
data$country <- factor(data$country)

# Make dataframe to get rid of "unknown or uninitialised column errors"
class(data)
data = as.data.frame(data) 
data = as_tibble(data) # For viewing output in R