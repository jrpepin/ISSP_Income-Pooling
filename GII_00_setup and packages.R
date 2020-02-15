#------------------------------------------------------------------------------------
# GENDER INEQUALITY INDEX PROJECT
# GII_00_setup and packages.R
# Joanna Pepin and Philip Cohen
#------------------------------------------------------------------------------------

#####################################################################################
## Install and load required packages
#####################################################################################
if(!require(renv)){           # https://rstudio.github.io/renv/articles/renv.html
  install.packages("renv")
  library(renv)
}

if(!require(haven)){
  install.packages("haven")
  library(haven)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(forcats)){
  install.packages("forcats")
  library(forcats)
}

if(!require(psych)){
  install.packages("psych")
  library(psych)
}

if(!require(foreign)){
  install.packages("foreign")
  library(foreign)
}

if(!require(tableone)){
  install.packages("tableone")
  library(tableone)
}

if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

if(!require(cowplot)){
  install.packages("cowplot")
  library(cowplot)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(ggrepel)){
  install.packages("ggrepel")
  library(ggrepel)
}

if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}

if(!require(here)){
  install.packages("here")
  library(here)
}

if(!require(conflicted)){
  devtools::install_github("r-lib/conflicted")
  library(conflicted)
}

renv::snapshot() # Save the state of the project library to the lockfile (called renv.lock)

# Address any conflicts in the packages
conflict_scout() # Identify the conflicts
conflict_prefer("remove", "base")
conflict_prefer("filter", "dplyr")
conflict_prefer("ggsave", "cowplot")

#####################################################################################
# Set-up the Directories
#####################################################################################

projDir <- here()                                 # Filepath to this project's directory
dataDir <- "C:/Users/Joanna/Dropbox/Data/ISSP"    # Name of folder where the ISSP data was downloaded
figDir  <- "figures"                              # Name of the sub-folder where we will save generated figures
outDir  <- "data"                                 # Name of the sub-folder where we will save data output
docDir  <- "docs"                                 # Name of the sub-folder where we will save results

## This will create sub-directory folders in the projDir if they don't exist
if (!dir.exists(here::here(figDir))){
  dir.create(figDir)
} else {
  print("Figure directory already exists!")
}

if (!dir.exists(here::here(outDir))){
  dir.create(outDir)
} else {
  print("Output directory already exists!")
}

if (!dir.exists(here::here(docDir))){
  dir.create(docDir)
} else {
  print("Results directory already exists!")
}

message("End of GII_00_setup and packages") # Marks end of R Script
