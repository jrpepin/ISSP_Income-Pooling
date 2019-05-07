cd "C:\Users\Joanna\Dropbox\Repositories\ISSP_Income-Pooling"
clear
set more off
local logdate = string( d(`c(current_date)'), "%dCY.N.D" )

// Replace NA with nothing in CSV file.
// Delete first column prodced by R output
// Replace m1 - t9 with numbers

/// log using ISSPdata_`logdate'.log, t replace

************************************************************************************************************
/// Import the data

// Data: ISSP2012 wrangled in R. 


import 	delimited C:\Users\Joanna\Dropbox\Repositories\ISSP_Income-Pooling\ISSPdata.csv
save 	"C:\Users\Joanna\Dropbox\Repositories\ISSP_Income-Pooling\ISSPdata.dta", replace

use 	"C:\Users\Joanna\Dropbox\Repositories\ISSP_Income-Pooling\ISSPdata.dta", clear

// gsem (i.schooltype <- englishgrade i.sex M1[class]@1), mlogit nocapslatent latent(M1)
