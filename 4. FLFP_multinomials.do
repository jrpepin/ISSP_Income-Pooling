*-------------------------------------------------------------------------------
* INCOME-POOLING PROJECT
* FLFP_multinomials.do
* Joanna Pepin and Philip Cohen
*-------------------------------------------------------------------------------
cd "C:\Users\Joanna\Dropbox\Repositories\ISSP_Income-Pooling"
clear
set more off
local logdate = string( d(`c(current_date)'), "%dCY.N.D" )

log using "docs\ISSPdata_FLFP_`logdate'.log", t replace
di "$S_DATE"
************************************************************************************************************
/// Import the data

// Data: ISSP2012 wrangled in R. 
use 	"C:\Users\Joanna\Dropbox\Repositories\ISSP_Income-Pooling\data\ISSPdata.dta", clear

drop if country == 9 // drop India as an outlier

**********************************************************************************************************************
*Table 3
**********************************************************************************************************************
// Model 1
mlogit		pool	flfp									, cluster(country) rrr
estat 		ic
// Model 2
mlogit		pool	flfp	 i.relinc						, cluster(country) rrr
estat 		ic
// Model 3
mlogit		pool	flfp	 i.marst						, cluster(country) rrr
estat 		ic


**********************************************************************************************************************
*Table 4 - Model 4
**********************************************************************************************************************
global 	ivars "sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit		pool	flfp i.relinc 		i.marst		$ivars 					, cluster(country) rrr
estat 		ic

**********************************************************************************************************************
*Table 4 - Model 5
**********************************************************************************************************************
global 	ivars " sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit		pool	c.flfp##i.relinc	i.marst		$ivars 					, cluster(country) rrr
estat 		ic

// Figure C
mlogit		pool	c.flfp##i.relinc	i.marst		$ivars					, cluster(country)

margin 	relinc, at(flfp=(39(1)51)) predict(outcome(1))	/* One Manages 	*/
margin 	relinc, at(flfp=(39(1)51)) predict(outcome(2))	/* Manage Equal */
margin 	relinc, at(flfp=(39(1)51)) predict(outcome(3))	/* Pool Some	*/
margin 	relinc, at(flfp=(39(1)51)) predict(outcome(4))	/* Keep Sep		*/

**********************************************************************************************************************
*Table 4 - Model 6
**********************************************************************************************************************
global 	ivars "i.marst sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit		pool	c.flfp##i.marst 		i.relinc $ivars 					, cluster(country) rrr
estat 		ic


// Figure D
mlogit		pool	c.flfp##i.marst 		i.relinc $ivars 					, cluster(country)
margin 		i.marst, at(flfp=(39(1)51)) predict(outcome(1))		/* One Manages 	*/
margin 		i.marst, at(flfp=(39(1)51)) predict(outcome(2))	 	/* Manage Equal */
margin 		i.marst, at(flfp=(39(1)51)) predict(outcome(3))	 	/* Pool Some	*/
margin 		i.marst, at(flfp=(39(1)51)) predict(outcome(4))	 	/* Keep Sep		*/


**********************************************************************************************************************
* Figure data
**********************************************************************************************************************
// Figure C
global 	ivars " sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit	pool	c.flfp##i.relinc	i.marst		$ivars						, cluster(country)
margin 		relinc, at(flfp=(39(1)51)) predict(outcome(1)) post		/* One Manages 	*/
est store 	oneman

mlogit		pool	c.flfp##i.relinc	i.marst		$ivars					, cluster(country)
margin 		relinc, at(flfp=(39(1)51)) predict(outcome(2))	post		/* Manage Equal */
est store 	equal

mlogit		pool	c.flfp##i.relinc	i.marst		$ivars					, cluster(country)
margin 		relinc, at(flfp=(39(1)51)) predict(outcome(3))	post		/* Pool Some	*/
est store 	some

mlogit		pool	c.flfp##i.relinc	i.marst		$ivars					, cluster(country)
margin 		relinc, at(flfp=(39(1)51)) predict(outcome(4))	post		/* Keep Sep		*/
est store 	sep

est table oneman equal some sep 

// Figure D
global 	ivars "i.marst sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit		pool	c.flfp##i.marst 		i.relinc $ivars 				, cluster(country)
margin 		i.marst, at(flfp=(39(1)51)) predict(outcome(1)) post		/* One Manages 	*/
est store 	oneman

mlogit		pool	c.flfp##i.marst 		i.relinc $ivars 				, cluster(country)
margin 		i.marst, at(flfp=(39(1)51)) predict(outcome(2)) post	 	/* Manage Equal */
est store 	equal

mlogit		pool	c.flfp##i.marst 		i.relinc $ivars 				, cluster(country)
margin 		i.marst, at(flfp=(39(1)51)) predict(outcome(3)) post	 	/* Pool Some	*/
est store 	some

mlogit		pool	c.flfp##i.marst 		i.relinc $ivars 				, cluster(country)
margin 		i.marst, at(flfp=(39(1)51)) predict(outcome(4)) post	 	/* Keep Sep		*/
est store 	sep

est table oneman equal some sep 

log close
