cd "C:\Users\Joanna\Dropbox\Repositories\ISSP_Income-Pooling"
clear
set more off
local logdate = string( d(`c(current_date)'), "%dCY.N.D" )

/// log using ISSPdata_`logdate'.log, t replace

************************************************************************************************************
/// Import the data

// Data: ISSP2012 wrangled in R. 
use 	"C:\Users\Joanna\Dropbox\Repositories\ISSP_Income-Pooling\ISSPdata.dta", clear

**********************************************************************************************************************
*Table 3
**********************************************************************************************************************
// Model 1
mlogit		pool	index									, cluster(country) rrr
estat 		ic
// Model 2
mlogit		pool	index	 i.relinc						, cluster(country) rrr
estat 		ic
// Model 3
mlogit		pool	index	 i.marst						, cluster(country) rrr
estat 		ic


**********************************************************************************************************************
*Table 4 - Model 4
**********************************************************************************************************************
global 	ivars "sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit		pool	index i.relinc 		i.marst		$ivars 					, cluster(country) rrr
estat 		ic

**********************************************************************************************************************
*Table 4 - Model 5
**********************************************************************************************************************
global 	ivars " sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit		pool	c.index##i.relinc	i.marst		$ivars 					, cluster(country) rrr
estat 		ic

// Figure 2
mlogit		pool	c.index##i.relinc	i.marst		$ivars					, cluster(country)

margin 	relinc, at(index=(.05(.02).62)) predict(outcome(1))	/* One Manages 	*/
margin 	relinc, at(index=(.05(.02).62)) predict(outcome(2))	/* Manage Equal */
margin 	relinc, at(index=(.05(.02).62)) predict(outcome(3))	/* Pool Some	*/
margin 	relinc, at(index=(.05(.02).62)) predict(outcome(4))	/* Keep Sep		*/

**********************************************************************************************************************
*Table 4 - Model 6
**********************************************************************************************************************
global 	ivars "i.marst sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit		pool	c.index##i.marst 		i.relinc $ivars 					, cluster(country) rrr
estat 		ic


// Figure 3
mlogit		pool	c.index##i.marst 		i.relinc $ivars 					, cluster(country)
margin 		i.marst, at(index=(.05(.02).62)) predict(outcome(1))		/* One Manages 	*/
margin 		i.marst, at(index=(.05(.02).62)) predict(outcome(2))	 	/* Manage Equal */
margin 		i.marst, at(index=(.05(.02).62)) predict(outcome(3))	 	/* Pool Some	*/
margin 		i.marst, at(index=(.05(.02).62)) predict(outcome(4))	 	/* Keep Sep		*/


**********************************************************************************************************************
* Figure data
**********************************************************************************************************************
// Figure 2
global 	ivars " sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit	pool	c.index##i.relinc	i.marst		$ivars						, cluster(country)
margin 		relinc, at(index=(.05(.02).62)) predict(outcome(1)) post		/* One Manages 	*/
est store 	oneman

mlogit		pool	c.index##i.relinc	i.marst		$ivars					, cluster(country)
margin 		relinc, at(index=(.05(.02).62)) predict(outcome(2))	post		/* Manage Equal */
est store 	equal

mlogit		pool	c.index##i.relinc	i.marst		$ivars					, cluster(country)
margin 		relinc, at(index=(.05(.02).62)) predict(outcome(3))	post		/* Pool Some	*/
est store 	some

mlogit		pool	c.index##i.relinc	i.marst		$ivars					, cluster(country)
margin 		relinc, at(index=(.05(.02).62)) predict(outcome(4))	post		/* Keep Sep		*/
est store 	sep

est table oneman equal some sep 

// Figure 3
global 	ivars "i.marst sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit		pool	c.index##i.marst 		i.relinc $ivars 				, cluster(country)
margin 		i.marst, at(index=(.05(.02).62)) predict(outcome(1)) post		/* One Manages 	*/
est store 	oneman

mlogit		pool	c.index##i.marst 		i.relinc $ivars 				, cluster(country)
margin 		i.marst, at(index=(.05(.02).62)) predict(outcome(2)) post	 	/* Manage Equal */
est store 	equal

mlogit		pool	c.index##i.marst 		i.relinc $ivars 				, cluster(country)
margin 		i.marst, at(index=(.05(.02).62)) predict(outcome(3)) post	 	/* Pool Some	*/
est store 	some

mlogit		pool	c.index##i.marst 		i.relinc $ivars 				, cluster(country)
margin 		i.marst, at(index=(.05(.02).62)) predict(outcome(4)) post	 	/* Keep Sep		*/
est store 	sep

est table oneman equal some sep 
**********************************************************************************************************************
*Appendix Table 2
**********************************************************************************************************************
// Dual-earners
preserve
drop if dualearn==2 /* Drops 1,353 respondents */
count
global 	ivars "sex age parent i.employ homemaker ib4.degree hswrk respmom famlife"

mlogit		pool	index i.relinc 		i.marst		$ivars 					, cluster(country) rrr
estat 		ic
mlogit		pool	c.index##i.relinc	i.marst		$ivars					, cluster(country) rrr
estat 		ic
mlogit		pool	c.index##i.marst 	i.relinc 	$ivars 					, cluster(country) rrr
estat 		ic
restore
