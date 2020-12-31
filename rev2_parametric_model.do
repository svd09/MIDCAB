/* date */
 
 // 11/28/2020
/* JTCVS rev2. do file for parametric models
use lvef < 40%, age = continuous variable, incomplete revasc. */
 



// at the beginning of every do file:

macro drop _all // remove macros from previous work, if any
capture log close // Close any open logs. Capture will ignore a command that gives 
//                   an error. So if there isn't an open log, instead of giving you 
//                   an error and stopping here, it'll just move onto the next line.


clear all // clean the belfries
drop _all // get rid of everything!

/* set wd */


cd "C:/github_rcode/midcab/midcab_paper/JTCVS_rev1/JTCVS_rev2/"

/* log */

log using parametric_model.log, replace text // change the name of this to whatever you'd like

/* The purpose of this .do file is parametric survival model for the 
MIDCAB paper - JTCVS rev2. The comments were to use age as continous variable, lvef < 40% and also 
include incomplete revascularisation in the models.
the models in the earlier rev1 was exponential, so will use the same distribution here.*/

version 15 // Every version of Stata is slightly different, but all are backwards 
//            compatible with previous ones. If you open up this do file with a way 
//            newer version, it'll run it in version 14 compatibility mode. Change 
//            this to the current version of Stata that you are using. This will 
//            also keep your code from running on older versions of stata that will 
//            break with new code that it isn't designed to handle. 

set more off, permanently // so you don't have to keep clicking through stata to 
//                           keep it running

set linesize 255 // this keeps longer lines from getting clipped. Helpful for making 
//                  tables.


/* set scheme for graphs */

set scheme sj

/* import data */
/* need to use only the dataset for the regression model */
 
import delim "C:/github_rcode/midcab/midcab_paper/JTCVS_rev1/pdf.csv", /// 
numericcols (2 3 4 5 6 7 8 11) clear // use numericcols to convert col to numeric format here

/* start code here */

// data

codebook, compact

// stset the data and then decribe to understand

stset fupyear, fail(died) 

// see the simple plots for the dataset

sts graph, survival 

/* know % missing */

misstable summ, all

/* need to encode many string to factor variables */

misstable summ, all

/* determine the mode for variables to fill the missing cells */

replace diabetes = 0 if diabetes == .
replace hyperlipemia = 0 if hyperlipemia == .
replace pre_dialysis = 0 if pre_dialysis == .
replace prior_pci = 0 if prior_pci == .
replace pre_mi = 0 if pre_mi == .
replace pad = 0 if pad == .


/* confirm that no missing */

misstable summ, all

// incomplete revasc appears to have -9

tab incompl_revascularisation

gen incomp_revasc = .
replace incomp_revasc = 0 if incompl_revascularisation == 0
replace incomp_revasc = 1 if incompl_revascularisation == 1
replace incomp_revasc = 0 if incomp_revasc == .

tab incomp_revasc

/* fit a parametric survival model, exponential distribution */
/*right now fit the model without the time variable */
/* we can then see how to fit the time variable into the model */

gen female_n = .
replace female_n = 0 if female == "no"
replace female_n = 1 if female == "yes"
tab female_n

summ pre_lvef

replace pre_lvef = 54 if pre_lvef == -9

summ pre_lvef



gen low_lvef = .

replace low_lvef = 1 if pre_lvef <= 40
replace low_lvef = 0 if pre_lvef > 40

tab low_lvef



// plan to run seperate models on each group too
// save data for each group seperately 

gen fupyears2 = fupyear + 0.01

stset fupyears2, fail(died)

streg i.diabetes i.hyperlipemia i.copd i.pre_dialysis i.prior_pci  incomp_revasc ///
i.pre_mi i.pre_cva i.prior_cardiac_surgery i.pad age_at_surgery i.female_n low_lvef ///
, distribution(exponential)


// save this dataaset so that we can use it for each time period model.

save "C:/github_rcode/midcab/midcab_paper/JTCVS_rev1/JTCVS_rev2/model_data", replace 
//--------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------

// to determine if there is any change in the importance of variables over time
// split the data into 3 groups A,B and C and rerun the same model on each dataaset



use era1_data, replace

// run the model and save the HR 

streg i.diabetes i.hyperlipemia i.copd i.pre_dialysis i.prior_pci ///
i.pre_mi i.pre_cva i.prior_cardiac_surgery i.pad ib(2).age_group i.female_n ///
, distribution(exponential)

// for era B

clear

use era2_data

streg i.diabetes i.hyperlipemia i.copd i.pre_dialysis i.prior_pci ///
i.pre_mi i.pre_cva i.prior_cardiac_surgery i.pad ib(2).age_group i.female_n ///
, distribution(exponential)

// for era C

clear

use era3_data

streg i.diabetes i.hyperlipemia i.copd i.pre_dialysis i.prior_pci ///
i.pre_mi i.pre_cva i.prior_cardiac_surgery i.pad ib(2).age_group i.female_n ///
, distribution(exponential)


// ageg is very odd

tab ageg died

sts graph, by(ageg)

// this is the survival model for mortality done
// use the data again for relative survival in another do file

// example for plotting age as spline with streg

// make the spline term for age

mkspline age_s = age_at_surgery, nknots(4) cubic displayknots

mat knots = r(knots)


streg i.diabetes i.hyperlipemia i.copd i.pre_dialysis i.prior_pci ///
i.pre_mi i.pre_cva i.prior_cardiac_surgery i.pad age_s* female_n ///
, distribution(exponential)


glm diabetes age_s*, family(binomial)


// graph the spline term now

xbrcspline age_s, matknots(knots) values (40 50 60) ref(60) eform



// At the very end of your .do file: 
log close
// Fin.