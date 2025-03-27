# refactored-couscous
Microeconomics Coursework Final Project

************************
***22/498026/EK/24005***

*----------------------------------------------------
**|||Data Preparation|||**
*----------------------------------------------------
*Note: Data berikut hanya data di Negara Bagian Louisiana*



*(((FILTERING DATA)))
keep if age >= 19 & age <= 64 //expansion criteria
keep if year >= 2017 //efektifnya ekspansi
drop if uhrswork == 0 //drop missing values
drop if poverty == 0 | poverty > 400 //0 means N/A & > 400 can not acces the treatment
drop hhwt cluster strata perwt

*(((Recoding)))
*# Has Insurance Through Medicaid or not
replace hinscaid = 0 if hinscaid ==1
replace hinscaid = 1 if hinscaid ==2
label define hinscaid_lbl 0 "no insurance through medicaid" 1 "has insurace through medicaid"
label value hinscaid hinscaid_lbl
label variable hinscaid "=1 has health insurance through medicaid, 0 otherwise"
format hinscaid %9.0g


//Dependent Variable
summ uhrswork, detail // to see the descriptive statistic
histogram uhrswork // visualization of the data
gen log_uhrswork = log(uhrswork) //transform because the variability so high
label variable log_uhrswork "log of usual hours worked per week"


//Explanatory Variable
*1. Sex
replace sex = 0 if sex ==2
label define sex_lbl 0 "female" 1 "male"
label values sex sex_lbl

*2. Marital Status
replace marst = 1 if inlist(marst, 2, 3) 
replace marst = 0 if inlist(marst, 4, 5, 6)
label define marst_lbl 0 "Not Married" 1 "Married"
label values marst marst_lbl

*3. Education
replace educ = 0 if educ <=6 //as a baseline
replace educ = 1 if educ >6 //individual with higher education
label define educ_lbl 0 "Without Higher Education" 1 "Higher Education"
label values educ educ_lbl

*(((Generating Variable)))
gen cutoff=138 
gen expansion = (poverty <= 138 & hinscaid ==0) //state requirement
gen centered = poverty - cutoff //centering the threshold
gen centered_sq = centered^2 // bacause correlation between running variable and outcome is quadratic


*-------------------------------------------------------------------------*
* Conditions For Valid Causal Inference                                             
*-------------------------------------------------------------------------*


*************************************************
*             Condition 1 & 2                   * 
* 1. Assignment Rule must be clear 	            * 
* 2. The running variable is an ordinal measure * 
*************************************************

* hinscaid
scatter hinscaid centered, msymbol(o) mcolor(%1) xline(0, lwidth(med) lcolor(red))  //graph with centering

*************************************************
*             Condition 3:                      * 
*  Cannot be Manipulate their own value of the -*
*  running variable                             *
*************************************************

rddensity centered, plot kernel(uniform) //Manipulation test for self selection as well as sorting arround the threshold


*************************************************
*             Condition 4:                      * 
*       Continuity in covariates                *
*************************************************

*1. Sex
rdbwselect sex centered, kernel(uniform) c(0) p(2) bwselect(mserd) //optimal bandwidth
rdplot sex centered, kernel(uniform) c(0) p(2) //visualization
eststo m1: reg sex expansion centered centered_sq c.centered#i.expansion c.centered_sq#i.expansion if abs(centered)<51, robust

*2. Educ
rdbwselect educ centered, kernel(uniform) c(0) p(2) bwselect(mserd) //optimal bandwidth
rdplot educ centered, kernel(uniform) c(0) p(2) //visualization
eststo m2: reg educ expansion centered centered_sq c.centered#i.expansion c.centered_sq#i.expansion if abs(centered) <43, robust

*3. Marst
rdbwselect marst centered, kernel(uniform) c(0) p(2) bwselect(mserd) //optimal bandwidth
rdplot marst centered, kernel(uniform) c(0) p(2) //visualization
eststo m3: reg marst expansion centered centered_sq c.centered#i.expansion c.centered_sq#i.expansion if abs(centered)<39, robust


esttab m1 m2 m3 using covariates.rtf, se replace //export



*-------------------------------------------------------------------------------
**|||Graphical Analysis for Outcome|||***
*-------------------------------------------------------------------------------

//1. Optimal Bandwidth selection
rdbwselect log_uhrswork centered, kernel(uniform) c(0) p(2) bwselect(mserd) 

//2. Discontinuity Checking Arround the threshold
rdplot log_uhrswork centered i.sex i.educ i.marst, kernel(uniform) c(0) p(2)// default number of bins
rdplot log_uhrswork centered i.sex i.educ i.marst, kernel(uniform) c(0) p(2) nbins(40 52)//number of bins disesuaikan agar lebih jelas


*-------------------------------------------------------------------------------
**|||Estimation|||***
*-------------------------------------------------------------------------------


//different slope below and above the cutoff
gen below = centered*expansion //kontribusi running variabel terhadap kelompok yang eligible dibawah cutoff
gen below_sq = centered_sq*expansion

gen above = centered*(1-expansion) //kontribusi running variabel terhadap kelompok yang eligible diatas cutoff
gen above_sq = centered_sq*(1-expansion)


*##ada manipulasi hasil rddensity --> hinscaid endogen
ivregress 2sls log_uhrswork (hinscaid=expansion) above below above_sq below_sq i.sex i.educ i.marst  if centered>= -33 & centered <=33, first robust 

*ekspor 
ivregress 2sls log_uhrswork (hinscaid = expansion) above below above_sq below_sq i.sex i.educ i.marst if centered>= -33 & centered <=33, first robust
estimates store model_2sls
esttab model_2sls using results.rtf,se replace //export


***sensitivity check of the bandwidth***
eststo m4: ivregress 2sls log_uhrswork (hinscaid = expansion) above below above_sq below_sq i.sex i.educ i.marst if centered>= -16.5 & centered <=16.5, first robust //0.5*Bandwidth

eststo m5: ivregress 2sls log_uhrswork (hinscaid = expansion) above below above_sq below_sq i.sex i.educ i.marst if centered>= -33 & centered <=33, first robust //Optimal Bandwidth

eststo m6: ivregress 2sls log_uhrswork (hinscaid = expansion) above below above_sq below_sq i.sex i.educ i.marst if centered>= -66 & centered <=66, first robust //2*Bandwidth

eststo m7: ivregress 2sls log_uhrswork (hinscaid = expansion) above below above_sq below_sq i.sex i.educ i.marst, first robust //Full Bandwidth







