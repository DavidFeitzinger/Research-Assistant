
clear

*set directory for whole dofile

local user = c(username)

display c(username)

*(David's version)
if "`user'" == "Gebruiker" {
	global datapath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling\Data"
	global resultspath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling\Results_new"
}
else {
	global datapath "C:\Users\User\Dropbox\Research\Wealth Inheritance\Analysis\Data"
	global resultspath "C:\Users\User\Dropbox\Research\Wealth Inheritance\Analysis\Results"
}

********************************************************************************
**#          		I. Cleaning and Variable Construction					  
********************************************************************************

import delimited "$datapath\staggered_did_data_2026_04_29.csv" , clear // n = 21,540
//import delimited "$datapath\staggered_did_data_2026_04_28.csv" , clear
//import delimited "$datapath\staggered_did_data_2026_04_21.csv" , clear
//import delimited "$datapath\staggered_did_data_2026_04_17.csv" , clear
//import delimited "$datapath\staggered_did_data_2026_04_16.csv" , clear


sort root_hhid survey_year


**## ID and wave variables

*Generate numeric hhid for FE regressions

encode root_hhid, gen(root_hhid_num)


*survey_wave as counting from 1 instead of year

gen survey_wave=.
replace  survey_wave=1 if survey_year==2010
replace  survey_wave=2 if survey_year==2014
replace  survey_wave=3 if survey_year==2017
replace  survey_wave=4 if survey_year==2021

lab define survey_wave 1 "2010" 2 "2014" 3 "2017" 4 "2021"
lab values survey_wave survey_wave


*Number of waves per individual

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

sort root_hhid survey_year

//Check number of waves across individual-waves and across individuals

/*
tab num_waves

  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          2 |     14,146       65.67       65.67
          3 |      4,962       23.04       88.71
          4 |      2,432       11.29      100.00
------------+-----------------------------------
      Total |     21,540      100.00

collapse num_waves , by(root_hhid_num)
tab num_waves

  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          2 |      7,073       75.77       75.77
          3 |      1,654       17.72       93.49
          4 |        608        6.51      100.00
------------+-----------------------------------
      Total |      9,335      100.00
*/



**## Inheritance variables

*Rename variables

rename treated_inheritance post_inh_dummy

//rename inheritance_year inh_year //Now we have when_1 when_2 when_3 because sometimes multiple inheritances/gifts 

rename inheritance_between_waves inh_amount


*Generate ever_inherited dummy

bysort root_hhid: egen ever_inh_dummy = max(post_inh_dummy)


*Generate dummies for first pre- and post-inheritance waves

gen firstwave_post_inh_dummy=post_inh_dummy 
bysort root_hhid: replace firstwave_post_inh_dummy=. if firstwave_post_inh_dummy[_n-1]==1
bysort root_hhid: replace firstwave_post_inh_dummy=. if firstwave_post_inh_dummy[_n-2]==1

//gen firstwave_pre_inh_dummy=(post_inh_dummy==0)
//bysort root_hhid: replace firstwave_pre_inh_dummy=. if firstwave_pre_inh_dummy[_n+1]==1
//bysort root_hhid: replace firstwave_pre_inh_dummy=. if firstwave_pre_inh_dummy==1 & firstwave_pre_inh_dummy[_n+1]!=0 //set those to missing which did not work in last line (last obs per individual)

gen firstwave_pre_inh_dummy=.
replace firstwave_pre_inh_dummy=0 if firstwave_post_inh_dummy==1
bysort root_hhid: replace firstwave_pre_inh_dummy=1 if firstwave_pre_inh_dummy[_n+1]==0
bysort root_hhid: replace firstwave_pre_inh_dummy=0 if firstwave_pre_inh_dummy[_n-1]==0

gen firstwave_prepost_inh_dummy=firstwave_post_inh_dummy+firstwave_pre_inh_dummy  

duplicates report  root_hhid_num firstwave_prepost_inh_dummy if firstwave_prepost_inh_dummy==1 //Note: all individual have a pre and post observation if they are ever treated


*Generate event_time in terms of survey waves

//Note: better to code event-time in survey waves rather than in years comapred to inheritance which causes problems in analysis because of unbalanced missings)

//rename event_time event_time_year

gen event_time_wave=.

bysort root_hhid_num (survey_year): replace event_time_wave = sum(post_inh_dummy==1) if ever_inh_dummy==1 //Note: this count upwards from first post-inheritance wave
replace event_time_wave=event_time_wave - 1 if ever_inh_dummy==1 & post_inh_dummy==1 //Note: this defines first post-inheritance wave as =0

bysort root_hhid_num: egen n_pre = sum(post_inh_dummy==0) if ever_inh_dummy==1 //count all pre-treatment observations
bysort root_hhid_num (survey_year): gen pre_inh_forward = sum(post_inh_dummy==0) if post_inh_dummy==0 & ever_inh_dummy==1 //count the order of pre-treatment observations forward
gen pre_inh_backward = pre_inh_forward - n_pre - 1 if post_inh_dummy==0 & ever_inh_dummy==1 //turn into backward counting -> defines first pre-inheritance wave as =-1
replace event_time_wave = pre_inh_backward if post_inh_dummy==0 & ever_inh_dummy==1 //combine with the post values
drop n_pre pre_inh_forward pre_inh_backward

//replace event_time_wave=. if survey_year==inh_year
//replace event_time_wave=0 if survey_year==inh_year //This defines individual-waves for which inheritance happens in observation year as =0. These people might already have a wealth jump if inheritance before the survey but they also might not if inheritance after the survey. It depends when inheritance was observed! If information comes from the same wave then fine to code as post. If it comes from waves after then it should be coded as pre! I would need Elisa to do this. For now dropped.

gen event_time_wave_inclC=event_time_wave
replace event_time_wave_inclC=0 if ever_inh_dummy==0


* Generate first observed treated wave for ever-inheritors

gen first_treat_wave = survey_year if event_time_wave == 0 & ever_inh_dummy==1
bysort root_hhid_num: egen cohort_wave = min(first_treat_wave)


*Generate dummies for when inheritance happens and 0 for control group which never inherits

//Note: similar to post_inh_dummy

gen event_m3 = event_time_wave == -3 if ever_inh_dummy==1
replace event_m3 = 0 if missing(event_m3)

gen event_m2 = event_time_wave == -2 if ever_inh_dummy==1
replace event_m2 = 0 if missing(event_m2)

// omit -1

gen event_0 = event_time_wave == 0 if ever_inh_dummy==1
replace event_0 = 0 if missing(event_0)

gen event_1 = event_time_wave == 1 if ever_inh_dummy==1
replace event_1 = 0 if missing(event_1)

gen event_2 = event_time_wave == 2 if ever_inh_dummy==1
replace event_2 = 0 if missing(event_2)

//Note: Why defining these dummies instead of using i.event_time_wave and defining event_time_wave=0 for never inheritors? Because this is not the same! With the manual dummies never inheritors have all dummies=0. With the i.event_time_wave they would have one dummy=1 (event_0 if event_time_wave is not shifted). 


*Generate inheritance amount that is stable within individual from when inh is received (similar to treatment dummy) 

replace inh_amount=. if inh_amount==0

bysort root_hhid_num (survey_wave): gen post_inh_amount = inh_amount if !missing(inh_amount)
bysort root_hhid_num (survey_wave): replace post_inh_amount = post_inh_amount[_n-1] if missing(post_inh_amount) & post_inh_dummy == 1
replace post_inh_amount=0 if post_inh_amount==.

*Generate inheritance amount that is stable within individual 

bysort root_hhid_num: egen ever_inh_amount = max(inh_amount)
replace ever_inh_amount=0 if ever_inh_amount==.

//Note: net_wealth_pre is the wealth in the earliest year of obervation rather than in the year before inheritance which would be probably better for most uses


*Generate Inheritance groups

gen inh_group = .
replace inh_group = 0 
replace inh_group = 1 if ever_inh_amount>0       & ever_inh_amount<20000
replace inh_group = 2 if ever_inh_amount>=20000  & ever_inh_amount<100000
replace inh_group = 3 if ever_inh_amount>=100000

label define inh_group 0 "none" 1 "small" 2 "medium" 3 "large"
label values inh_group inh_group

gen inh_small  = inh_group==1
gen inh_medium = inh_group==2
gen inh_large  = inh_group==3


*Generate Inh-group-specific treatment that switches on after inheritance

gen post_inh_dummy_small  = post_inh_dummy * (inh_group==1)
gen post_inh_dummy_medium = post_inh_dummy * (inh_group==2)
gen post_inh_dummy_large  = post_inh_dummy * (inh_group==3)


*Generate Inh-group-specific event dummies

* small
gen event_sm_m3 = event_time_wave==-3 & inh_group==1
gen event_sm_m2 = event_time_wave==-2 & inh_group==1
gen event_sm_0  = event_time_wave== 0 & inh_group==1
gen event_sm_1  = event_time_wave== 1 & inh_group==1
gen event_sm_2  = event_time_wave== 2 & inh_group==1

* medium
gen event_md_m3 = event_time_wave==-3 & inh_group==2
gen event_md_m2 = event_time_wave==-2 & inh_group==2
gen event_md_0  = event_time_wave== 0 & inh_group==2
gen event_md_1  = event_time_wave== 1 & inh_group==2
gen event_md_2  = event_time_wave== 2 & inh_group==2

* large
gen event_lg_m3 = event_time_wave==-3 & inh_group==3
gen event_lg_m2 = event_time_wave==-2 & inh_group==3
gen event_lg_0  = event_time_wave== 0 & inh_group==3
gen event_lg_1  = event_time_wave== 1 & inh_group==3
gen event_lg_2  = event_time_wave== 2 & inh_group==3

foreach v in event_sm_m3 event_sm_m2 event_sm_0 event_sm_1 event_sm_2 event_md_m3 event_md_m2 event_md_0 event_md_1 event_md_2 event_lg_m3 event_lg_m2 event_lg_0 event_lg_1 event_lg_2 {
    replace `v' = 0 if missing(`v')
}


*Generate Inh-amount * event dummies

gen amt_m3 = ever_inh_amount * (event_time_wave==-3) if ever_inh_dummy==1
replace amt_m3 = 0 if missing(amt_m3)

gen amt_m2 = ever_inh_amount * (event_time_wave==-2) if ever_inh_dummy==1
replace amt_m2 = 0 if missing(amt_m2)

* omit -1

gen amt_0 = ever_inh_amount * (event_time_wave==0) if ever_inh_dummy==1
replace amt_0 = 0 if missing(amt_0)

gen amt_1 = ever_inh_amount * (event_time_wave==1) if ever_inh_dummy==1
replace amt_1 = 0 if missing(amt_1)

gen amt_2 = ever_inh_amount * (event_time_wave==2) if ever_inh_dummy==1
replace amt_2 = 0 if missing(amt_2)



**## Wealth variables


*Define wealth difference to year before and wealth difference to year before when inherited 

sort root_hhid_num survey_year

bysort root_hhid_num : gen net_wealth_diff = net_wealth[_n]-net_wealth[_n-1]

gen net_wealth_diff_inh = net_wealth_diff if post_inh_dummy==1
replace  net_wealth_diff_inh = . if firstwave_post_inh_dummy!=1


*Generate wealth group from first observed wealth
by root_hhid_num: gen first_wealth = net_wealth[1]

gen wealth_group = .
replace wealth_group = 1 if first_wealth < 100000
replace wealth_group = 2 if first_wealth >= 100000 & first_wealth <= 300000
replace wealth_group = 3 if first_wealth > 300000

label define wg 1 "Low (<100k)" 2 "Middle (100k-300k)" 3 "High (>300k)", replace
label values wealth_group wg


*Generate age-specific wealth groups from first observed wealth at certain age

sort root_hhid_num survey_year
bysort root_hhid_num (survey_year): gen first_obs = _n==1

bysort root_hhid_num (survey_year): gen age_first = age if _n==1
bysort root_hhid_num (survey_year): replace age_first = age_first[1]

bysort root_hhid_num (survey_year): gen wealth_first = net_wealth if _n==1
bysort root_hhid_num (survey_year): replace wealth_first = wealth_first[1]

gen agefirst5 = floor(age_first/5)*5
tab agefirst5 if first_obs

egen wealth_terc_first = xtile(wealth_first) if first_obs, by(agefirst5) nq(3)
bysort root_hhid_num (survey_year): replace wealth_terc_first = wealth_terc_first[1]
rename wealth_terc_first wealth_terc

egen wealth_quar_first = xtile(wealth_first) if first_obs, by(agefirst5) nq(4)
bysort root_hhid_num (survey_year): replace wealth_quar_first = wealth_quar_first[1]
rename wealth_quar_first wealth_quar


*Generate Wealth-group-specific event dummies

* low
gen wealth_low_event_m3 = wealth_group==1 & event_time_wave==-3
gen wealth_low_event_m2 = wealth_group==1 & event_time_wave==-2
gen wealth_low_event_0 = wealth_group==1 & event_time_wave==0
gen wealth_low_event_1 = wealth_group==1 & event_time_wave==1
gen wealth_low_event_2 = wealth_group==1 & event_time_wave==2

* middle
gen wealth_mid_event_m3 = wealth_group==2 & event_time_wave==-3
gen wealth_mid_event_m2 = wealth_group==2 & event_time_wave==-2
gen wealth_mid_event_0 = wealth_group==2 & event_time_wave==0
gen wealth_mid_event_1 = wealth_group==2 & event_time_wave==1
gen wealth_mid_event_2 = wealth_group==2 & event_time_wave==2

* high
gen wealth_high_event_m3 = wealth_group==3 & event_time_wave==-3
gen wealth_high_event_m2 = wealth_group==3 & event_time_wave==-2
gen wealth_high_event_0  = wealth_group==3 & event_time_wave==0
gen wealth_high_event_1  = wealth_group==3 & event_time_wave==1
gen wealth_high_event_2  = wealth_group==3 & event_time_wave==2

foreach v in wealth_low_event_m3 wealth_low_event_m2 wealth_low_event_0 wealth_low_event_1 wealth_low_event_2 wealth_mid_event_m3 wealth_mid_event_m2 wealth_mid_event_0 wealth_mid_event_1 wealth_mid_event_2 wealth_high_event_m3 wealth_high_event_m2 wealth_high_event_0 wealth_high_event_1 wealth_high_event_2 {
    replace `v' = 0 if missing(`v')
}



**## Order dataset

order  root_hhid_num survey_year survey_wave   event_time_wave event_time_wave_inclC  value* type* when*  post_inh_dummy ever_inh_dummy  inh_amount post_inh_amount ever_inh_amount  n_inheritance_waves_so_far n_gift_waves_so_far  inh_group  net_wealth  wealth_quar wealth_terc  net_wealth_diff net_wealth_diff_inh


**## Drop observations

* Drop if inheritance but amount=0 //Note: this was actually people with amount==. -> now dropped in original dataset already

//drop if ever_inh_dummy==1 & inh_amount==0 //n=245 individual-waves

//Note: singletons (only one wave) and those with only post-inheritance observations are dropped above 

* Drop from wave when second inheritance received

drop if n_inheritance_waves_so_far >= 2 //Note: we need to drop from second inheritance because otherwise our inheritance shock would be confounded with a later inheritance


* Drop from wave when first gift is received

//drop if n_gift_waves_so_far >= 1 //Note: ABM models inheritance to child when parent dies. But then we need to include in children sample for wealth trajectories the ones who receive a gift such that gift is taken int oaccount for child wealth trajectory but not for shock to it.


/*
bysort root_hhid_num (survey_year): gen prev_year = survey_year[_n-1]
gen gap_years = survey_year - prev_year
gen nonconsecutive = !inlist(gap_years, 3, 4) if !missing(gap_years)
bysort root_hhid_num (survey_year): egen nonconsecutive_resp = max(nonconsecutive)
//br if nonconsecutive_resp==1
//br if nonconsecutive_resp==1 & ever_inh_dummy==1
*/


//replace event_time_wave=-2 if root_hhid=="w2_4832_DE" & survey_year==2014 //Note: only individual who jumps one wave and ever inherits so I code it manually like this. Should be done automatically in code that defines event_time_wave above



save "$datapath\staggered_did_data_cleaned_2026_04_29.dta" , replace


xtset root_hhid_num survey_year
xtdescribe
//n=20,500 individual-waves, n=9,123 individuals




********************************************************************************
**#          				II. Descriptives 							  
********************************************************************************

use "$datapath\staggered_did_data_cleaned_2026_04_29.dta" , clear





********************************************************************************
**#          				III. Regression Analysis - DiD							  
********************************************************************************

//Note on why to use DiD:

//Treated-only trajectory asks: how does wealth vary across event waves among inheritors?
//Simple DiD without controls: control group only enters through a constant baseline level so differences are small
//DiD with time FEs asks: how does wealth vary across event waves among inheritors, relative to what is happening to never-inheritors in the same survey waves?
//DiD with individual FE + time FE asks: how does the same person's wealth move across event waves, relative to their own baseline, net of common survey-wave movements seen in the control group?

// -> Why the treated-only trajectory is not enough:
//Suppose wealth rises over time for everyone because: housing prices rise, stock markets rise, people age and accumulate assets
//Then inheritors' wealth may rise from -1 to 0 even if inheritance had no causal effect.
//The treated-only regression would still show a positive event_0.
//The DiD logic says: compare that rise to the rise among similar untreated people over the same period. That is the missing counterfactual in the treated-only trajectory.

//But: This is interesting to estimate the causal effect of inheritance on wealth trajectories. But for ABM we need the wealth trajectory before and after inheritance as a whole, right? Or actually, do we need the wealth trajectories across ages and an inheritance effect on wealth apart from that?

//In short:
//Your treated-only event-wave regression tells you: what inheritors' wealth path looks like
//Your DiD setup tells you: how inheritors' wealth path differs from the path you would expect based on the control group
//In the simplest OLS version, those can look similar because the control group is only entering weakly.
//With time FE and especially individual FE, the difference becomes much more meaningful.




****************************
**# 6. By Wealth group - Continuous Treatment
****************************

use "$datapath\staggered_did_data_cleaned_2026_04_29.dta" , clear


drop if event_time_wave==-3 //only n=39 (those with all 4 waves observed and inheritance between wave 3 and 4)

drop if event_time_wave==2 //only n=79 (those with all 4 waves observed and inheritance between wave 1 and 2)


replace event_time_wave=event_time_wave+3


drop if age > 65


xtset root_hhid_num survey_year

xtdescribe




**## 6.4 Fixed-Effects DiD 


*6.4.1 Wealth group 

reghdfe net_wealth c.(amt_m2 amt_0 amt_1 )##i.wealth_group , absorb(root_hhid_num survey_year)   //Note: amt_2 dropped because very few observations

preserve

tempname memhold
postfile `memhold' byte wealth_group int event_time double b se using slopeplot_fe, replace

foreach g in 1 2 3 {

    * event time -2
    lincom c.amt_m2 ///
        + `=(`g'==2)'*(2.wealth_group#c.amt_m2) ///
        + `=(`g'==3)'*(3.wealth_group#c.amt_m2)
    post `memhold' (`g') (-2) (r(estimate)) (r(se))

    * event time -1 = omitted baseline
    post `memhold' (`g') (-1) (0) (0)

    * event time 0
    lincom c.amt_0 ///
        + `=(`g'==2)'*(2.wealth_group#c.amt_0) ///
        + `=(`g'==3)'*(3.wealth_group#c.amt_0)
    post `memhold' (`g') (0) (r(estimate)) (r(se))

    * event time 1
    lincom c.amt_1 ///
        + `=(`g'==2)'*(2.wealth_group#c.amt_1) ///
        + `=(`g'==3)'*(3.wealth_group#c.amt_1)
    post `memhold' (`g') (1) (r(estimate)) (r(se))

//    * event time 2
//    lincom c.amt_2 ///
//        + `=(`g'==2)'*(2.wealth_group#c.amt_2) ///
//        + `=(`g'==3)'*(3.wealth_group#c.amt_2)
//    post `memhold' (`g') (2) (r(estimate)) (r(se))
}

postclose `memhold'
use slopeplot_fe, clear

gen lb = b - 1.96*se
gen ub = b + 1.96*se

label define wg 1 "Low wealth" 2 "Middle wealth" 3 "High wealth"
label values wealth_group wg

sort event_time wealth_group

replace event_time = event_time - 0.05 if wealth_group==1
replace event_time = event_time + 0.05 if wealth_group==3

twoway ///
    (connect b event_time if wealth_group==1, color("215 48 39")) ///
    (rcap lb ub event_time if wealth_group==1, color("215 48 39")) ///
    (connect b event_time if wealth_group==2, color("254 224 139")) ///
    (rcap lb ub event_time if wealth_group==2, color("254 224 139")) ///
    (connect b event_time if wealth_group==3, color("26 152 80")) ///
    (rcap lb ub event_time if wealth_group==3, color("26 152 80")), ///
	name(DiD_contTr, replace) ///
    yline(0) xline(-0.5, lpattern(dash)) ///
    xlabel(-2(1)2) ///
    xtitle("Event time") ///
    ytitle("Additional effect per €1 inheritance relative to wave -1") ///
    legend(order(1 "Low wealth" 3 "Middle wealth" 5 "High wealth"))

graph export "$resultspath\Figures\DiD_contTr_FEs_m2to2_bywg3_2026_04_29.pdf", replace

restore 


*6.1.3 !!! Wealth quartile (by age group) 

reghdfe net_wealth c.(amt_m2 amt_0 amt_1 )##i.wealth_quar, absorb(root_hhid_num survey_year)  //Note: amt_2 dropped because very few observations

preserve

tempname memhold
postfile `memhold' byte wealth_quar int event_time double b se using slopeplot_fe, replace

foreach g in 1 2 3 4 {

    * event time -2
    lincom c.amt_m2 ///
        + `=(`g'==2)'*(2.wealth_quar#c.amt_m2) ///
        + `=(`g'==3)'*(3.wealth_quar#c.amt_m2) ///
        + `=(`g'==4)'*(4.wealth_quar#c.amt_m2)
    post `memhold' (`g') (-2) (r(estimate)) (r(se))

    * event time -1 = omitted baseline
    post `memhold' (`g') (-1) (0) (0)

    * event time 0
    lincom c.amt_0 ///
        + `=(`g'==2)'*(2.wealth_quar#c.amt_0) ///
        + `=(`g'==3)'*(3.wealth_quar#c.amt_0) ///
        + `=(`g'==4)'*(4.wealth_quar#c.amt_0)
    post `memhold' (`g') (0) (r(estimate)) (r(se))

    * event time 1
    lincom c.amt_1 ///
        + `=(`g'==2)'*(2.wealth_quar#c.amt_1) ///
        + `=(`g'==3)'*(3.wealth_quar#c.amt_1) ///
        + `=(`g'==4)'*(4.wealth_quar#c.amt_1)
    post `memhold' (`g') (1) (r(estimate)) (r(se))

//    * event time 2
//    lincom c.amt_2 ///
//        + `=(`g'==2)'*(2.wealth_quar#c.amt_2) ///
//        + `=(`g'==3)'*(3.wealth_quar#c.amt_2) ///
//        + `=(`g'==4)'*(4.wealth_quar#c.amt_2)
//    post `memhold' (`g') (2) (r(estimate)) (r(se))
}

postclose `memhold'
use slopeplot_fe, clear

gen lb = b - 1.96*se
gen ub = b + 1.96*se

label define wq ///
    1 "1st wealth quartile" ///
    2 "2nd wealth quartile" ///
    3 "3rd wealth quartile" ///
    4 "4th wealth quartile"

label values wealth_quar wq

sort event_time wealth_quar

replace event_time = event_time - 0.075 if wealth_quar==1
replace event_time = event_time - 0.025 if wealth_quar==2
replace event_time = event_time + 0.025 if wealth_quar==3
replace event_time = event_time + 0.075 if wealth_quar==4

twoway ///
    (connect b event_time if wealth_quar==1, color("215 48 39")) ///
    (rcap lb ub event_time if wealth_quar==1, color("215 48 39")) ///
    (connect b event_time if wealth_quar==2, color("254 224 139")) ///
    (rcap lb ub event_time if wealth_quar==2, color("254 224 139")) ///
    (connect b event_time if wealth_quar==3, color("166 217 106")) ///
    (rcap lb ub event_time if wealth_quar==3, color("166 217 106")) ///
    (connect b event_time if wealth_quar==4, color("26 152 80")) ///
    (rcap lb ub event_time if wealth_quar==4, color("26 152 80")), ///
    name(DiD_contTr, replace) ///
    yline(0) xline(-0.5, lpattern(dash)) ///
    xlabel(-2(1)2) ///
    xtitle("Event time") ///
    ytitle("Additional effect per €1 inheritance relative to wave -1") ///
    legend(order(1 "1st quartile" ///
                 3 "2nd quartile" ///
                 5 "3rd quartile" ///
                 7 "4th quartile"))

graph export "$resultspath\Figures\DiD_contTr_FEs_m2to2_bywq_2026_04_29.pdf", replace

restore







