
*ssc install eventstudyinteract
*ssc install avar

*set directory for whole dofile

global projectpath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling"   // [NEW] Main project folder
global datapath    "$projectpath\Data"       // [NEW] Folder containing input data
global resultspath "$projectpath\Results"    // [NEW] Folder where outputs will be saved


********************************************************************************
**#          		I. Cleaning and Variable Construction					  
********************************************************************************

import delimited "$datapath\staggered_did_data_2026_04_17.csv" , clear
//import delimited "$datapath\staggered_did_data_2026_04_16.csv" , clear


sort root_hhid survey_year



*** ID and wave variables

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

//tab num_waves
/*
  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |        323        0.73        0.73
          2 |     21,602       49.02       49.75
          3 |     14,070       31.93       81.68
          4 |      8,076       18.32      100.00
------------+-----------------------------------
      Total |     44,071      100.00
*/  
//br if num_waves==1
drop if num_waves==1 // drops n=323 individuals with only one wave

//Note: why do we have 326 singletons?



*** Inheritance variables

*Rename variables

rename treated post_inh_dummy

rename inheritance_year inh_year
rename inheritance_value inh_amount


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

duplicates report  root_hhid_num firstwave_prepost_inh_dummy if firstwave_prepost_inh_dummy==1
duplicates tag  root_hhid_num firstwave_prepost_inh_dummy if firstwave_prepost_inh_dummy==1 , gen(flag_pre_and_post_obs)





*Flag and drop those with only post-inheritance observations

by root_hhid: egen always_treated = min(post_inh_dummy) 

//keep if always_treated==1
//xtset root_hhid_num survey_year
//xtdescribe if always_treated==1 // n=2,984 individual-waves, n=1346 individuals without pre-inheritance observation if singletons dropped before (e.g. w1_801279_IT w1_809254_IT w3_17116886_FR)

drop if always_treated==1 // drops 2,984 individual-waves, n=1346 individuals


*Generate dummy for ever inherited

gen ever_inh_dummy = !missing(inh_year)

gen never_inh_dummy = ever_inh_dummy==0 


*Generate event_time in terms of survey waves

//Note: better to code event-time in survey waves rather than in years comapred to inheritance which causes problems in analysis because of unbalanced missings)

rename event_time event_time_year

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


*Generate inheritance amount that is not stable within individual but is 0 before inheritance

gen inh_amount_dyn=post_inh_dummy*inh_amount

//Note: net_wealth_pre is the wealth in the earliest year of obervation rather than in the year before inheritance which would be probably better for most uses


*Generate Inheritance groups

gen inh_group = .
replace inh_group = 0 if ever_inh_dummy==0
replace inh_group = 1 if ever_inh_dummy==1 & inh_amount>0     & inh_amount<20000
replace inh_group = 2 if ever_inh_dummy==1 & inh_amount>=20000 & inh_amount<100000
replace inh_group = 3 if ever_inh_dummy==1 & inh_amount>=100000

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

gen amt_m3 = inh_amount * (event_time_wave==-3) if ever_inh_dummy==1
replace amt_m3 = 0 if missing(amt_m3)

gen amt_m2 = inh_amount * (event_time_wave==-2) if ever_inh_dummy==1
replace amt_m2 = 0 if missing(amt_m2)

* omit -1

gen amt_0 = inh_amount * (event_time_wave==0) if ever_inh_dummy==1
replace amt_0 = 0 if missing(amt_0)

gen amt_1 = inh_amount * (event_time_wave==1) if ever_inh_dummy==1
replace amt_1 = 0 if missing(amt_1)

gen amt_2 = inh_amount * (event_time_wave==2) if ever_inh_dummy==1
replace amt_2 = 0 if missing(amt_2)



*** Wealth variables

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



*** Order dataset (and drop observations)

order  root_hhid_num survey_year survey_wave  inh_year event_time_wave event_time_wave_inclC event_time_year post_inh_dummy ever_inh_dummy  inh_amount inh_amount_dyn inh_group  net_wealth net_wealth_pre net_wealth_diff net_wealth_diff_inh


* Drop if inheritance but amount=0 //Note: this was actually people with amount==. -> now dropped in original dataset already

//drop if ever_inh_dummy==1 & inh_amount==0 //n=245 individual-waves

//Note: singletons (only one wave) and those with only post-inheritance observations are dropped above 




bysort root_hhid_num (survey_year): gen prev_year = survey_year[_n-1]
gen gap_years = survey_year - prev_year
gen nonconsecutive = !inlist(gap_years, 3, 4) if !missing(gap_years)
bysort root_hhid_num (survey_year): egen nonconsecutive_resp = max(nonconsecutive)
//br if nonconsecutive_resp==1
//br if nonconsecutive_resp==1 & ever_inh_dummy==1

replace event_time_wave=-2 if root_hhid=="w2_4832_DE" & survey_year==2014 //Note: only individual who jumps one wave and ever inherits so I code it manually like this. Should be done automatically in code that defines event_time_wave above




save "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , replace


xtset root_hhid_num survey_year
xtdescribe
//n=40,764 individual-waves, n=16,164 individuals




********************************************************************************
**#          				II. Descriptives 							  
********************************************************************************

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


*Individual-waves by inheritance year

tab inh_year , miss

/*
   inh_year |      Freq.     Percent        Cum.
------------+-----------------------------------
       2011 |        151        0.37        0.37
       2012 |        201        0.49        0.86
       2013 |        237        0.58        1.44
       2014 |        239        0.59        2.03
       2015 |        463        1.14        3.17
       2016 |        515        1.26        4.43
       2017 |        487        1.19        5.63
       2018 |        458        1.12        6.75
       2019 |        592        1.45        8.20
       2020 |        655        1.61        9.81
          . |     36,766       90.19      100.00
------------+-----------------------------------
      Total |     40,764      100.00
*/




use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear

collapse inh_amount net_wealth_diff_inh , by(root_hhid_num num_waves)

*Mean inheritance amount include those with 0 inheritance

sum inh_amount  //Note: alternatively do 

/*
    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
  inh_amount |     16,164    9761.675    108770.2          0   1.00e+07
*/


*Mean inheritance amount and net wealth difference from first pre- to first post-inheritance wave

sum inh_amount net_wealth_diff_inh if inh_amount>0 //Note: alternatively do 

/*
    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
  inh_amount |      1,486    106182.8    344274.6          1   1.00e+07
net_wealth~h |      1,486    68921.71     1582277  -5.34e+07    6977010
*/

/*
//Note: alterantive we could run:
use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear
keep if flag_pre_and_post_obs==1 
sum  inh_amount net_wealth_diff_inh
*/


*Histogram inheritance amount 

hist inh_amount  if inh_amount<1000000 & inh_amount>0





********************************************************************************
**#          	III. Regression Analysis - Wealth Trajectories						  
********************************************************************************

//Note: here we are not interested how wealth develops differently for those inheriting compared to those not inheriting but only in how wealth develops for those inheriting (and those not inheriting)

//We want wealth trajectory over time for each inheritance group/amount

//Do we need also interaction with age?


//Parents and children data should be different! In parent data we can have singletons and we can have those that are only observed after inheritance or only before (and maybe even those with inheritance amount unknown if we do not control for it)



******************************************************
**# 1. "Parents" (Inheritors) Wealth Trajectories
******************************************************

//Note: here we can use the full data including singleton observations, maybe even including those with inheritance amount missing if we do not model inheritance vs non-inheritance wealth trajectories 


**## Pooled Wealth Trajectories across Age

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear

drop if age < 50

reg net_wealth i.age 

margins , over(age)

marginsplot



**## Wealth Trajectories across Age by Wealth Group

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear

drop if age < 50

reg net_wealth i.age##i.wealth_group 

margins , over(age wealth_group)

marginsplot


// figure to compare without Confidence Intervals

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear

drop if age < 50

//drop if ever_inh_dummy==1

reg net_wealth i.age##i.wealth_group 

	
* Low
margins if wealth_group==1, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low") ///
    legend(off) ///
    name(g1_noci, replace)

* Middle
margins if wealth_group==2, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Middle") ///
    legend(off) ///
    name(g2_noci, replace)

* High
margins if wealth_group==3, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High") ///
    legend(off) ///
    name(g3_noci, replace)

* Combine
graph combine g1_noci g2_noci g3_noci, cols(3)





**## Wealth Trajectories across Age by Wealth Group with Controls

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear

keep if age >= 50

reg net_wealth i.age##i.wealth_group  ever_inh_dummy inh_amount 

margins , over(age wealth_group)

marginsplot








*************************************************
**# 2. "Children" (Inheritees) Wealth Trajectories
**************************************************

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


replace event_time_year=event_time_year+10

drop if event_time_wave==-3 //only n=58 (those with all 4 waves observed and inheritance between wave 3 and 4)
replace event_time_wave=event_time_wave+3


xtset root_hhid_num survey_year

xtdescribe



**## 2.0 Pre-Post for those inheriting only

//reg  net_wealth  i.post_inh_dummy   // i.survey_year   // , vce(cluster root_hhid_num)

//Note: reference group is those who inherit in periods before they inherit and those who never inherit in all periods


reg  net_wealth  i.post_inh_dummy if ever_inh_dummy==1 

//Note: reference group is those who inherit in periods before they inherit (excluding those who never inherit in all periods)


reg  net_wealth  i.post_inh_dummy if ever_inh_dummy==1 & firstwave_prepost_inh_dummy==1 

//Note: reference group is those who inherit in periods before they inherit (excluding those who never inherit in all periods) --> now average pre vs average post same as the average difference calculated above (Also sample size matches)



**## 2.1 Wealth trajectories

* For those inheriting

reg  net_wealth  ib2.event_time_wave  if ever_inh_dummy==1  //Note: this is always for ever_inh_dummy==1 because event_time_wave==. otherwise

margins, over(event_time_wave)
marginsplot, xline(2.5, lpattern(dash)) 


* For those not inheriting

reg  net_wealth  ib2.survey_wave  if ever_inh_dummy==0 

margins, over(survey_wave)
marginsplot


reg  net_wealth  ib2.survey_wave  if ever_inh_dummy==1  //just to check for those inheriting how it looks by survey_wave

margins, over(survey_wave)
marginsplot


reg  net_wealth  ib2.survey_wave##ever_inh_dummy  //just to check for those inheriting how it looks by survey_wave

margins, over(survey_wave ever_inh_dummy)
marginsplot


reg  inh_amount_dyn  ib2.survey_wave  if ever_inh_dummy==1  //to then check if the difference can be explained by the inheritance amount across survey_waves -> not fully so there is also more accumulation of those inheriting beyond inheritance (maybe because they are already richer at baseline and accumulate or maybe because inheritance is not spent but helps accumulating even more) 

margins, over(survey_wave)
marginsplot





/*

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


xtset root_hhid_num survey_year

xtdescribe


keep if ever_inh_dummy==1

replace event_time=event_time+10


reg net_wealth ib9.event_time##i.inh_group

margins net_wealth , over(event_time)
marginsplot, xline(9.5, lpattern(dash)) yline(0, lpattern(dash)) ///
    title("Marginal effect of inheritance amount by event time") ///
    xtitle("Event time") ytitle("d(Net wealth)/d(Inheritance amount)")




reghdfe net_wealth ib9.event_time##c.post_inh_dummy 

margins, dydx(post_inh_dummy) over(event_time)
marginsplot, xline(9.5, lpattern(dash)) yline(0, lpattern(dash)) ///
    title("Marginal effect of inheritance amount by event time") ///
    xtitle("Event time") ytitle("d(Net wealth)/d(Inheritance amount)")


*/





********************************************************************************
**#          				IV. Regression Analysis - DiD							  
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




use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


replace event_time_year=event_time_year+10


drop if event_time_wave==-3 //only n=58 (those with all 4 waves observed and inheritance between wave 3 and 4)

replace event_time_wave=event_time_wave+3


xtset root_hhid_num survey_year

xtdescribe


******************************************
**# 1. Pooled Sample -- Binary Treatment
******************************************

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


replace event_time_year=event_time_year+10


drop if event_time_wave==-3 //only n=58 (those with all 4 waves observed and inheritance between wave 3 and 4)

replace event_time_wave=event_time_wave+3
replace event_time_wave_inclC=event_time_wave_inclC+3 if ever_inh_dummy==1


xtset root_hhid_num survey_year

xtdescribe



**## 1a. Simple DiD without clean comparison just to fix idea

//reg  net_wealth  event_m3 event_m2  event_0 event_1 event_2
reg  net_wealth           event_m2  event_0 event_1 event_2 //Note: when running this I need to drop individual-waves (not full individual in all waves) three waves before inheritance (event_time_wave==-3)

//Note on Interpretation of Coefficients: 

//_cons: mean wealth of all individual-waves for which all dummies =0, i.e. the control group (never inherit) and the treatment group individual-waves of the omitted category (the wave before treatment)

//event_0: difference of treatment group (ever inherit) observed in event_0 (all rows with event_0=1, i.e. the treatment group in the wave after they are treated) vs all individual-waves for which all dummies =0, i.e. the control group (never inherit) and the treatment group individual-waves of the omitted category (the wave before treatment) (but not the treatment group individual-waves for which other dummies =1, i.e. e.g. those observations two waves after they are treated)

//event_1: difference of treatment group (ever inherit) observed in event_1 (all rows with event_1=1, i.e. the treatment group in the second wave after they are treated) vs all individual-waves for which all dummies =0, i.e. the control group (never inherit) and the treatment group individual-waves of the omitted category (the wave before treatment) (but not the treatment group individual-waves for which other dummies =1, i.e. e.g. those observations two waves before they are treated)



**## 1b. "Clean" comparison but no FEs

//reg  net_wealth  ever_inh_dummy  event_m3 event_m2  event_0 event_1 event_2
reg  net_wealth  ever_inh_dummy           event_m2  event_0 event_1 event_2 //Note: when running this I need to drop individual-waves (not full individual in all waves) three waves before inheritance (event_time_wave==-3)

reg  net_wealth  ever_inh_dummy  ib2.event_time_wave_inclC

//Note on Interpretation of Coefficients:  

//_cons: mean wealth of all individual-waves for which all dummies (including ever_inh_dummy) =0, i.e. the control group (never inherit) (but not the treatment group individual-waves of the omitted category (the wave before treatment))

//ever_inh_dummy: difference of treatment group (ever inherit) observed in omitted category (wave before treatment) (i.e. all rows with ever_inh_dummy=1 and other dummies=0, because if another dummy=1 the wealth will be the sum of _cons + ever_inh_dummy + that dummy coef) vs control group (never inheritors) (i.e. all dummies=0) 
//-> mean wealth of control group = _cons, so the mean wealth of treatment group observed in omitted category = _cons + ever_inh_dummy

//event_0: difference of treatment group (ever inherit) observed in event_0 (all rows with event_0=1, i.e. the treatment group in the wave after they are treated) vs treatment group in omitted category (observed in wave before treatment). That means, event_0 is the *additional* difference of treatment group (ever inherit) observed in event_0 vs control group (never inherit). This is exactly the pre-post comparison of treated individuals we got above (68921.71) 
//-> mean wealth of treatment group observed in wave after treatment = _cons + ever_inh_dummy + event_0


coefplot, keep(event_m3 event_m2 event_0 event_1 event_2) ///
    vertical yline(0) xline(1.5, lpattern(dash)) ///
    coeflabels(event_m3 = "-3" event_m2 = "-2" event_0 = "0" event_1 = "1" event_2 = "2") ///
    xtitle("Event time wave") ///
    ytitle("Tr vs. C difference relative to difference event time -1")

graph export "$resultspath\Figures\DiD_TrCdifference_relativetobaselinediff_m2to2_2026_04_17.pdf", replace

preserve

//reg  net_wealth  ever_inh_dummy  event_m3 event_m2  event_0 event_1 event_2
reg  net_wealth  ever_inh_dummy           event_m2  event_0 event_1 event_2

clear
input event_time str20 group mean
//-3 "Never inherit" .
-2 "Never inherit" .
-1 "Never inherit" .
0  "Never inherit" .
1  "Never inherit" .
2  "Never inherit" .
//-3 "Ever inherit" .
-2 "Ever inherit" .
-1 "Ever inherit" .
0  "Ever inherit" .
1  "Ever inherit" .
2  "Ever inherit" .
end

replace mean = _b[_cons] if group=="Never inherit"

//replace mean = _b[_cons] + _b[ever_inh_dummy] + _b[event_m3] if group=="Ever inherit" & event_time==-3
replace mean = _b[_cons] + _b[ever_inh_dummy] + _b[event_m2] if group=="Ever inherit" & event_time==-2
replace mean = _b[_cons] + _b[ever_inh_dummy]                if group=="Ever inherit" & event_time==-1
replace mean = _b[_cons] + _b[ever_inh_dummy] + _b[event_0]  if group=="Ever inherit" & event_time==0
replace mean = _b[_cons] + _b[ever_inh_dummy] + _b[event_1]  if group=="Ever inherit" & event_time==1
replace mean = _b[_cons] + _b[ever_inh_dummy] + _b[event_2]  if group=="Ever inherit" & event_time==2

twoway ///
    (line mean event_time if group=="Never inherit", sort) ///
    (line mean event_time if group=="Ever inherit", sort), ///
    xline(-0.5, lpattern(dash)) ///
    xtitle("Event time wave") ///
    ytitle("Predicted net wealth") ///
    legend(order(1 "Never inherit" 2 "Ever inherit"))

graph export "$resultspath\Figures\DiD_predTrandC_m2to2_2026_04_17.pdf", replace

restore

**## 1c. "Clean" comparison incl survey wave dummies but no individual FEs

//reg  net_wealth  ever_inh_dummy  event_m3 event_m2  event_0 event_1 event_2  i.survey_year
reg  net_wealth  ever_inh_dummy           event_m2  event_0 event_1 event_2  i.survey_year

coefplot, keep(event_m3 event_m2 event_0 event_1 event_2) ///
    vertical yline(0) xline(1.5, lpattern(dash)) ///
    coeflabels(event_m3 = "-3" event_m2 = "-2" event_0 = "0" event_1 = "1" event_2 = "2") ///
    xtitle("Event time wave") ///
    ytitle("Tr vs. C difference relative to difference event time -1")

graph export "$resultspath\Figures\DiD_TrCdifference_relativetobaselinediff_inclsurveyyeardummies_2026_04_17.pdf", replace



**## 1d. Fixed-Effects DiD


reghdfe  net_wealth  event_m3 event_m2 event_0 event_1 event_2, absorb(root_hhid_num survey_year)   // vce(cluster root_hhid_num)
reghdfe  net_wealth           event_m2 event_0 event_1 event_2, absorb(root_hhid_num survey_year)   // vce(cluster root_hhid_num) //Note: when running this I need to drop individual-waves (not full individual in all waves) three waves before inheritance (event_time_wave==-3)

//reghdfe  net_wealth  event_m3 event_m2 event_0 event_1 event_2, absorb(root_hhid_num)     // vce(cluster root_hhid_num)
//reghdfe  net_wealth  event_m3 event_m2 event_0 event_1 event_2  if ever_inh_dummy==1 , absorb(root_hhid_num)     // vce(cluster root_hhid_num)


coefplot, keep(event_m3 event_m2 event_0 event_1 event_2) ///
    vertical yline(0) xline(1.5, lpattern(dash)) ///
    coeflabels(event_m3 = "-3" event_m2 = "-2" event_0 = "0" event_1 = "1" event_2 = "2") ///
    xtitle("Event time wave") ///
    ytitle("Tr vs. C difference relative to difference event time -1")

graph export "$resultspath\Figures\DiD_TrCdifference_relativetobaselinediff_inclFEs_2026_04_17.pdf", replace



//Note on Interpretation of Coefficients:  

//event_0: within treated people, how wealth at the first post-inheritance wave differs from -1, net of survey-year effects

//Note: never-inheritors are not compared through an explicit ever_inh_dummy anymore. Instead they help define the common time pattern that gets absorbed by absorb(survey_year).

//-> does a person's wealth move differently around inheritance than would be expected from common survey-wave movements? (rather than are inheritors richer than non-inheritors?)



**## 1e. Staggered DiD package

eventstudyinteract net_wealth     event_m2 event_0 event_1 event_2 , ///
    cohort(cohort_wave) ///
    control_cohort(never_inh_dummy) ///
    absorb(i.root_hhid_num i.survey_year) ///
    vce(cluster root_hhid_num)

* Plot
matrix B = e(b_iw)
mata st_matrix("SE", sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = B \ SE'

coefplot matrix(C[1]), se(C[2]) vertical ///
    yline(0) ///
    xline(1.5, lpattern(dash)) ///
    coeflabels(rel_m3 = "-3" rel_m2 = "-2" rel_0 = "0" rel_1 = "1" rel_2 = "2") ///
    xtitle("Event time (waves)") ///
    ytitle("Effect on net wealth relative to wave -1") ///
    msymbol(O) ciopts(recast(rcap))

//Note: very small differences to 1d.



******************************************
**# 2. Pooled Sample -- Treatment Inheritance Groups
******************************************

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


replace event_time_year=event_time_year+10


drop if event_time_wave==-3 //only n=58 (those with all 4 waves observed and inheritance between wave 3 and 4)

replace event_time_wave=event_time_wave+3


xtset root_hhid_num survey_year

xtdescribe



**## 2a. Simple DiD without clean comparison just to fix idea


reg net_wealth ///
    event_sm_m2 event_sm_0 event_sm_1 event_sm_2 ///
    event_md_m2 event_md_0 event_md_1 event_md_2 ///
    event_lg_m2 event_lg_0 event_lg_1 event_lg_2 



**## 2b. "Clean" comparison but no FEs

preserve

reg net_wealth ///
    i.inh_group ///
    event_sm_m2 event_sm_0 event_sm_1 event_sm_2 ///
    event_md_m2 event_md_0 event_md_1 event_md_2 ///
    event_lg_m2 event_lg_0 event_lg_1 event_lg_2 
	
//Note on Interpretation of Coefficients:  

//_cons: Mean wealth of the none inh group.

//inh_group: Difference between inheritance group at event time -1 and no-inheritance group

//event_sm_0: additional difference of the small treatment group in event time 0 vs the control group (never inherit) compared to the difference of the small treatment group vs the control group -> difference of the small treatment group in -1 vs the small treatment group in 0


coefplot, ///
    keep(event_sm_m2 event_sm_0 event_sm_1 event_sm_2 ///
         event_md_m2 event_md_0 event_md_1 event_md_2 ///
         event_lg_m2 event_lg_0 event_lg_1 event_lg_2) ///
    vertical ///
    yline(0) ///
    coeflabels( ///
        event_sm_m2 = "S:-2" event_sm_0 = "S:0" event_sm_1 = "S:1" event_sm_2 = "S:2" ///
        event_md_m2 = "M:-2" event_md_0 = "M:0" event_md_1 = "M:1" event_md_2 = "M:2" ///
        event_lg_m2 = "L:-2" event_lg_0 = "L:0" event_lg_1 = "L:1" event_lg_2 = "L:2" ///
    ) ///
    xtitle("Group and event time") ///
    ytitle("Deviation from group baseline at wave -1")

//Note: should be plotted better. At best with different colors by group but same x-axis except a small offset


tempname memhold
postfile `memhold' str12 group event_time b se using plot_groups, replace

* none group
lincom _cons
post `memhold' ("None") (-2) (r(estimate)) (r(se))
post `memhold' ("None") (-1) (r(estimate)) (r(se))
post `memhold' ("None") (0)  (r(estimate)) (r(se))
post `memhold' ("None") (1)  (r(estimate)) (r(se))
post `memhold' ("None") (2)  (r(estimate)) (r(se))

* small group
lincom _cons + 1.inh_group + event_sm_m2
post `memhold' ("Small") (-2) (r(estimate)) (r(se))

lincom _cons + 1.inh_group
post `memhold' ("Small") (-1) (r(estimate)) (r(se))

lincom _cons + 1.inh_group + event_sm_0
post `memhold' ("Small") (0) (r(estimate)) (r(se))

lincom _cons + 1.inh_group + event_sm_1
post `memhold' ("Small") (1) (r(estimate)) (r(se))

lincom _cons + 1.inh_group + event_sm_2
post `memhold' ("Small") (2) (r(estimate)) (r(se))

* medium group
lincom _cons + 2.inh_group + event_md_m2
post `memhold' ("Medium") (-2) (r(estimate)) (r(se))

lincom _cons + 2.inh_group
post `memhold' ("Medium") (-1) (r(estimate)) (r(se))

lincom _cons + 2.inh_group + event_md_0
post `memhold' ("Medium") (0) (r(estimate)) (r(se))

lincom _cons + 2.inh_group + event_md_1
post `memhold' ("Medium") (1) (r(estimate)) (r(se))

lincom _cons + 2.inh_group + event_md_2
post `memhold' ("Medium") (2) (r(estimate)) (r(se))

* large group
lincom _cons + 3.inh_group + event_lg_m2
post `memhold' ("Large") (-2) (r(estimate)) (r(se))

lincom _cons + 3.inh_group
post `memhold' ("Large") (-1) (r(estimate)) (r(se))

lincom _cons + 3.inh_group + event_lg_0
post `memhold' ("Large") (0) (r(estimate)) (r(se))

lincom _cons + 3.inh_group + event_lg_1
post `memhold' ("Large") (1) (r(estimate)) (r(se))

lincom _cons + 3.inh_group + event_lg_2
post `memhold' ("Large") (2) (r(estimate)) (r(se))

postclose `memhold'
use plot_groups, clear

gen lb = b - 1.96*se
gen ub = b + 1.96*se

twoway ///
    (rcap lb ub event_time if group=="None") ///
    (line b event_time if group=="None", sort) ///
    (scatter b event_time if group=="None") ///
    (rcap lb ub event_time if group=="Small") ///
    (line b event_time if group=="Small", sort) ///
    (scatter b event_time if group=="Small") ///
    (rcap lb ub event_time if group=="Medium") ///
    (line b event_time if group=="Medium", sort) ///
    (scatter b event_time if group=="Medium") ///
    (rcap lb ub event_time if group=="Large") ///
    (line b event_time if group=="Large", sort) ///
    (scatter b event_time if group=="Large"), ///
    xline(-0.5, lpattern(dash)) ///
    xlabel(-2(1)2) ///
    xtitle("Event time (waves)") ///
    ytitle("Predicted net wealth") ///
    legend(order(2 "None" 5 "Small" 8 "Medium" 11 "Large"))
	

restore

**## 2c. "Clean" comparison incl survey wave dummies but no individual FEs

reg net_wealth ///
    i.inh_group ///
    event_sm_m2 event_sm_0 event_sm_1 event_sm_2 ///
    event_md_m2 event_md_0 event_md_1 event_md_2 ///
    event_lg_m2 event_lg_0 event_lg_1 event_lg_2 ///
    i.survey_year

coefplot, ///
    keep(event_sm_m2 event_sm_0 event_sm_1 event_sm_2 ///
         event_md_m2 event_md_0 event_md_1 event_md_2 ///
         event_lg_m2 event_lg_0 event_lg_1 event_lg_2) ///
    vertical ///
    yline(0) ///
    coeflabels( ///
        event_sm_m2 = "S:-2" event_sm_0 = "S:0" event_sm_1 = "S:1" event_sm_2 = "S:2" ///
        event_md_m2 = "M:-2" event_md_0 = "M:0" event_md_1 = "M:1" event_md_2 = "M:2" ///
        event_lg_m2 = "L:-2" event_lg_0 = "L:0" event_lg_1 = "L:1" event_lg_2 = "L:2" ///
    ) ///
    xtitle("Group and event time") ///
    ytitle("Deviation from group baseline at wave -1")

//Note: should be plotted better. At best with different colors by group but same x-axis except a small offset


**## 2d. Fixed-Effects DiD

reghdfe net_wealth ///
    i.inh_group ///
    event_sm_m2 event_sm_0 event_sm_1 event_sm_2 ///
    event_md_m2 event_md_0 event_md_1 event_md_2 ///
    event_lg_m2 event_lg_0 event_lg_1 event_lg_2 , ///
    absorb(root_hhid_num survey_year) ///
    vce(cluster root_hhid_num)

coefplot, ///
    keep(event_sm_m2 event_sm_0 event_sm_1 event_sm_2 ///
         event_md_m2 event_md_0 event_md_1 event_md_2 ///
         event_lg_m2 event_lg_0 event_lg_1 event_lg_2) ///
    vertical ///
    yline(0) ///
    coeflabels( ///
        event_sm_m2 = "S:-2" event_sm_0 = "S:0" event_sm_1 = "S:1" event_sm_2 = "S:2" ///
        event_md_m2 = "M:-2" event_md_0 = "M:0" event_md_1 = "M:1" event_md_2 = "M:2" ///
        event_lg_m2 = "L:-2" event_lg_0 = "L:0" event_lg_1 = "L:1" event_lg_2 = "L:2" ///
    ) ///
    xtitle("Group and event time") ///
    ytitle("Deviation from group baseline at wave -1")

//Note: should be plotted better. At best with different colors by group but same x-axis except a small offset

test event_sm_0 = event_md_0 = event_lg_0
test event_sm_1 = event_md_1 = event_lg_1
test event_sm_2 = event_md_2 = event_lg_2


**## 2e. Staggered DiD package

//Note: would need to be done separately by inheritance group. But difference were anyway small so stick with 2d



******************************************
**# 3. Pooled Sample -- Continuous Treatment (Inheritance Amount)
******************************************

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


replace event_time_year=event_time_year+10


drop if event_time_wave==-3 //only n=58 (those with all 4 waves observed and inheritance between wave 3 and 4)

replace event_time_wave=event_time_wave+3


xtset root_hhid_num survey_year

xtdescribe



**## 3a. Simple DiD without clean comparison just to fix idea

reg net_wealth amt_m2 amt_0 amt_1 amt_2



**## 3b. "Clean" comparison but no FEs

reg net_wealth c.inh_amount amt_m2 amt_0 amt_1 amt_2

//Interpretation
//_cons = mean wealth of never-inheritors

//inh_amount = baseline slope at event time -1: //how much higher wealth is, at -1, for each additional €1 of inheritance amount, relative to never-inheritors

//amt_0 = additional slope at event time 0 relative to -1
//amt_1 = additional slope at event time 1 relative to -1
//amt_2 = additional slope at event time 2 relative to -1

coefplot, keep(amt_m2 amt_0 amt_1 amt_2) ///
    vertical yline(0) xline(1.5, lpattern(dash)) ///
    coeflabels(event_m3 = "-3" event_m2 = "-2" event_0 = "0" event_1 = "1" event_2 = "2") ///
    xtitle("Event time wave") ///
    ytitle("Tr vs. C difference relative to difference event time -1")



**## 3c. "Clean" comparison incl survey wave dummies but no individual FEs

reg net_wealth c.inh_amount amt_m2 amt_0 amt_1 amt_2 i.survey_year

coefplot, keep(amt_m2 amt_0 amt_1 amt_2) ///
    vertical yline(0) xline(1.5, lpattern(dash)) ///
    coeflabels(event_m3 = "-3" event_m2 = "-2" event_0 = "0" event_1 = "1" event_2 = "2") ///
    xtitle("Event time wave") ///
    ytitle("Tr vs. C difference relative to difference event time -1")



**## 3d. Fixed-Effects DiD !!!

reghdfe net_wealth amt_m2 amt_0 amt_1 amt_2, absorb(root_hhid_num survey_year) 

coefplot, keep(amt_m2 amt_0 amt_1 amt_2) ///
    vertical yline(0) xline(1.5, lpattern(dash)) ///
    coeflabels(event_m3 = "-3" event_m2 = "-2" event_0 = "0" event_1 = "1" event_2 = "2") ///
    xtitle("Event time wave") ///
    ytitle("Tr vs. C difference relative to difference event time -1")


//Note: maybe do this with log amount instead




****************************
**# 4. By Wealth group - Binary Treatment
****************************

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


replace event_time_year=event_time_year+10


drop if event_time_wave==-3 //only n=58 (those with all 4 waves observed and inheritance between wave 3 and 4)

replace event_time_wave=event_time_wave+3


xtset root_hhid_num survey_year

xtdescribe



**## 1a. Simple DiD without clean comparison just to fix idea

//reg  net_wealth  event_m3 event_m2  event_0 event_1 event_2
reg  net_wealth           (event_m2  event_0 event_1 event_2)##i.wealth_group   //Note: when running this I need to drop individual-waves (not full individual in all waves) three waves before inheritance (event_time_wave==-3)



**## 1b. "Clean" comparison but no FEs

//reg  net_wealth  ever_inh_dummy  event_m3 event_m2  event_0 event_1 event_2
reg  net_wealth  (ever_inh_dummy           event_m2  event_0 event_1 event_2)##i.wealth_group //Note: when running this I need to drop individual-waves (not full individual in all waves) three waves before inheritance (event_time_wave==-3)

//Note on Interpretation of Coefficients:  

//_cons: mean wealth of all individual-waves for which all dummies (including ever_inh_dummy) =0, i.e. the control group (never inherit) (but not the treatment group individual-waves of the omitted category (the wave before treatment))

//ever_inh_dummy: difference of treatment group (ever inherit) observed in omitted category (wave before treatment) (i.e. all rows with ever_inh_dummy=1 and other dummies=0, because if another dummy=1 the wealth will be the sum of _cons + ever_inh_dummy + that dummy coef) vs control group (never inheritors) (i.e. all dummies=0) 
//-> mean wealth of control group = _cons, so the mean wealth of treatment group observed in omitted category = _cons + ever_inh_dummy

//event_0: difference of treatment group (ever inherit) observed in event_0 (all rows with event_0=1, i.e. the treatment group in the wave after they are treated) vs treatment group in omitted category (observed in wave before treatment). That means, event_0 is the *additional* difference of treatment group (ever inherit) observed in event_0 vs control group (never inherit). This is exactly the pre-post comparison of treated individuals we got above (68921.71) 
//-> mean wealth of treatment group observed in wave after treatment = _cons + ever_inh_dummy + event_0



//graph export "$resultspath\Figures\DiD_TrCdifference_relativetobaselinediff_m2to2_2026_04_17.pdf", replace



...


****************************
**# 5. By Wealth group - Treatment Groups
****************************

...






****************************
**# 6. By Wealth group - Continuous Treatment
****************************


use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


replace event_time_year=event_time_year+10


drop if event_time_wave==-3 //only n=58 (those with all 4 waves observed and inheritance between wave 3 and 4)

replace event_time_wave=event_time_wave+3


xtset root_hhid_num survey_year

xtdescribe



**## 6a. Simple DiD without clean comparison just to fix idea

reg net_wealth c.(amt_m2 amt_0 amt_1 amt_2)##i.wealth_group 



**## 6b. "Clean" comparison but no FEs

preserve

reg net_wealth c.(inh_amount amt_m2 amt_0 amt_1 amt_2)##i.wealth_group 


tempname memhold
postfile `memhold' byte wealth_group int event_time double b se using slopeplot, replace

foreach g in 1 2 3 {
	
	* event time -2
    lincom c.inh_amount + c.amt_m2 ///
        + `=(`g'==2)'*(2.wealth_group#c.inh_amount + 2.wealth_group#c.amt_m2) ///
        + `=(`g'==3)'*(3.wealth_group#c.inh_amount + 3.wealth_group#c.amt_m2)
    post `memhold' (`g') (-2) (r(estimate)) (r(se))
	
    * event time -1
    lincom c.inh_amount + `=(`g'==2)'*2.wealth_group#c.inh_amount + `=(`g'==3)'*3.wealth_group#c.inh_amount
    post `memhold' (`g') (-1) (r(estimate)) (r(se))

    * event time 0
    lincom c.inh_amount + c.amt_0 ///
        + `=(`g'==2)'*(2.wealth_group#c.inh_amount + 2.wealth_group#c.amt_0) ///
        + `=(`g'==3)'*(3.wealth_group#c.inh_amount + 3.wealth_group#c.amt_0)
    post `memhold' (`g') (0) (r(estimate)) (r(se))

    * event time 1
    lincom c.inh_amount + c.amt_1 ///
        + `=(`g'==2)'*(2.wealth_group#c.inh_amount + 2.wealth_group#c.amt_1) ///
        + `=(`g'==3)'*(3.wealth_group#c.inh_amount + 3.wealth_group#c.amt_1)
    post `memhold' (`g') (1) (r(estimate)) (r(se))

    * event time 2
    lincom c.inh_amount + c.amt_2 ///
        + `=(`g'==2)'*(2.wealth_group#c.inh_amount + 2.wealth_group#c.amt_2) ///
        + `=(`g'==3)'*(3.wealth_group#c.inh_amount + 3.wealth_group#c.amt_2)
    post `memhold' (`g') (2) (r(estimate)) (r(se))
}

postclose `memhold'
use slopeplot, clear

gen lb = b - 1.96*se
gen ub = b + 1.96*se

label define wg 1 "Low wealth" 2 "Middle wealth" 3 "High wealth"
label values wealth_group wg

sort event_time wealth_group


replace event_time = event_time-0.05 if wealth_group==1
replace event_time = event_time+0.05 if wealth_group==3

twoway ///
    (connect b event_time if wealth_group==1 , color("215 48 39") ) ///
    (rcap lb ub event_time if wealth_group==1 , color("215 48 39") ) ///
    (connect b event_time if wealth_group==2 , color("254 224 139") ) ///
    (rcap lb ub event_time if wealth_group==2 , color("254 224 139") ) ///
    (connect b event_time if wealth_group==3 , color("26 152 80") ) ///
    (rcap lb ub event_time if wealth_group==3 , color("26 152 80") ) , ///
    yline(0) xline(-0.5, lpattern(dash)) ///
    xlabel(-2(1)2) ///
    xtitle("Event time") ///
    ytitle("Effect per additional €1 inheritance") ///
    legend(order(1 "Low wealth" 3 "Middle wealth" 5 "High wealth"))

/*
margins wealth_group, at(inh_amount=(10000 20000 50000 100000 200000 400000 1000000))
marginsplot, xdimension(inh_amount) ///
    xtitle("Inheritance amount (€10,000)") ///
    ytitle("Predicted net wealth")
*/

restore

**## 6c. "Clean" comparison incl survey wave dummies but no individual FEs

reg net_wealth c.(inh_amount amt_m2 amt_0 amt_1 amt_2)##i.wealth_group i.survey_year




**## 6d. Fixed-Effects DiD !!!

reghdfe net_wealth c.(amt_m2 amt_0 amt_1 amt_2)##i.wealth_group , absorb(root_hhid_num survey_year) 


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

    * event time 2
    lincom c.amt_2 ///
        + `=(`g'==2)'*(2.wealth_group#c.amt_2) ///
        + `=(`g'==3)'*(3.wealth_group#c.amt_2)
    post `memhold' (`g') (2) (r(estimate)) (r(se))
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
    yline(0) xline(-0.5, lpattern(dash)) ///
    xlabel(-2(1)2) ///
    xtitle("Event time") ///
    ytitle("Additional effect per €1 inheritance relative to wave -1") ///
    legend(order(1 "Low wealth" 3 "Middle wealth" 5 "High wealth"))






/*



**# 3a

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


xtset root_hhid_num survey_year

xtdescribe


replace event_time=event_time+10

reghdfe net_wealth ib9.event_time##c.post_inh_dummy  if ever_inh_dummy==1 & firstwave_prepost_inh_dummy==1 & flag_pre_and_post_obs==1 , absorb(root_hhid_num survey_year) vce(cluster root_hhid_num) 

margins, dydx(post_inh_dummy) over(event_time)
marginsplot, xline(9.5, lpattern(dash)) yline(0, lpattern(dash)) ///
    title("Marginal effect of inheritance amount by event time") ///
    xtitle("Event time") ytitle("d(Net wealth)/d(Inheritance amount)")






**# 3c

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


xtset root_hhid_num survey_year

xtdescribe


replace event_time=event_time+10

reghdfe net_wealth ib9.event_time##c.inh_amount, absorb(root_hhid_num survey_year) // vce(cluster root_hhid_num)

margins, dydx(inh_amount) over(event_time)
marginsplot, xline(9.5, lpattern(dash)) yline(0, lpattern(dash)) ///
    title("Marginal effect of inheritance amount by event time") ///
    xtitle("Event time") ytitle("d(Net wealth)/d(Inheritance amount)")






use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


xtset root_hhid_num survey_year

xtdescribe


replace event_time=event_time+10

reghdfe net_wealth ib9.event_time##c.inh_amount##i.inh_group  , absorb(root_hhid_num survey_year) vce(cluster root_hhid_num)

margins, dydx(inh_amount) over(event_time inh_group)
marginsplot, xline(9.5, lpattern(dash)) yline(0, lpattern(dash)) ///
    name(bygroup, replace) title("Marginal effect of inheritance amount by event time") ///
    xtitle("Event time") ytitle("d(Net wealth)/d(Inheritance amount)")





//What parameters we need for the ABM:

spending behavior for people that inherit


*/







