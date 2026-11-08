

*set directory for whole dofile

global datapath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling\Data"
global resultspath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling\Results"


********************************************************************************
**#          		I. Cleaning and Variable Construction					  
********************************************************************************

import delimited "$datapath\staggered_did_data_2026_04_17.csv" , clear
//import delimited "$datapath\staggered_did_data_2026_04_16.csv" , clear


// sorting by 
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

drop if always_treated==1 // drops 2,984 individual-waves, n=... individuals


*Generate dummy for ever inherited

gen ever_inh_dummy = !missing(inh_year)


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

//replace event_time_wave=0 if ever_inh_dummy==0


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


*Generate Inh-group-specific treatment switches on after inheritance

gen post_inh_dummy_small  = post_inh_dummy * (inh_group==1)
gen post_inh_dummy_medium = post_inh_dummy * (inh_group==2)
gen post_inh_dummy_large  = post_inh_dummy * (inh_group==3)



*** Wealth variables

*Define wealth difference to year before and wealth difference to year before when inherited 

sort root_hhid_num survey_year

bysort root_hhid_num : gen net_wealth_diff = net_wealth[_n]-net_wealth[_n-1]

gen net_wealth_diff_inh = net_wealth_diff if post_inh_dummy==1
replace  net_wealth_diff_inh = . if firstwave_post_inh_dummy!=1



*** Order dataset (and drop observations)

order  root_hhid_num survey_year survey_wave  inh_year event_time_wave event_time_year post_inh_dummy ever_inh_dummy  inh_amount inh_amount_dyn inh_group  net_wealth net_wealth_pre net_wealth_diff net_wealth_diff_inh


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


****************************************************************************************************************************************************************

**#          	A) Inheritance Modelling							  
****************************************************************************************************************************************************************



********************************************************************************
**##          	III. Inheritance - Regression Analysis - Look at those inheriting only							  
********************************************************************************

//Note: here we are not interested how wealth develops differently for those inheriting compared to those not inheriting but only in how wealth develops for those inheriting (and those not inheriting)

//We want wealth trajectory over time for each inheritance group/amount

//Do we need also interaction with age?



use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


replace event_time_year=event_time_year+10

drop if event_time_wave==-3 //only n=58 (those with all 4 waves observed and inheritance between wave 3 and 4)
replace event_time_wave=event_time_wave+3


xtset root_hhid_num survey_year

xtdescribe



**### 0. Pre-Post for those inheriting only

//reg  net_wealth  i.post_inh_dummy   // i.survey_year   // , vce(cluster root_hhid_num)

//Note: reference group is those who inherit in periods before they inherit and those who never inherit in all periods


// compare impact of ever inheriting on net wealth (only those who inherit)

reg  net_wealth  i.post_inh_dummy if ever_inh_dummy==1 

//Note: reference group is those who inherit in periods before they inherit (excluding those who never inherit in all periods)

// impact of inheriting immediately after 

reg  net_wealth  i.post_inh_dummy if ever_inh_dummy==1 & firstwave_prepost_inh_dummy==1 

//Note: reference group is those who inherit in periods before they inherit (excluding those who never inherit in all periods) --> now average pre vs average post same as the average difference calculated above (Also sample size matches)


// including more controls

reg  net_wealth  i.post_inh_dummy education_level c.age##c.age if ever_inh_dummy==1 


reg  net_wealth  i.post_inh_dummy education_level c.age##c.age if ever_inh_dummy==1 & firstwave_prepost_inh_dummy==1 




**### 1. Wealth trajectories

* For those inheriting DONE

reg  net_wealth  ib2.event_time_wave //Note: this is always for ever_inh_dummy==1 because event_time_wave==. otherwise

margins, over(event_time_wave)
marginsplot, xline(2.5, lpattern(dash)) 


* For those not inheriting DONE

reg  net_wealth  ib2.survey_wave  if ever_inh_dummy==0 

margins, over(survey_wave)
marginsplot


reg  net_wealth  ib2.survey_wave  if ever_inh_dummy==1  //just to check for those inheriting how it looks by survey_wave DONE

margins, over(survey_wave)
marginsplot


reg  net_wealth  ib2.survey_wave##ever_inh_dummy  //just to check for those inheriting how it looks by survey_wave DONE

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
**##          				IV. Regression Analysis - DiD							  
********************************************************************************

use "$datapath\staggered_did_data_cleaned_2026_04_17.dta" , clear


replace event_time_year=event_time_year+10

drop if event_time_wave==-3 //only n=58 (those with all 4 waves observed and inheritance between wave 3 and 4)
replace event_time_wave=event_time_wave+3


xtset root_hhid_num survey_year

xtdescribe


*### 1. Simple DiD without FEs


**### 1a. Simple DiD without clean comparison just to fix idea

reg  net_wealth  event_m3 event_m2  event_0 event_1 event_2

//Note: interpretation for e.g. event_0: difference of treatment group treated in event_0 vs control group (never inherit) and treatment group who is treated in other wave


**### 1b. "Clean" comparison but no FEs

//reg  net_wealth  ever_inh_dummy  event_m3 event_m2  event_0 event_1 event_2
reg  net_wealth  ever_inh_dummy           event_m2  event_0 event_1 event_2

//Note: 
//_cons = Mean wealth of never-inheritors
//ever_inh_dummy = difference between treated units at -1 and control units (never-inheritors)
//event_0 = *additional* difference between treated and control units at event time 0 compared to event time -1 -> exactly the pre-post comparison of treated individuals we got above (68921.71)


coefplot, keep(event_m3 event_m2 event_0 event_1 event_2) ///
    vertical yline(0) xline(1.5, lpattern(dash)) ///
    coeflabels(event_m3 = "-3" event_m2 = "-2" event_0 = "0" event_1 = "1" event_2 = "2") ///
    xtitle("Event time wave") ///
    ytitle("Tr vs. C difference relative to difference event time -1")



//reg  net_wealth  ever_inh_dummy  event_m3 event_m2  event_0 event_1 event_2
reg net_wealth ever_inh_dummy event_m2 event_0 event_1 event_2

preserve

clear
input event_time str20 group mean
-2 "Never inherit" .
-1 "Never inherit" .
0  "Never inherit" .
1  "Never inherit" .
2  "Never inherit" .
-2 "Ever inherit" .
-1 "Ever inherit" .
0  "Ever inherit" .
1  "Ever inherit" .
2  "Ever inherit" .
end

replace mean = _b[_cons] if group=="Never inherit"
replace mean = _b[_cons] + _b[ever_inh_dummy] + _b[event_m2] if group=="Ever inherit" & event_time==-2
replace mean = _b[_cons] + _b[ever_inh_dummy]                if group=="Ever inherit" & event_time==-1
replace mean = _b[_cons] + _b[ever_inh_dummy] + _b[event_0] if group=="Ever inherit" & event_time==0
replace mean = _b[_cons] + _b[ever_inh_dummy] + _b[event_1] if group=="Ever inherit" & event_time==1
replace mean = _b[_cons] + _b[ever_inh_dummy] + _b[event_2] if group=="Ever inherit" & event_time==2

twoway ///
    (line mean event_time if group=="Never inherit", sort) ///
    (line mean event_time if group=="Ever inherit", sort), ///
    xline(-0.5, lpattern(dash)) ///
    xtitle("Event time wave") ///
    ytitle("Predicted net wealth") ///
    legend(order(1 "Never inherit" 2 "Ever inherit"))

restore

//reg  net_wealth  ever_inh_dummy  event_m3 event_m2  event_0 event_1 event_2  i.survey_year
reg  net_wealth  ever_inh_dummy           event_m2  event_0 event_1 event_2  i.survey_year

coefplot, keep(event_m3 event_m2 event_0 event_1 event_2) ///
    vertical yline(0) xline(1.5, lpattern(dash)) ///
    coeflabels(event_m3 = "-3" event_m2 = "-2" event_0 = "0" event_1 = "1" event_2 = "2") ///
    xtitle("Event time wave") ///
    ytitle("Tr vs. C difference relative to difference event time -1")



**### 2. Fixed-Effects DiD


reghdfe net_wealth event_m3 event_m2 event_0 event_1 event_2, absorb(root_hhid_num survey_year)   // vce(cluster root_hhid_num)

reghdfe net_wealth          event_m2 event_0 event_1 event_2, absorb(root_hhid_num survey_year)   // vce(cluster root_hhid_num)

//Note: within-individual differences (net of common survey-year shocks)






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


********************************************************************************
********************************************************************************
**#          	B) Wealth Age Dynamics Modelling							  
********************************************************************************
********************************************************************************





////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
**##          	V. Wealth Age Dynamics - Running the regression (by age)
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**###          	0. preparing the data
////////////////////////////////////////////////////////////////////////////////


preserve

*--------------------------------------------------*
* 1. Create wealth group based on first observed wave
*--------------------------------------------------*
sort root_hhid_num survey_year

* wealth in first observed survey
by root_hhid_num: gen first_wealth = net_wealth[1]
by root_hhid_num: gen first_survey_year = survey_year[1]

* assign wealth group from first observed wealth
gen wealth_group = .
replace wealth_group = 1 if first_wealth < 100000
replace wealth_group = 2 if first_wealth >= 100000 & first_wealth <= 300000
replace wealth_group = 3 if first_wealth > 300000

label define wg 1 "Low (<100k)" 2 "Middle (100k-300k)" 3 "High (>300k)", replace
label values wealth_group wg

* check
tab wealth_group
tab wealth_group first_survey_year

browse root_hhid_num survey_year net_wealth wealth_group


*--------------------------------------------------*
* 2. Create wealth group based on first observed wave
*--------------------------------------------------*

* count number of observations per individual
bysort root_hhid_num: gen n_obs = _N

* drop singletons
drop if n_obs == 1

drop n_obs

bys root_hhid_num: assert _N > 1


////////////////////////////////////////////////////////////////////////////////
**###          	1. Regression by age
////////////////////////////////////////////////////////////////////////////////


* keep 50+
keep if age >= 50


* regression with age dummies interacted with wealth group
reg net_wealth i.age##i.wealth_group, vce(robust)


margins wealth_group, over(age)

marginsplot, ///
    plotdimension(wealth_group) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    legend(title("Wealth group"))

// figure to compare	

* Low
margins if wealth_group==1, over(age)
marginsplot, ///
    recast(line) ///
    recastci(rarea) ///
    ciopts(color(gs12%40)) ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low") ///
    legend(off) ///
    name(g1, replace)

* Middle
margins if wealth_group==2, over(age)
marginsplot, ///
    recast(line) ///
    recastci(rarea) ///
    ciopts(color(gs12%40)) ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Middle") ///
    legend(off) ///
    name(g2, replace)

* High
margins if wealth_group==3, over(age)
marginsplot, ///
    recast(line) ///
    recastci(rarea) ///
    ciopts(color(gs12%40)) ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High") ///
    legend(off) ///
    name(g3, replace)

* Combine with independent scales
graph combine g1 g2 g3, cols(3)
	

// figure without Confidence Intervals
	
	
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
	
	
	
	
	
restore


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
**##          	VI. Wealth Age Dynamics - Running the regression (2010 - 2021)
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
**###          	0. preparing the data
////////////////////////////////////////////////////////////////////////////////

preserve


*--------------------------------------------------*
* 1. Construct age in 2010
*--------------------------------------------------*
sort root_hhid_num survey_year
by root_hhid_num: gen first_year = survey_year[1]
by root_hhid_num: gen first_age  = age[1]
gen age_2010 = first_age - (first_year - 2010)

*--------------------------------------------------*
* 2. Inspect 2010 wealth distribution
*--------------------------------------------------*
sum net_wealth if survey_year == 2010, detail

* Temporary terciles for inspection only
xtile wealth_group_tercile = net_wealth if survey_year == 2010, n(3)

label define wg_temp 1 "Low" 2 "Middle" 3 "High"
label values wealth_group_tercile wg_temp

tab wealth_group_tercile if survey_year == 2010

* Show tercile cutoffs explicitly
centile net_wealth if survey_year == 2010, centile(33.33 66.67)

* Optional: inspect ranges within temporary terciles
tabstat net_wealth if survey_year == 2010, by(wealth_group_tercile) ///
    stats(min p50 max mean n)

*--------------------------------------------------*
* 3. Create final manual wealth groups
*--------------------------------------------------*
gen wealth_group = .
replace wealth_group = 1 if survey_year == 2010 & net_wealth < 100000
replace wealth_group = 2 if survey_year == 2010 & net_wealth >= 100000 & net_wealth <= 300000
replace wealth_group = 3 if survey_year == 2010 & net_wealth > 300000

label define wg 1 "Low (<100k)" 2 "Middle (100k-300k)" 3 "High (>300k)"
label values wealth_group wg

* Carry 2010 wealth group across all waves for each individual
bysort root_hhid_num: egen wealth_group_fill = max(wealth_group)
drop wealth_group
rename wealth_group_fill wealth_group
label values wealth_group wg

* Check final group sizes in 2010
tab wealth_group if survey_year == 2010

*--------------------------------------------------*
* 4. Computing ΔWealth (2010 → 2021)
*--------------------------------------------------*

* extract wealth in each year
bysort root_hhid_num: egen wealth_2010 = max(cond(survey_year==2010, net_wealth, .))
bysort root_hhid_num: egen wealth_2021 = max(cond(survey_year==2021, net_wealth, .))

* compute change
gen d_wealth_2010_2021 = wealth_2021 - wealth_2010


*--------------------------------------------------*
* 5. Creating the cohort dummy
*--------------------------------------------------*

gen cohort = .

replace cohort = 1 if age_2010 >= 50 & age_2010 < 60
replace cohort = 2 if age_2010 >= 60 & age_2010 < 70
replace cohort = 3 if age_2010 >= 70 & age_2010 < 80
replace cohort = 4 if age_2010 >= 80

label define cohort_lbl 1 "50-59" 2 "60-69" 3 "70-79" 4 "80+"
label values cohort cohort_lbl

*Manual Check

sum d_wealth_2010_2021
tab cohort

browse root_hhid_num survey_year net_wealth d_wealth_2010_2021


*--------------------------------------------------*
* 6 keep only those observations with net_wealth present both in 2010 and 2021
*--------------------------------------------------*

bysort root_hhid_num: egen has_w_2010 = max(survey_year == 2010 & !missing(net_wealth))
bysort root_hhid_num: egen has_w_2021 = max(survey_year == 2021 & !missing(net_wealth))

keep if has_w_2010 == 1 & has_w_2021 == 1


////////////////////////////////////////////////////////////////////////////////
**###          	1. Regression in amounts
////////////////////////////////////////////////////////////////////////////////

// Collapse to one obervation per individual 

collapse (first) d_wealth_2010_2021 age_2010 wealth_group ///
                 inh_amount edu cohort, by(root_hhid_num)

// keep + only
keep if age_2010 >= 50


// running regression
reg d_wealth_2010_2021 c.age_2010##i.wealth_group ///
    inh_amount edu i.cohort, vce(robust)


margins wealth_group, at(age_2010=(55 65 75 85))

marginsplot, ///
    xdimension(age_2010) ///
    plotdimension(wealth_group) ///
    xlabel(55 "50–60" 65 "60–70" 75 "70–80" 85 "80+") ///
    ytitle("Δ Wealth 2010–2021") ///
    xtitle("Age group") ///
    legend(order(1 "Low (<100k)" 2 "Middle (100k–300k)" 3 "High (>300k)") ///
           title("Wealth group"))


// Nicer Plots
 // running regression again briefly
reg d_wealth_2010_2021 c.age_2010##i.wealth_group ///
    inh_amount edu i.cohort, vce(robust)
	
* 50-60
margins wealth_group, at(age_2010=55)
marginsplot, ///
    recast(bar) ///
    recastci(rcap) ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("50-60") ///
    ytitle("Delta Wealth 2010-2021") ///
    xtitle("") ///
    legend(off) ///
    name(g1, replace)

* 60-70
margins wealth_group, at(age_2010=65)
marginsplot, ///
    recast(bar) ///
    recastci(rcap) ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("60-70") ///
    ytitle("Delta Wealth 2010-2021") ///
    xtitle("") ///
    legend(off) ///
    name(g2, replace)

* 70-80
margins wealth_group, at(age_2010=75)
marginsplot, ///
    recast(bar) ///
    recastci(rcap) ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("70-80") ///
    ytitle("Delta Wealth 2010-2021") ///
    xtitle("") ///
    legend(off) ///
    name(g3, replace)

* 80+
margins wealth_group, at(age_2010=85)
marginsplot, ///
    recast(bar) ///
    recastci(rcap) ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("80+") ///
    ytitle("Delta Wealth 2010-2021") ///
    xtitle("") ///
    legend(off) ///
    name(g4, replace)

* Combine with common axes
graph combine g1 g2 g3 g4, ///
    cols(4) ///
    ycommon xcommon ///
    title("Predicted Delta Wealth 2010-2021 by age group and wealth group")


// without CI

	* 50-60
margins wealth_group, at(age_2010=55)
marginsplot, ///
    recast(bar) ///
    noci ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("50-60") ///
    ytitle("Delta Wealth 2010-2021") ///
    xtitle("") ///
    legend(off) ///
    name(g1, replace)

* 60-70
margins wealth_group, at(age_2010=65)
marginsplot, recast(bar) noci ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("60-70") ytitle("Delta Wealth 2010-2021") xtitle("") ///
    legend(off) name(g2, replace)

* 70-80
margins wealth_group, at(age_2010=75)
marginsplot, recast(bar) noci ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("70-80") ytitle("Delta Wealth 2010-2021") xtitle("") ///
    legend(off) name(g3, replace)

* 80+
margins wealth_group, at(age_2010=85)
marginsplot, recast(bar) noci ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("80+") ytitle("Delta Wealth 2010-2021") xtitle("") ///
    legend(off) name(g4, replace)

graph combine g1 g2 g3 g4, cols(4) ycommon xcommon ///
    title("Predicted Delta Wealth by age and wealth group")


restore

////////////////////////////////////////////////////////////////////////////////
**###           2. Regression in percentage deviations from the baseline wealth level
////////////////////////////////////////////////////////////////////////////////

preserve

collapse (first) d_wealth_2010_2021 wealth_2010 wealth_2021 ///
                 age_2010 wealth_group inh_amount edu cohort, ///
                 by(root_hhid_num)

* keep only valid observations for logs
keep if wealth_2010 > 0 & wealth_2021 > 0

* log change in wealth
gen log_change_wealth_2010_2021 = log(wealth_2021) - log(wealth_2010)

* regression
reg log_change_wealth_2010_2021 ///
    c.age_2010##i.wealth_group ///
    inh_amount edu i.cohort, vce(robust)

* 50-60
margins wealth_group, at(age_2010=55)
marginsplot, ///
    recast(bar) ///
    noci ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("50-60") ///
    ytitle("Approx. % (log) change in wealth (2010 to 2021)") ///
    xtitle("") ///
    legend(off) ///
    name(g1, replace)

* 60-70
margins wealth_group, at(age_2010=65)
marginsplot, recast(bar) noci ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("60-70") ytitle("Approx. % (log) change in wealth (2010 to 2021)") xtitle("") ///
    legend(off) name(g2, replace)

* 70-80
margins wealth_group, at(age_2010=75)
marginsplot, recast(bar) noci ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("70-80") ytitle("Approx. % (log) change in wealth (2010 to 2021)") xtitle("") ///
    legend(off) name(g3, replace)

* 80+
margins wealth_group, at(age_2010=85)
marginsplot, recast(bar) noci ///
    xlabel(1 "L" 2 "M" 3 "H") ///
    title("80+") ytitle("Approx. % (log) change in wealth (2010 to 2021)") xtitle("") ///
    legend(off) name(g4, replace)

graph combine g1 g2 g3 g4, cols(4) ycommon xcommon ///
    title("Predicted log change in wealth by age and wealth group")
	

restore
	