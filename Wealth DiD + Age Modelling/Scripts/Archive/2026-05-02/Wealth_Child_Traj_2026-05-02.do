

**# generell die methodik zwischen parents und children muss ähnlich sein
* bei parents muss noch unterschiedlich sein, dass bei child sample nur die drin sind die kein inheritance haben - für children 

clear

*set directory for whole dofile

local user = c(username)

display c(username)

*(David's version)
if "`user'" == "Gebruiker" {
	global datapath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling\Data"
	global resultspath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling\Results_old"
}
else {
	global datapath "C:\Users\User\Dropbox\Research\Wealth Inheritance\Analysis\Data"
	global resultspath "C:\Users\User\Dropbox\Research\Wealth Inheritance\Analysis\Results"
}




********************************************************************************
**#          		I. Cleaning and Variable Construction					  
********************************************************************************

import delimited "$datapath\df_long_format_everyone_2026_04_29.csv" , clear // n = 83,368
//import delimited "$datapath\df_long_format_everyone_2026_04_28.csv" , clear // n = 83,368
//import delimited "$datapath\df_long_format_everyone_2026_04_28_old.csv" , clear
//import delimited "$datapath\df_long_format_everyone_split_age_jump_2026_04_24.csv" , clear 
//import delimited "$datapath\df_long_format_everyone_no_age_jump_2026_04_22.csv" , clear
//import delimited "$datapath\df_long_format_everyone_2026_04_21.csv" , clear

drop if net_wealth==. //drops n=1,618

sort root_hhid wave


**## ID and wave variables

*Generate numeric hhid for FE regressions

encode root_hhid, gen(root_hhid_num)


*survey_wave as counting from 1 instead of year

gen survey_wave=.
replace survey_wave=1 if wave=="w1"
replace survey_wave=2 if wave=="w2"
replace survey_wave=3 if wave=="w3"
replace survey_wave=4 if wave=="w4"

lab define survey_wave 1 "2010" 2 "2014" 3 "2017" 4 "2021"
lab values survey_wave survey_wave


*birthyear and cohort

gen birthyear = survey_year - age
gen cohort10 = floor(birthyear/10)*10


*Number of waves per individual

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

sort root_hhid survey_wave

//Check number of waves across individual-waves and across individuals

/*
tab num_waves

  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     48,256       57.88       57.88
          2 |     21,846       26.20       84.09
          3 |      9,018       10.82       94.90
          4 |      4,248        5.10      100.00
------------+-----------------------------------
      Total |     83,368      100.00

collapse num_waves , by(root_hhid_num)
tab num_waves

  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     48,256       76.30       76.30
          2 |     10,923       17.27       93.57
          3 |      3,006        4.75       98.32
          4 |      1,062        1.68      100.00
------------+-----------------------------------
      Total |     63,247      100.00
*/



**## Inheritance variables

//Note: we do not care about this for parents wealth trajectory because we simply treat it as part of the wealth accumulation here and we are not specifically interested in it


**## Wealth variables

*Generate wealth groups from first observed wealth
bysort root_hhid_num (survey_wave): gen first_wealth = net_wealth[1]

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


**## Order dataset and drop observations

order  root_hhid_num survey_year survey_wave   net_wealth  age

*Drop old age

drop if age > 65 //Note: not sure if to cap it at some age too?? Depends on our goal of until when to run the ABM in terms of child age. The further we estimate the ABM the worse the assumption gets that the child wealth trajectory is well estimated based on our 2010 - 2021 data

*Drop those who received inheritance already (inheritance shock modeled separately in DiD)

drop if treated_inheritance==1 
//Note: drop individual-waves from the point when they receive inheritance (include those with gifts because the ABM does not model that separately so we need to mix it up with wealth trajectory for now)
//Note: years_since_first_inheritance=0 only if reported in same wave (i.e. treated=1) (if reported in next wave then years_since_first_inheritance=. and treated=0)


//Alternatively: 

//tab years_since_first_inheritance treated_inheritance , miss

//drop if treated_inheritance==1 & years_since_first_inheritance<=10 //Note: drops only those with recent inheritance (e.g. 10 years or less ago) to keep those e.g. in 2010 with inheritance in 1990. But this is less clean! Drops only 8,348 instead of 16,235
//drop if years_since_first_inheritance==-1 //Note: for 9 obs we have an inheritance reported in the year before it is received. These cases should be dropped now. They were already dropped above because treated=1. 



save "$datapath\child_wealth_traj_cleaned_2026_04_28.dta" , replace


xtset root_hhid_num survey_wave
xtdescribe
//n=43,164 individual-waves, n=34,119 individuals


/*
use "$datapath\child_wealth_traj_cleaned_2026_04_28.dta" , clear

tab num_waves

  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     25,363       58.76       58.76
          2 |     11,319       26.22       84.98
          3 |      4,262        9.87       94.86
          4 |      2,220        5.14      100.00
------------+-----------------------------------
      Total |     43,164      100.00

collapse num_waves , by(root_hhid_num)
tab num_waves

  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     25,363       74.34       74.34
          2 |      6,279       18.40       92.74
          3 |      1,719        5.04       97.78
          4 |        758        2.22      100.00
------------+-----------------------------------
      Total |     34,119      100.00
*/



********************************************************************************
**#          	II. Regression Analysis - Child Wealth Trajectory						  
********************************************************************************


**## 1. Pooled Wealth Trajectories across Age

use "$datapath\child_wealth_traj_cleaned_2026_04_28.dta" , clear

drop if age < 20


**### 1.1 Simple OLS

reg net_wealth i.age 

margins , over(age)

marginsplot, name(c_w_byage, replace)


**### 1.2 Individual FEs

reghdfe net_wealth i.age , absorb(root_hhid_num)

margins , over(age)

marginsplot, name(c_w_byage_iFE, replace)


**### 1.3 Individual and Wave FEs

reghdfe net_wealth i.age , absorb(root_hhid_num survey_year)

margins , over(age)

marginsplot, name(c_w_byage_iwFE, replace)



**## 2. Wealth Trajectories across Age by Cohort 

use "$datapath\child_wealth_traj_cleaned_2026_04_28.dta" , clear

drop if age < 20

reg net_wealth i.age##i.cohort10

margins , over(age cohort10)

marginsplot, name(c_w_byage_bycohort, replace)



**## 3. Wealth Trajectories across Single Ages by Wealth Group

use "$datapath\child_wealth_traj_cleaned_2026_04_28.dta" , clear


**### 3.1 (!) Simple OLS


* 3.1.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

// one figure 

reg net_wealth i.age##i.wealth_quar

margins , over(age wealth_quar)

marginsplot


// separate figures (without CIs)

reg net_wealth i.age##i.wealth_quar 

* Low
margins if wealth_quar==1, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    legend(off) ///
    name(g1_noci, replace)

* Low-Middle
margins if wealth_quar==2, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    legend(off) ///
    name(g2_noci, replace)

* High-Middle
margins if wealth_quar==3, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    legend(off) ///
    name(g3_noci, replace)

* High
margins if wealth_quar==4, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    legend(off) ///
    name(g4_noci, replace)

* Combine
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2) name(p_w_byage_bywq, replace)

//graph display, ysize(2.5) //Note 4.5 is default

graph export "$resultspath\Figures\Child_wealth_byage_bywealthquar_simpleOLS_2026_04_28.pdf", replace



**### 3.3 !! Indvidiual and Wave FEs


* 3.3.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

use "$datapath\child_wealth_traj_cleaned_2026_04_28.dta" , clear

drop if age<18

//drop if age <35
//drop if age>=35

*Keep panel (redundant because reg does it):

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

*Re-generate age-specific wealth groups from first observed wealth at certain age for panel

drop first_obs
sort root_hhid_num survey_year
bysort root_hhid_num (survey_year): gen first_obs = _n==1

drop age_first
bysort root_hhid_num (survey_year): gen age_first = age if _n==1
bysort root_hhid_num (survey_year): replace age_first = age_first[1]

drop wealth_first
bysort root_hhid_num (survey_year): gen wealth_first = net_wealth if _n==1
bysort root_hhid_num (survey_year): replace wealth_first = wealth_first[1]

drop agefirst5
gen agefirst5 = floor(age_first/5)*5
tab agefirst5 if first_obs

drop wealth_terc
egen wealth_terc_first = xtile(wealth_first) if first_obs, by(agefirst5) nq(3)
bysort root_hhid_num (survey_year): replace wealth_terc_first = wealth_terc_first[1]
rename wealth_terc_first wealth_terc

drop wealth_quar
egen wealth_quar_first = xtile(wealth_first) if first_obs, by(agefirst5) nq(4)
bysort root_hhid_num (survey_year): replace wealth_quar_first = wealth_quar_first[1]
rename wealth_quar_first wealth_quar 

*Drop those with unrealistically huge jumps in wealth

bysort root_hhid_num (survey_wave): gen net_wealth_diff = net_wealth[_n]-net_wealth[_n-1]
bysort root_hhid_num (survey_wave): egen net_wealth_diff_max = max(net_wealth_diff) 
bysort root_hhid_num (survey_wave): replace net_wealth_diff_max = . if net_wealth_diff_max<0
bysort root_hhid_num (survey_wave): egen net_wealth_diff_min = min(net_wealth_diff) //Note: this is for losses but probably there are no unrealistic losses
bysort root_hhid_num (survey_wave): replace net_wealth_diff_min = . if net_wealth_diff_min>0

bysort root_hhid_num (survey_wave): gen net_wealth_diff_rel = (net_wealth[_n]-net_wealth[_n-1])/net_wealth[_n-1] if net_wealth[_n]>0 & net_wealth[_n-1]>0 //Note: I do not define diff rel for neg amounts because going into debts and coming out of debts is probably a special case. But diff rel can still be negative if going from pos to less pos. Also this I will not drop.
bysort root_hhid_num (survey_wave): egen net_wealth_diff_rel_max = max(net_wealth_diff_rel)

gen net_wealth_diff_flag=1 if net_wealth_diff>250000 & net_wealth_diff!=.
gen net_wealth_diff_rel_flag=1 if net_wealth_diff_rel>10 & net_wealth_diff_rel!=.
gen net_wealth_flag_wave = net_wealth_diff_flag * net_wealth_diff_rel_flag
bysort root_hhid_num (survey_wave): egen net_wealth_flag = max(net_wealth_flag)
drop net_wealth_diff_flag net_wealth_diff_rel_flag net_wealth_flag_wave 

order  root_hhid_num survey_year survey_wave   net_wealth  age  net_wealth_diff net_wealth_diff_max net_wealth_diff_min  net_wealth_diff_rel net_wealth_diff_rel_max  net_wealth_flag

//sort net_wealth_diff_max survey_wave //to check large gains from neg to neg or from neg to pos -> they are not very big so for now nothing done here
//br if net_wealth_diff_rel_max==.

//sort net_wealth_diff_min survey_wave //to check large losses
//br if net_wealth_diff_rel_max==.

//sort net_wealth_diff_max survey_wave
//br

//br if net_wealth_flag==1

//br if net_wealth_flag==1 & wealth_quar==1
//br if net_wealth_flag==1 & wealth_quar==2
//br if net_wealth_flag==1 & wealth_quar==3
//br if net_wealth_flag==1 & wealth_quar==4

drop if net_wealth_flag==1 //Note: drops if net_wealth_diff>250000 & net_wealth_diff_rel>10 in (at least) one (same) wave

//br if net_wealth_diff_max>250000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==1
//br if net_wealth_diff_max>500000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==2 // n = 0
//br if net_wealth_diff_max>1000000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==3 // n = 0
//br if net_wealth_diff_max>2000000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==4 // n = 0

drop if net_wealth_diff_max>250000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==1 //Note: these are those going from neg to pos 

**# oben auch noch bei den wealth backets auch noch "net of Gift" mitreinnehmen bei den gifts
*

*Run main model 

reghdfe net_wealth i.age##i.wealth_quar, absorb(root_hhid_num survey_year) resid
	
predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

**# genau gleich bisher wie parents



/*
//Figure of predicted wealth before smoothing

preserve 

collapse (mean) yhat_xbd, by(wealth_quar age)

twoway line yhat_xbd age if wealth_quar == 1 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(g1_noci, replace)

twoway line yhat_xbd age if wealth_quar == 2 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    name(g2_noci, replace)

twoway line yhat_xbd age if wealth_quar == 3 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    name(g3_noci, replace)

twoway line yhat_xbd age if wealth_quar == 4 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    name(g4_noci, replace)

* Combine
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Child_wealth_byage_bywealthquar_indwaveFE_2026_04_28.pdf", replace

restore
*/


**# das bissi anders - erster attempt gleich wie zuvor collapse und mean ähnlich 

*Collapse by age and quartile

//preserve

collapse  (mean) yhat_xbd  (count) n = yhat_xbd , by(age wealth_quar)

//Note: It is the same to run smoothing model on full data or to first collapse and then apply weights (sample size per age-quartile). But collapsing first shows me the sample sizes and I can also run without weights if I want equal weights for each age (this generally makes sense but I do not want to give weird spikes a the same weight).

sort wealth_quar age 


//Figure of predicted wealth before smoothing

twoway line yhat_xbd age if wealth_quar == 1 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(g1_noci, replace)

twoway line yhat_xbd age if wealth_quar == 2 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    name(g2_noci, replace)

twoway line yhat_xbd age if wealth_quar == 3 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    name(g3_noci, replace)

twoway line yhat_xbd age if wealth_quar == 4 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    name(g4_noci, replace)

*Combine
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Child_wealth_byage_bywealthquar_indwaveFE_2026_04_28.pdf", replace

**# Rolling average versuch

*Smoothing with rolling weighted (sample size) weighted (weights around center) average

sort wealth_quar age

//Neighbor fitted values
by wealth_quar: gen v_m3 = yhat_xbd[_n-3] if age[_n-3] == age - 3
by wealth_quar: gen v_m2 = yhat_xbd[_n-2] if age[_n-2] == age - 2
by wealth_quar: gen v_m1 = yhat_xbd[_n-1] if age[_n-1] == age - 1
gen v_0 = yhat_xbd
by wealth_quar: gen v_p1 = yhat_xbd[_n+1] if age[_n+1] == age + 1
by wealth_quar: gen v_p2 = yhat_xbd[_n+2] if age[_n+2] == age + 2
by wealth_quar: gen v_p3 = yhat_xbd[_n+3] if age[_n+3] == age + 3

//Neighbor sample sizes
by wealth_quar: gen n_m3 = n[_n-3] if age[_n-3] == age - 3
by wealth_quar: gen n_m2 = n[_n-2] if age[_n-2] == age - 2
by wealth_quar: gen n_m1 = n[_n-1] if age[_n-1] == age - 1
gen n_0 = n
by wealth_quar: gen n_p1 = n[_n+1] if age[_n+1] == age + 1
by wealth_quar: gen n_p2 = n[_n+2] if age[_n+2] == age + 2
by wealth_quar: gen n_p3 = n[_n+3] if age[_n+3] == age + 3

//Construct weighted moving average
gen num = 0
gen den = 0

replace num = num + 0.05*n_m3*v_m3 if !missing(n_m3, v_m3) //Note: this missing condition is for building wwm at corner ages (e.g. at age 18, age 15, 16, 17 not available so skipped)
replace den = den + 0.05*n_m3      if !missing(n_m3, v_m3)

replace num = num + 0.10*n_m2*v_m2 if !missing(n_m2, v_m2)
replace den = den + 0.10*n_m2      if !missing(n_m2, v_m2)

replace num = num + 0.20*n_m1*v_m1 if !missing(n_m1, v_m1)
replace den = den + 0.20*n_m1      if !missing(n_m1, v_m1)

replace num = num + 0.30*n_0*v_0   if !missing(n_0, v_0)
replace den = den + 0.30*n_0       if !missing(n_0, v_0)

replace num = num + 0.20*n_p1*v_p1 if !missing(n_p1, v_p1)
replace den = den + 0.20*n_p1      if !missing(n_p1, v_p1)

replace num = num + 0.10*n_p2*v_p2 if !missing(n_p2, v_p2)
replace den = den + 0.10*n_p2      if !missing(n_p2, v_p2)

replace num = num + 0.05*n_p3*v_p3 if !missing(n_p3, v_p3)
replace den = den + 0.05*n_p3      if !missing(n_p3, v_p3)

gen wwm_yhat_xbd = num / den if den > 0


//Figure of predicted wealth after smoothing with rolling weighted weighted average

twoway line wwm_yhat_xbd age if wealth_quar == 1 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(g1_noci, replace)

twoway line wwm_yhat_xbd age if wealth_quar == 2 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    name(g2_noci, replace)

twoway line wwm_yhat_xbd age if wealth_quar == 3 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    name(g3_noci, replace)

twoway line wwm_yhat_xbd age if wealth_quar == 4 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    name(g4_noci, replace)

graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Child_wealth_byage_wwm_smoothed_bywealthquar_indwaveFE_2026_04_28.pdf", replace

**# auf das gesmoothe model evtl noch cubic raufpacken )´(aber unrealistisch)

**# hier wollen danach 65+ noch linear weiter rausextrapolaten

**# auch ansehen ob distribution of death zeitpunkt nach education quartiles? 

*Smoothing with model 


//Cubic:

/*

reg yhat_xbd c.age##c.age##c.age##i.wealth_quar // if age<=60 //Note: cubic regression does not work well because it overemphasizes slight swings on the tails and the right tail is very important for us as we even need to project it further into higher ages!

etable, export("$resultspath\Coefficients\Child_wealth_byagecfit_smoothed_bywealthquar_indwaveFE_2026_04_28.xlsx", replace)

estimates store quadmod

//Figure of functional form of smoothing 

//preserve

clear
set obs 31
gen age = 34 + _n   // ages 35 to 65
expand 4
bysort age: gen wealth_quar = _n

estimates restore quadmod
predict yhat, xb

/*
twoway ///
    (line yhat age if wealth_quar==1, sort) ///
    (line yhat age if wealth_quar==2, sort) ///
    (line yhat age if wealth_quar==3, sort) ///
    (line yhat age if wealth_quar==4, sort), ///
    xtitle("Age") ytitle("Predicted net wealth") ///
    legend(order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4"))
*/

twoway ///
    (line yhat age if wealth_quar==1, sort) , ///
    ytitle("Net wealth") ///
    xtitle("Age") xlabel(35(5)65) ///
    title("Low: 1st Quartile") ///
    name(g1_noci, replace)

twoway ///
    (line yhat age if wealth_quar==2, sort) , ///
    ytitle("Net wealth") ///
    xtitle("Age") xlabel(35(5)65) ///
    title("Low-Middle: 2nd Quartile") ///
    name(g2_noci, replace)

twoway ///
    (line yhat age if wealth_quar==3, sort) , ///
    ytitle("Net wealth") ///
    xtitle("Age") xlabel(35(5)65) ///
    title("High-Middle: 3rd Quartile") ///
    name(g3_noci, replace)

twoway ///
    (line yhat age if wealth_quar==4, sort) , ///
    ytitle("Net wealth") ///
    xtitle("Age") xlabel(35(5)65) ///
    title("High: 4th Quartile") ///
    name(g4_noci, replace)

//restore

* Combine
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Child_wealth_byage_cfit_smoothed_bywealthquar_indwaveFE_2026_04_28.pdf", replace

*/

**# Vielleicht noch ein cubic draufpacken was die tails nicht so betont


//Restricted cubic spline:

local knots 18 25 35 45 60  

mkspline age_s = age, cubic  knots(`knots')

reg yhat_xbd i.wealth_quar##c.(age_s*) [aw=n] //weighted

//reg yhat_xbd i.wealth_quar##c.(age_s*) //unweighed

predict spline_yhat_xbd , xb

//Export for the ABM

export excel  age wealth_quar n yhat_xbd wwm_yhat_xbd spline_yhat_xbd  using "$resultspath\Coefficients\Child_wealth_byage_smoothed_bywealthquar_indwaveFE_2026_04_28.xlsx" , firstrow(variables) replace


//Figure of predicted (after smoothing) predicted wealth 

twoway line spline_yhat_xbd age if wealth_quar == 1 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(g1_noci, replace)

twoway line spline_yhat_xbd age if wealth_quar == 2 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    name(g2_noci, replace)

twoway line spline_yhat_xbd age if wealth_quar == 3 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    name(g3_noci, replace)

twoway line spline_yhat_xbd age if wealth_quar == 4 , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    name(g4_noci, replace)

graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Child_wealth_byage_spline_w_smoothed_bywealthquar_indwaveFE_2026_04_28.pdf", replace
//graph export "$resultspath\Figures\Child_wealth_byage_spline_unw_smoothed_bywealthquar_indwaveFE_2026_04_28.pdf", replace


//Note: export this and implement in ABM as follows: projected_wealth_i,t = observed_wealth_i,2021 - predicted_wealth(age_i,2021) + predicted_wealth(age_i,t) , where t=2021+




**## 5. Wealth Trajectories across Age Cubic by Wealth Group

**### 5.1 ! Simple OLS

use "$datapath\child_wealth_traj_cleaned_2026_04_28.dta" , clear

drop if age < 18

* 5.1.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

// separate figures (without CIs)

reg net_wealth c.age##c.age##c.age##i.wealth_quar

//gen age2 = age*age
//gen age3 = age*age*age
//reg net_wealth c.age##i.wealth_quar c.age2##i.wealth_quar c.age3##i.wealth_quar

etable, export("$resultspath\Coefficients\Child_wealth_byagecfit_bywealthquar_simpleOLS_2026_04_28.xlsx", replace)

estimates store cubicmod

//preserve

clear
set obs 48
gen age = 17 + _n   // ages 18 to 65
expand 4
bysort age: gen wealth_quar = _n

estimates restore cubicmod
predict yhat, xb

twoway ///
    (line yhat age if wealth_quar==1, sort) ///
    (line yhat age if wealth_quar==2, sort) ///
    (line yhat age if wealth_quar==3, sort) ///
    (line yhat age if wealth_quar==4, sort), ///
    xtitle("Age") ytitle("Predicted net wealth") ///
    legend(order(1 "Q1" 2 "Q2" 3 "Q3" 4 "Q4"))

twoway ///
    (line yhat age if wealth_quar==1, sort) , ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(g1_noci, replace)

twoway ///
    (line yhat age if wealth_quar==2, sort) , ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    name(g2_noci, replace)

twoway ///
    (line yhat age if wealth_quar==3, sort) , ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    name(g3_noci, replace)

twoway ///
    (line yhat age if wealth_quar==4, sort) , ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    name(g4_noci, replace)

//restore

* Combine
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Child_wealth_byagecfit_bywealthquar_simpleOLS_2026_04_28.pdf", replace

/*
* Low
//margins if wealth_quar==1, over(age)  //Note: same as  margins , over(age) at(wealth_quar=1)  or  margins , at(age=(50(1)85) wealth_quar=1)
margins , at(age=(50(1)85) wealth_quar=1)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    legend(off) ///
    name(g1_noci, replace)

* Low-Middle
//margins if wealth_quar==2, over(age)
margins , at(age=(50(1)85) wealth_quar=2)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    legend(off) ///
    name(g2_noci, replace)

* High-Middle
margins if wealth_quar==3, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    legend(off) ///
    name(g3_noci, replace)

* High
margins if wealth_quar==4, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    legend(off) ///
    name(g4_noci, replace)

* Combine
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Parent_wealth_byagecfit_bywealthquar_simpleOLS_2026_04_17.pdf", replace
*/



**## 6. Wealth Trajectories across Age Splines by Wealth Group

**### 6.1 Simple OLS

use "$datapath\child_wealth_traj_cleaned_2026_04_28.dta" , clear

* 4.1.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

// separate figures (without CIs)

mkspline age_s1 30 age_s2 40 age_s3 50 age_s4 = age

reg net_wealth  c.age_s1 c.age_s2 c.age_s3 c.age_s4  i.wealth_quar  c.age_s1#i.wealth_quar c.age_s2#i.wealth_quar c.age_s3#i.wealth_quar c.age_s4#i.wealth_quar


* Low
margins if wealth_quar==1, over(age)  //Note: same as  margins , over(age) at(wealth_quar=1)  or  margins , at(age=(50(1)85) wealth_quar=1)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    legend(off) ///
    name(g1_noci, replace)

* Low-Middle
margins if wealth_quar==2, over(age)
margins , at(age=(50(1)85) wealth_quar=2)


marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    legend(off) ///
    name(g2_noci, replace)

* High-Middle
margins if wealth_quar==3, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    legend(off) ///
    name(g3_noci, replace)

* High
margins if wealth_quar==4, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    legend(off) ///
    name(g4_noci, replace)

* Combine
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Parent_wealth_byagecfit_bywealthquar_simpleOLS_2026_04_17.pdf", replace







