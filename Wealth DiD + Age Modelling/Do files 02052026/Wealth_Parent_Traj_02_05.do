
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

* 1) anstatt wealth_first den average machen
* für kinder keine gift shocks - 

egen wealth_terc_first = xtile(wealth_first) if first_obs, by(agefirst5) nq(3)
bysort root_hhid_num (survey_year): replace wealth_terc_first = wealth_terc_first[1]
rename wealth_terc_first wealth_terc

egen wealth_quar_first = xtile(wealth_first) if first_obs, by(agefirst5) nq(4)
bysort root_hhid_num (survey_year): replace wealth_quar_first = wealth_quar_first[1]
rename wealth_quar_first wealth_quar


**## Order dataset and drop observations

* 2) age below 50 oder 

order  root_hhid_num survey_year survey_wave   net_wealth  age

drop if age < 50


save "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , replace


xtset root_hhid_num survey_wave
xtdescribe
//n=54,186 individual-waves, n=41,188 individuals (n=61,561 individuals before dropping age<50)  //(before: n=54,186 individual-waves, n=40,219 individuals (n=61,561 individuals before dropping age<50))


/*
use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

tab num_waves

  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     30,450       56.20       56.20
          2 |     14,264       26.32       82.52
          3 |      6,362       11.74       94.26
          4 |      3,110        5.74      100.00
------------+-----------------------------------
      Total |     54,186      100.00

collapse num_waves , by(root_hhid_num)
tab num_waves

  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     30,450       73.93       73.93
          2 |      7,542       18.31       92.24
          3 |      2,313        5.62       97.86
          4 |        883        2.14      100.00
------------+-----------------------------------
      Total |     41,188      100.00
*/




********************************************************************************
**#          	II. Regression Analysis - Parent Wealth Trajectory						  
********************************************************************************


**## 1. Pooled Wealth Trajectories across Age


**### 1.1 Simple OLS

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

reg net_wealth i.age 

margins , over(age)

marginsplot, name(p_w_byage, replace)

graph export "$resultspath\Figures\Parent_wealth_byage_simpleOLS_2026_04_28.pdf", replace


**### 1.2 Individual FEs

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

//Re-generate age-specific wealth groups from first observed wealth at certain age for panel

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

//Figure 

reghdfe net_wealth i.age , absorb(root_hhid_num)  resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

collapse (mean) yhat_xbd, by(age)

twoway line yhat_xbd age , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(p_w_byage_iFE, replace)

graph export "$resultspath\Figures\Parent_wealth_byage_indFE_2026_04_28.pdf", replace


**### 1.3 Individual and Wave FEs

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

//Re-generate age-specific wealth groups from first observed wealth at certain age for panel

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

//Figure 

reghdfe net_wealth i.age , absorb(root_hhid_num survey_year)  resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

collapse (mean) yhat_xbd, by(age)

twoway line yhat_xbd age , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(p_w_byage_iwFE, replace)

graph export "$resultspath\Figures\Parent_wealth_byage_indwaveFE_2026_04_28.pdf", replace



**## 2. Wealth Trajectories across Age by Cohort 

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

reg net_wealth i.age##i.cohort10

margins , over(age cohort10)

marginsplot, name(p_w_byage_bycohort, replace)

graph export "$resultspath\Figures\Parent_wealth_byage_bycohort_2026_04_28.pdf", replace



**## 3. Wealth Trajectories across Single Ages by Wealth Group


**### 3.1 (!) Simple OLS


* 3.1.1 By wealth group with fixed definition across age

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

// one figure 

reg net_wealth i.age##i.wealth_group 

margins , over(age wealth_group)

marginsplot


// separate figures (without CIs)

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

graph display, ysize(2.0) //Note 4.5 is default

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthgr3_simpleOLS_2026_04_28.pdf", replace


* 3.1.2 By wealth terciles defined separately by 5-year age-groups build from first observed age by individual

// one figure 

reg net_wealth i.age##i.wealth_terc

margins , over(age wealth_terc)

marginsplot


// separate figures (without CIs)

reg net_wealth i.age##i.wealth_terc 

* Low
margins if wealth_terc==1, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Tercile") ///
    legend(off) ///
    name(g1_noci, replace)

* Middle
margins if wealth_terc==2, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Middle: 2nd Tercile") ///
    legend(off) ///
    name(g2_noci, replace)

* High
margins if wealth_terc==3, over(age)
marginsplot, ///
    recast(line) ///
    noci ///
    plotopts(lwidth(medthick)) ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 3rd Tercile") ///
    legend(off) ///
    name(g3_noci, replace)

* Combine
graph combine g1_noci g2_noci g3_noci, cols(3)

graph display, ysize(2.0) //Note 4.5 is default

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthterc_simpleOLS_2026_04_28.pdf", replace


* 3.1.3 ! By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

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

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthquar_simpleOLS_2026_04_28.pdf", replace



**### 3.2 Indvidiual FEs


* 3.2.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

//Re-generate age-specific wealth groups from first observed wealth at certain age for panel

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


// separate figures (without CIs)

reghdfe net_wealth i.age##i.wealth_quar  , absorb(root_hhid_num)  resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

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

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthquar_indFE_2026_04_28.pdf", replace



**### 3.3 !! Indvidiual and Wave FEs

* 3.3.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1
* drop alle mit nur 1 wave 

//Re-generate age-specific wealth groups from first observed wealth at certain age for panel

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

* 4) hier auch nochmal den mean machen wie oben

drop wealth_terc
egen wealth_terc_first = xtile(wealth_first) if first_obs, by(agefirst5) nq(3)
bysort root_hhid_num (survey_year): replace wealth_terc_first = wealth_terc_first[1]
rename wealth_terc_first wealth_terc

drop wealth_quar
egen wealth_quar_first = xtile(wealth_first) if first_obs, by(agefirst5) nq(4)
bysort root_hhid_num (survey_year): replace wealth_quar_first = wealth_quar_first[1]
rename wealth_quar_first wealth_quar 


**# Bookmark #3

* 5) das was unten steht hier noch mal anfangen - den block unten gibts auch für Wealth_Child_Traj.do also die increases droppen die unrealistisch sind, relative increase e.g. 1 Mio zu 1,3 Mio ist ok

//Drop those with unrealistically huge jumps in wealth //Not sure if doing this here because there might be inheritances here

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

**# Bookmark #1
* checken ob es negative wealth giv

//br if net_wealth_diff_max>250000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==1
//br if net_wealth_diff_max>500000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==2 // n = 0
//br if net_wealth_diff_max>1000000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==3 // n = 0
//br if net_wealth_diff_max>2000000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==4 // n = 0

drop if net_wealth_diff_max>250000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==1 //Note: these are those going from neg to pos 
*/
**# Bookmark #2
* hier werden wealth von 1 assigned für negative




// separate figures (without CIs)

* hier sehen wir die jumps per age, das sollte dann besser werden wenn wir die "means" nehmen dann hast du nicht mehr diese jumps

reghdfe net_wealth i.age##i.wealth_quar  , absorb(root_hhid_num survey_year) resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

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

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthquar_indwaveFE_2026_04_28.pdf", replace

restore


//Get smoothed out version (now quadratic but could also be cubic)

//reghdfe net_wealth i.age##i.wealth_quar  , absorb(root_hhid_num survey_year) resid
//predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

reg yhat_xbd c.age##c.age##i.wealth_quar

etable, export("$resultspath\Coefficients\Parent_wealth_byageqfit_smoothed_bywealthquar_indwaveFE_2026_04_28.xlsx", replace)

* Low
margins if wealth_quar==1, over(age)  //Note: same as  margins , over(age) at(wealth_quar=1)  or  margins , at(age=50(1)85 wealth_quar=1)

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
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Parent_wealth_byageqfit_smoothed_bywealthquar_indwaveFE_2026_04_28.pdf", replace



**## 4. Wealth Trajectories across Age Quadratic by Wealth Group


**### 4.1 ! Simple OLS

* 4.1.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

// separate figures (without CIs)

reg net_wealth c.age##c.age##i.wealth_quar 

etable, export("$resultspath\Coefficients\Parent_wealth_byageqfit_bywealthquar_simpleOLS_2026_04_28.xlsx", replace)

**# Bookmark #4
*checken wieder unten welche zu hoch sind


**# Bookmark #5 testen ob panel members structurally different 
* und generell survey weights 


* Low
margins if wealth_quar==1, over(age)  //Note: same as  margins , over(age) at(wealth_quar=1)  or  margins , at(age=50(1)85 wealth_quar=1)

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
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Parent_wealth_byageqfit_bywealthquar_simpleOLS_2026_04_28.pdf", replace


**### 4.2 Individual FEs

* 4.2.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

//Re-generate age-specific wealth groups from first observed wealth at certain age for panel

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

// separate figures (without CIs)

reghdfe net_wealth c.age##c.age##i.wealth_quar  , absorb(root_hhid_num)  resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

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

graph export "$resultspath\Figures\Parent_wealth_byageqfit_bywealthquar_indFE_2026_04_28.pdf", replace



**### 4.3 Individual and Wave FEs


* 4.3.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

//Re-generate age-specific wealth groups from first observed wealth at certain age for panel

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

// separate figures (without CIs)

reghdfe net_wealth c.age##c.age##i.wealth_quar  , absorb(root_hhid_num survey_year)  resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

**# hier wird quasi dann auf die predicted values dann die Squared raufgepackt


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

graph export "$resultspath\Figures\Parent_wealth_byageqfit_bywealthquar_indFE_2026_04_28.pdf", replace


**## 5. Wealth Trajectories across Age Cubic by Wealth Group

**### 4.1 Simple OLS

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

* 4.1.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

// separate figures (without CIs)

reg net_wealth c.age##c.age##c.age##i.wealth_quar //Note: same as  margins , over(age) at(wealth_quar=1)  or  margins , at(age=(50(1)85) wealth_quar=1)

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
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Parent_wealth_byagecfit_bywealthquar_simpleOLS_2026_04_28.pdf", replace









