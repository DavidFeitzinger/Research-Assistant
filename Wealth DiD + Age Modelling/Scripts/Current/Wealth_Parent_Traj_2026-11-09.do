
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
	global resultspath "C:\Users\User\Dropbox\Research\Wealth Inheritance\Analysis\Results_new"
}

********************************************************************************
**#          		I. Cleaning and Variable Construction					  
********************************************************************************

import excel "$datapath\df_long_format_everyone_2026_05_20.xlsx", ///
    firstrow clear

destring hh_weight, replace force
drop if missing(hh_weight) | hh_weight <= 0

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


**## Wealth variables

* Generate wealth groups from first observed wealth
bysort root_hhid_num (survey_wave): gen first_wealth = net_wealth[1]

gen wealth_group = .
replace wealth_group = 1 if first_wealth < 100000
replace wealth_group = 2 if first_wealth >= 100000 & first_wealth <= 300000
replace wealth_group = 3 if first_wealth > 300000

label define wg 1 "Low (<100k)" 2 "Middle (100k-300k)" 3 "High (>300k)", replace
label values wealth_group wg

* Generate age-specific wealth groups: first observed vs average observed wealth

sort root_hhid_num survey_year
bysort root_hhid_num (survey_year): gen first_obs = _n==1

bysort root_hhid_num (survey_year): gen age_first = age if _n==1
bysort root_hhid_num (survey_year): replace age_first = age_first[1]

* Average observed wealth
bysort root_hhid_num: egen wealth_avg = mean(net_wealth)

gen agefirst5 = floor(age_first/5)*5
tab agefirst5 if first_obs

* Terciles: first observed wealth
gen wealth_terc_first = .
gen wealth_terc_avg   = .
gen wealth_quar_first = .
gen wealth_quar_avg   = .

levelsof agefirst5 if first_obs, local(agebins)

foreach a of local agebins {
    capture drop temp_terc_first
    capture drop temp_terc_avg
    capture drop temp_quar_first
    capture drop temp_quar_avg

    xtile temp_terc_first = first_wealth [pweight=hh_weight] ///
        if first_obs & agefirst5 == `a', nq(3)

    xtile temp_terc_avg = wealth_avg [pweight=hh_weight] ///
        if first_obs & agefirst5 == `a', nq(3)

    xtile temp_quar_first = first_wealth [pweight=hh_weight] ///
        if first_obs & agefirst5 == `a', nq(4)

    xtile temp_quar_avg = wealth_avg [pweight=hh_weight] ///
        if first_obs & agefirst5 == `a', nq(4)

    replace wealth_terc_first = temp_terc_first if first_obs & agefirst5 == `a'
    replace wealth_terc_avg   = temp_terc_avg   if first_obs & agefirst5 == `a'
    replace wealth_quar_first = temp_quar_first if first_obs & agefirst5 == `a'
    replace wealth_quar_avg   = temp_quar_avg   if first_obs & agefirst5 == `a'
}

bysort root_hhid_num (survey_year): replace wealth_terc_first = wealth_terc_first[1]
bysort root_hhid_num (survey_year): replace wealth_terc_avg   = wealth_terc_avg[1]
bysort root_hhid_num (survey_year): replace wealth_quar_first = wealth_quar_first[1]
bysort root_hhid_num (survey_year): replace wealth_quar_avg   = wealth_quar_avg[1]

* Compare classifications
tab wealth_terc_first wealth_terc_avg if first_obs, row col
tab wealth_quar_first wealth_quar_avg if first_obs, row col

* Create matrix from tab
tab wealth_quar_first wealth_quar_avg if first_obs, matcell(freq)

* Heatmap
heatplot freq, ///
    color(Blues) ///
    ytitle("First wealth quartile") ///
    xtitle("Average wealth quartile") ///
    title("Mobility: First vs Average Wealth Quartile") ///
    values(format(%9.0g))
	
graph export "$resultspath\Figures\First_vs_average_wealth_quartile_overlap_0805.pdf", replace


* The heatmap compares wealth quartile classification based on first observed wealth vs. average wealth.
* The strong concentration along the diagonal indicates very high persistence across definitions, with around 90–96% of households remaining in the same quartile.
* Off-diagonal movements are limited and mostly occur between adjacent quartiles, suggesting that using average instead of initial wealth has only a minor impact on classification.

* Use average-wealth classification for all subsequent analysis
gen wealth_terc = wealth_terc_avg
gen wealth_quar = wealth_quar_avg

* Set preferred specification to average wealth for subsequent analysis
ds wealth_terc*
ds wealth_quar*
	
	
	
	
	
**## Order dataset and drop observations

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

reg net_wealth i.age [pweight=hh_weight]

margins , over(age)

marginsplot, name(p_w_byage, replace)

graph export "$resultspath\Figures\Parent_wealth_byage_simpleOLS_2026_08_05.pdf", replace

**### 1.2 Individual FEs

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

drop num_waves

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

//Figure 

reghdfe net_wealth i.age [pweight=hh_weight], absorb(root_hhid_num) resid
predict yhat_xbd, xbd

collapse (mean) yhat_xbd [aw=hh_weight], by(age)

twoway line yhat_xbd age , ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(p_w_byage_iFE, replace)

graph export "$resultspath\Figures\Parent_wealth_byage_indFE_2026_08_05.pdf", replace


**### 1.3 Individual and Wave FEs

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

//Re-generate age-specific wealth groups from average observed wealth for panel

drop first_obs
sort root_hhid_num survey_year
bysort root_hhid_num (survey_year): gen first_obs = _n==1

drop age_first
bysort root_hhid_num (survey_year): gen age_first = age if _n==1
bysort root_hhid_num (survey_year): replace age_first = age_first[1]

drop wealth_avg
bysort root_hhid_num: egen wealth_avg = mean(net_wealth)

drop agefirst5
gen agefirst5 = floor(age_first/5)*5
tab agefirst5 if first_obs

drop wealth_terc
drop wealth_terc_avg
drop wealth_quar
drop wealth_quar_avg

gen wealth_terc_avg = .
gen wealth_quar_avg = .

levelsof agefirst5 if first_obs, local(agebins)

foreach a of local agebins {
    capture drop temp_terc
    capture drop temp_quar

    xtile temp_terc = wealth_avg [pweight=hh_weight] ///
        if first_obs & agefirst5 == `a', nq(3)

    xtile temp_quar = wealth_avg [pweight=hh_weight] ///
        if first_obs & agefirst5 == `a', nq(4)

    replace wealth_terc_avg = temp_terc if first_obs & agefirst5 == `a'
    replace wealth_quar_avg = temp_quar if first_obs & agefirst5 == `a'
}

bysort root_hhid_num (survey_year): replace wealth_terc_avg = wealth_terc_avg[1]
rename wealth_terc_avg wealth_terc

bysort root_hhid_num (survey_year): replace wealth_quar_avg = wealth_quar_avg[1]
rename wealth_quar_avg wealth_quar

//Figure 

reghdfe net_wealth i.age [pweight=hh_weight], absorb(root_hhid_num survey_year) resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

collapse (mean) yhat_xbd [aw=hh_weight], by(age)

twoway line yhat_xbd age , ///
	ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(p_w_byage_iwFE, replace)

graph export "$resultspath\Figures\Parent_wealth_byage_indwaveFE_2026_08_05.pdf", replace


**## 2. Wealth Trajectories across Age by Cohort 

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

reg net_wealth i.age##i.cohort10 [pweight=hh_weight]

margins , over(age cohort10)

marginsplot, name(p_w_byage_bycohort, replace)

graph export "$resultspath\Figures\Parent_wealth_byage_bycohort_2026_08_05.pdf", replace



**## 3. Wealth Trajectories across Single Ages by Wealth Group


**### 3.1 (!) Simple OLS


* 3.1.1 By wealth group with fixed definition across age

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

// one figure 

reg net_wealth i.age##i.wealth_group [pweight=hh_weight]

margins , over(age wealth_group)

marginsplot


// separate figures (without CIs)

reg net_wealth i.age##i.wealth_group [pweight=hh_weight]

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

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthgr3_simpleOLS_2026_08_05.pdf", replace


* 3.1.2 By wealth terciles defined separately by 5-year age-groups build from first observed age by individual

// one figure 

reg net_wealth i.age##i.wealth_terc [pweight=hh_weight]

margins , over(age wealth_terc)

marginsplot


// separate figures (without CIs)

reg net_wealth i.age##i.wealth_terc [pweight=hh_weight]

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

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthterc_simpleOLS_2026_08_05.pdf", replace


* 3.1.3 ! By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

// one figure 

reg net_wealth i.age##i.wealth_quar [pweight=hh_weight]

margins , over(age wealth_quar)

marginsplot


// separate figures (without CIs)

reg net_wealth i.age##i.wealth_quar [pweight=hh_weight]

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

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthquar_simpleOLS_2026_08_05.pdf", replace


**### 3.2 Indvidiual FEs

* 3.2.3 By wealth quartiles defined separately by 5-year age-groups based on first observed age and average observed wealth

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

// separate figures (without CIs)

reghdfe net_wealth i.age##i.wealth_quar [pweight=hh_weight], absorb(root_hhid_num) resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

collapse (mean) yhat_xbd [aw=hh_weight], by(wealth_quar age)

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

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthquar_indFE_2026_08_05.pdf", replace


**### 3.3 !! Indvidiual and Wave FEs

* 3.3.3 By wealth quartiles defined separately by 5-year age-groups based on first observed age and average observed wealth

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1
* drop alle mit nur 1 wave 

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

drop if net_wealth_flag==1 //Note: drops if net_wealth_diff>250000 & net_wealth_diff_rel>10 in (at least) one (same) wave

drop if net_wealth_diff_max>250000 & net_wealth_diff_max!=. & net_wealth_diff_rel_max==. & wealth_quar==1 //Note: these are those going from neg to pos 



// separate figures (without CIs)

* hier sehen wir die jumps per age, das sollte dann besser werden wenn wir die "means" nehmen dann hast du nicht mehr diese jumps

reghdfe net_wealth i.age##i.wealth_quar [pweight=hh_weight], absorb(root_hhid_num survey_year) resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

preserve 

collapse (mean) yhat_xbd [aw=hh_weight], by(wealth_quar age)

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

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthquar_indwaveFE_2026_08_05.pdf", replace

restore


//Get smoothed out version (now quadratic but could also be cubic)

//reghdfe net_wealth i.age##i.wealth_quar  , absorb(root_hhid_num survey_year) resid
//predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

reg yhat_xbd c.age##c.age##i.wealth_quar [pweight=hh_weight]

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

graph export "$resultspath\Figures\Parent_wealth_byageqfit_smoothed_bywealthquar_indwaveFE_2026_08_05.pdf", replace



**## 4. Wealth Trajectories across Age Quadratic by Wealth Group


**### 4.1 ! Simple OLS

* 4.1.3 By wealth quartiles defined separately by 5-year age-groups build from first observed age by individual

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

// separate figures (without CIs)

reg net_wealth c.age##c.age##i.wealth_quar [pweight=hh_weight]

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

graph export "$resultspath\Figures\Parent_wealth_simpleOLS_2026_08_05.pdf", replace


**### 4.2 Individual FEs

* 4.2.3 By wealth quartiles defined separately by 5-year age-groups based on first observed age and average observed wealth

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

// separate figures (without CIs)

reghdfe net_wealth c.age##c.age##i.wealth_quar [pweight=hh_weight], absorb(root_hhid_num) resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

collapse (mean) yhat_xbd [aw=hh_weight], by(wealth_quar age)

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

graph export "$resultspath\Figures\Parent_wealth_byageqfit_bywealthquar_indFE_2026_08_05.pdf", replace



**### 4.3 Individual and Wave FEs

* 4.3.3 By wealth quartiles defined separately by 5-year age-groups based on first observed age and average observed wealth

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

//Keep panel (redundant because reg does it)

drop num_waves //Note: because we dropped if age<50 this is not fitting anymore

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

drop if num_waves==1

// separate figures (without CIs)

reghdfe net_wealth c.age##c.age##i.wealth_quar [pweight=hh_weight], absorb(root_hhid_num survey_year) resid

predict yhat_xbd, xbd //Note: I need to predict using fixed-effects because margins does not take them into account (and therefore also not wealth quartile difference bc dropped)

**# hier wird quasi dann auf die predicted values dann die Squared raufgepackt

collapse (mean) yhat_xbd [aw=hh_weight], by(wealth_quar age)

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

graph export "$resultspath\Figures\Parent_wealth_byageqfit_bywealthquar_indFE_2026_08_05.pdf", replace


**## 5. Wealth Trajectories across Age Cubic by Wealth Group

**### 5.1 Simple OLS

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

* 5.1.3 By wealth quartiles defined separately by 5-year age-groups based on first observed age and average observed wealth

// separate figures (without CIs)

gen age2 = age^2
gen age3 = age^3

reg net_wealth c.age##i.wealth_quar c.age2##i.wealth_quar c.age3##i.wealth_quar [pweight=hh_weight]

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

graph export "$resultspath\Figures\Parent_wealth_byagecfit_bywealthquar_simpleOLS_2026_08_05.pdf", replace


**# 6. Adding a stricter exclusion criteria
use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

* Keep panel
drop num_waves
duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves = num_waves + 1
drop if num_waves == 1

* Drop top 20% wealth outliers within survey wave
bysort survey_wave: egen p80_wealth = pctile(net_wealth), p(80)
drop if net_wealth > p80_wealth

* Original model
reghdfe net_wealth i.age##i.wealth_quar [pweight=hh_weight], ///
    absorb(root_hhid_num survey_year) resid

predict yhat_xbd, xbd

* Weighted mean after weighted regression
collapse (mean) yhat_xbd [aw=hh_weight], by(wealth_quar age)

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

graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthquar_indwaveFE_outlierdrop_weightedmean_2026_08_05.pdf", replace


**# 7) Using logs and backing out Euro amount in the end

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

* Keep panel
drop num_waves
duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves = num_waves + 1
drop if num_waves == 1

* Drop extreme outliers
bysort survey_wave: egen p99_wealth = pctile(net_wealth), p(99)
drop if net_wealth > p99_wealth

* Log wealth only for positive values
gen ln_wealth = log(net_wealth) if net_wealth > 0

* FE regression in logs
reghdfe ln_wealth i.age##i.wealth_quar [pweight=hh_weight], ///
    absorb(root_hhid_num survey_year) resid

predict ln_yhat, xbd

* Convert back to EUR
gen yhat_eur = exp(ln_yhat)

* Collapse weighted mean
collapse (mean) yhat_eur [aw=hh_weight], by(wealth_quar age)

* Plot
twoway line yhat_eur age if wealth_quar == 1 , ///
    ytitle("Net wealth (EUR)") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(g1, replace)

twoway line yhat_eur age if wealth_quar == 2 , ///
    ytitle("Net wealth (EUR)") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    name(g2, replace)

twoway line yhat_eur age if wealth_quar == 3 , ///
    ytitle("Net wealth (EUR)") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    name(g3, replace)

twoway line yhat_eur age if wealth_quar == 4 , ///
    ytitle("Net wealth (EUR)") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    name(g4, replace)

graph combine g1 g2 g3 g4, cols(2)

graph export "$resultspath\Figures\Parent_wealth_byage_log_backtransformed_2026_08_05.pdf", replace


**# 8) trimming *within* quartiles not globally, binning ages and collapsing median

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

* 1. Trim within quartile
bysort wealth_quar survey_wave: egen p99 = pctile(net_wealth), p(99)
drop if net_wealth > p99

* 2. Bin ages
gen age_bin = floor(age/3)*3

* 3. Run model
reghdfe net_wealth i.age_bin##i.wealth_quar [pweight=hh_weight], absorb(root_hhid_num survey_year) resid

predict yhat_xbd, xbd

* 4. Collapse median
collapse (mean) yhat_xbd [aw=hh_weight], by(wealth_quar age_bin)

* 4b. Save coefficient-style trajectory output for ABM
export excel wealth_quar age_bin yhat_xbd ///
    using "$resultspath\Coefficients\Parent_wealth_byage_binned_trimmed_indwaveFE_2026_08_05.xlsx", ///
    firstrow(variables) replace


* 5. Plot trajectories (using age bins)

twoway line yhat_xbd age_bin if wealth_quar == 1 , ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    name(g1_noci, replace)

twoway line yhat_xbd age_bin if wealth_quar == 2 , ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    name(g2_noci, replace)

twoway line yhat_xbd age_bin if wealth_quar == 3 , ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    name(g3_noci, replace)

twoway line yhat_xbd age_bin if wealth_quar == 4 , ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    name(g4_noci, replace)

* Combine
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthquar_binned_trimmed_2026_08_05.pdf", replace


**# 9) linear extrapolation to age 100 based on last 10 years, exporting profile and coefficients

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

* 1. Trim within quartile
bysort wealth_quar survey_wave: egen p99 = pctile(net_wealth), p(99)
drop if net_wealth > p99

* 2. Bin ages
gen age_bin = floor(age/3)*3

* 3. Run model
reghdfe net_wealth i.age_bin##i.wealth_quar [pweight=hh_weight], absorb(root_hhid_num survey_year) resid

predict yhat_xbd, xbd

* 4. Collapse median
collapse (mean) yhat_xbd [aw=hh_weight], by(wealth_quar age_bin)

gen extrapolated = 0

tempfile observed extrapolated extrap_coefs
save `observed', replace

* 5. Extrapolate linearly to age 100 using last 10 years
postfile handle wealth_quar age_bin yhat_xbd extrapolated using `extrapolated', replace
postfile coefhandle wealth_quar last_observed_age intercept slope using `extrap_coefs', replace

levelsof wealth_quar, local(qs)

foreach q of local qs {

    quietly summarize age_bin if wealth_quar == `q'
    local maxage = r(max)

    quietly regress yhat_xbd age_bin if wealth_quar == `q' & age_bin >= `maxage' - 10

    local b0 = _b[_cons]
    local b1 = _b[age_bin]

    post coefhandle (`q') (`maxage') (`b0') (`b1')

    quietly summarize yhat_xbd if wealth_quar == `q' & age_bin == `maxage'
    local last_y = r(mean)

    post handle (`q') (`maxage') (`last_y') (1)

    local start = `maxage' + 3

    forvalues a = `start'(3)100 {
        local pred = `last_y' + `b1' * (`a' - `maxage')
        post handle (`q') (`a') (`pred') (1)
    }
}

postclose handle
postclose coefhandle

* 6. Export extrapolation coefficients
preserve

use `extrap_coefs', clear

export excel wealth_quar last_observed_age intercept slope ///
    using "$resultspath\Coefficients\Parent_wealth_binned_trimmed_extrapolation_coefficients_last10_2026_08_05.xlsx", ///
    firstrow(variables) replace

restore

* 7. Export full observed and extrapolated profile
use `observed', clear
append using `extrapolated'

sort wealth_quar age_bin extrapolated

export excel wealth_quar age_bin yhat_xbd extrapolated ///
    using "$resultspath\Coefficients\Parent_wealth_binned_trimmed_extrapolated100_last10_2026_08_05.xlsx", ///
    firstrow(variables) replace

* 8. Plot observed and extrapolated trajectories
twoway ///
    (line yhat_xbd age_bin if wealth_quar == 1 & extrapolated == 0, sort) ///
    (line yhat_xbd age_bin if wealth_quar == 1 & extrapolated == 1, sort lpattern(dash)), ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    legend(off) ///
    name(g1_noci, replace)

twoway ///
    (line yhat_xbd age_bin if wealth_quar == 2 & extrapolated == 0, sort) ///
    (line yhat_xbd age_bin if wealth_quar == 2 & extrapolated == 1, sort lpattern(dash)), ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    legend(off) ///
    name(g2_noci, replace)

twoway ///
    (line yhat_xbd age_bin if wealth_quar == 3 & extrapolated == 0, sort) ///
    (line yhat_xbd age_bin if wealth_quar == 3 & extrapolated == 1, sort lpattern(dash)), ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    legend(off) ///
    name(g3_noci, replace)

twoway ///
    (line yhat_xbd age_bin if wealth_quar == 4 & extrapolated == 0, sort) ///
    (line yhat_xbd age_bin if wealth_quar == 4 & extrapolated == 1, sort lpattern(dash)), ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    legend(off) ///
    name(g4_noci, replace)

graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

graph export "$resultspath\Figures\Parent_wealth_byage_bywealthquar_binned_trimmed_extrapolated100_last10_2026_08_05.pdf", replace


* ============================================================
* 10) Outlier handling for graphing + figures: parent wealth
*    Uses $resultspath\Figures
*    Does NOT modify original data
* ============================================================

global figpath "$resultspath\Figures"

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta", clear

* ------------------------------------------------------------
* Outlier handling for graphing only
* ------------------------------------------------------------

gen wealth = net_wealth

bysort age: egen p99_age = pctile(wealth), p(99)
bysort age: egen p1_age  = pctile(wealth), p(1)

gen wealth_graph = wealth

replace wealth_graph = . if wealth > p99_age & !missing(wealth)
replace wealth_graph = . if wealth < p1_age  & !missing(wealth)

gen upper_out = wealth > p99_age if !missing(wealth)
gen lower_out = wealth < p1_age  if !missing(wealth)
gen neg_wealth = wealth < 0 if !missing(wealth)

* Diagnostics
di "Upper outliers:"
count if upper_out == 1

di "Lower outliers:"
count if lower_out == 1

di "Negative wealth:"
count if neg_wealth == 1

* ------------------------------------------------------------
* FIGURE 1: Age-specific trimmed parent profiles
* ------------------------------------------------------------

preserve

collapse (mean) wealth_graph [aw=hh_weight], by(age wealth_quar)

twoway line wealth_graph age if wealth_quar == 1, ///
    title("Low: 1st Quartile") ///
    ytitle("Net wealth") xtitle("Age") ///
    name(pq1, replace)

twoway line wealth_graph age if wealth_quar == 2, ///
    title("Low-Middle: 2nd Quartile") ///
    ytitle("Net wealth") xtitle("Age") ///
    name(pq2, replace)

twoway line wealth_graph age if wealth_quar == 3, ///
    title("High-Middle: 3rd Quartile") ///
    ytitle("Net wealth") xtitle("Age") ///
    name(pq3, replace)

twoway line wealth_graph age if wealth_quar == 4, ///
    title("High: 4th Quartile") ///
    ytitle("Net wealth") xtitle("Age") ///
    name(pq4, replace)

graph combine pq1 pq2 pq3 pq4, cols(2) ///
    name(parent_trimmed_raw, replace)

graph export "$figpath\Parent_wealth_byage_age_trimmed_graphcheck_2026_08_05.pdf", replace

restore

* ------------------------------------------------------------
* FIGURE 2: Smoothed age-specific trimmed parent profiles
* ------------------------------------------------------------

preserve

collapse (mean) wealth_graph [aw=hh_weight], by(age wealth_quar)

twoway lowess wealth_graph age if wealth_quar == 1, ///
    title("Low: 1st Quartile") ///
    ytitle("Net wealth") xtitle("Age") ///
    name(spq1, replace)

twoway lowess wealth_graph age if wealth_quar == 2, ///
    title("Low-Middle: 2nd Quartile") ///
    ytitle("Net wealth") xtitle("Age") ///
    name(spq2, replace)

twoway lowess wealth_graph age if wealth_quar == 3, ///
    title("High-Middle: 3rd Quartile") ///
    ytitle("Net wealth") xtitle("Age") ///
    name(spq3, replace)

twoway lowess wealth_graph age if wealth_quar == 4, ///
    title("High: 4th Quartile") ///
    ytitle("Net wealth") xtitle("Age") ///
    name(spq4, replace)

graph combine spq1 spq2 spq3 spq4, cols(2) ///
    name(parent_trimmed_lowess, replace)

graph export "$figpath\Parent_wealth_byage_age_trimmed_lowess_2026_08_05.pdf", replace

restore


**# 11) linear extrapolation to age 100 based on LOWESS trimmed profiles

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta" , clear

* 1. Age-specific p1/p99 trimming for graphing
gen wealth = net_wealth

bysort age: egen p99_age = pctile(wealth), p(99)
bysort age: egen p1_age  = pctile(wealth), p(1)

gen wealth_graph = wealth
replace wealth_graph = . if wealth > p99_age & !missing(wealth)
replace wealth_graph = . if wealth < p1_age  & !missing(wealth)

* 2. Collapse mean by age and quartile
collapse (mean) wealth_graph [aw=hh_weight], by(wealth_quar age)

* 3. LOWESS smoothing by quartile
gen lowess_y = .

levelsof wealth_quar, local(qs)

foreach q of local qs {
    lowess wealth_graph age if wealth_quar == `q', gen(lowess_temp) nograph
    replace lowess_y = lowess_temp if wealth_quar == `q'
    drop lowess_temp
}

gen extrapolated = 0

tempfile observed extrapolated
save `observed', replace

* 4. Extrapolate linearly to age 100 using last 10 years
postfile handle wealth_quar age lowess_y extrapolated using `extrapolated', replace

levelsof wealth_quar, local(qs)

foreach q of local qs {

    quietly summarize age if wealth_quar == `q'
    local maxage = r(max)

    quietly regress lowess_y age if wealth_quar == `q' & age >= `maxage' - 10

    local b1 = _b[age]

    quietly summarize lowess_y if wealth_quar == `q' & age == `maxage'
    local last_y = r(mean)

    post handle (`q') (`maxage') (`last_y') (1)

    local start = `maxage' + 1

    forvalues a = `start'(1)100 {
        local pred = `last_y' + `b1' * (`a' - `maxage')
        post handle (`q') (`a') (`pred') (1)
    }
}

postclose handle

use `observed', clear
append using `extrapolated'

* 5. Plot observed and extrapolated LOWESS trajectories

twoway ///
    (line lowess_y age if wealth_quar == 1 & extrapolated == 0, sort) ///
    (line lowess_y age if wealth_quar == 1 & extrapolated == 1, sort lpattern(dash)), ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low: 1st Quartile") ///
    legend(off) ///
    name(g1_noci, replace)

twoway ///
    (line lowess_y age if wealth_quar == 2 & extrapolated == 0, sort) ///
    (line lowess_y age if wealth_quar == 2 & extrapolated == 1, sort lpattern(dash)), ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("Low-Middle: 2nd Quartile") ///
    legend(off) ///
    name(g2_noci, replace)

twoway ///
    (line lowess_y age if wealth_quar == 3 & extrapolated == 0, sort) ///
    (line lowess_y age if wealth_quar == 3 & extrapolated == 1, sort lpattern(dash)), ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High-Middle: 3rd Quartile") ///
    legend(off) ///
    name(g3_noci, replace)

twoway ///
    (line lowess_y age if wealth_quar == 4 & extrapolated == 0, sort) ///
    (line lowess_y age if wealth_quar == 4 & extrapolated == 1, sort lpattern(dash)), ///
    ytitle("Net wealth") ///
    xtitle("Age") ///
    title("High: 4th Quartile") ///
    legend(off) ///
    name(g4_noci, replace)

* Combine
graph combine g1_noci g2_noci g3_noci g4_noci, cols(2)

sort wealth_quar age

export excel wealth_quar age lowess_y extrapolated ///
    using "$resultspath\Parent_wealth_lowess_trimmed_extrapolated100_last5_2026_20_05.xlsx", ///
    firstrow(variables) replace

graph export "$resultspath\Figures\Parent_wealth_byage_lowess_trimmed_extrapolated100_last10_2026_08_05.pdf", replace

* ============================================================
* 12) LOWESS fan chart — observed + extrapolated to age 100 (PARENT)
* ============================================================

* Bands show the cross-sectional spread of individual households around the LOWESS line.
* Residuals (actual wealth minus LOWESS trajectory) are computed at individual level,
* trimmed at p5/p95 within each quartile to limit outlier influence, then percentiles
* of those residuals are taken at each age and added back onto the LOWESS line.
* The five shaded layers = p5/p95, p10/p90, p20/p80, p30/p70, p40/p60 of the residual
* distribution — innermost band contains middle 20% of households, outermost middle 90%.
* The LOWESS line itself is unchanged from the original collapsed-mean smoothing.
* Extrapolation uses linear projection from last 10 observed years, applied separately
* to lp_y and each band bound.

use "$datapath\parent_wealth_traj_cleaned_2026_04_28.dta", clear

* 1. Trim
gen wealth = net_wealth
bysort age: egen p99_age = pctile(wealth), p(99)
bysort age: egen p1_age  = pctile(wealth), p(1)
gen wealth_graph = wealth
replace wealth_graph = . if wealth > p99_age & !missing(wealth)
replace wealth_graph = . if wealth < p1_age  & !missing(wealth)

tempfile indiv
save `indiv', replace

* 2. Collapse then LOWESS — identical to block 11
collapse (mean) wealth_graph [aw=hh_weight], by(age wealth_quar)

gen lp_y = .
levelsof wealth_quar, local(qs)
foreach q of local qs {
    lowess wealth_graph age if wealth_quar == `q', gen(lowess_temp) nograph
    replace lp_y = lowess_temp if wealth_quar == `q'
    drop lowess_temp
}

tempfile traj
keep age wealth_quar lp_y
save `traj', replace

* 3. Merge trajectory back to individual level
use `indiv', clear
merge m:1 age wealth_quar using `traj', nogen

* 4. Residuals, trim extremes per quartile
gen resid = wealth_graph - lp_y
levelsof wealth_quar, local(qs)
foreach q of local qs {
    quietly summarize resid if wealth_quar == `q', detail
    replace resid = . if wealth_quar == `q' & (resid < r(p5) | resid > r(p95))
}

* 5. Percentile bands
foreach p in 5 10 20 30 40 {
    local p2 = 100 - `p'
    bysort age wealth_quar: egen resid_lo`p' = pctile(resid), p(`p')
    bysort age wealth_quar: egen resid_hi`p' = pctile(resid), p(`p2')
    gen lo`p' = lp_y + resid_lo`p'
    gen hi`p' = lp_y + resid_hi`p'
}

* 6. Collapse
collapse (mean) lp_y lo5 hi5 lo10 hi10 lo20 hi20 lo30 hi30 lo40 hi40 ///
    [aw=hh_weight], by(age wealth_quar)
gen extrapolated = 0

tempfile observed extrapolated
save `observed', replace

* 7. Extrapolate all bands to age 100 using last 10 years
postfile handle wealth_quar age lp_y ///
    lo5 hi5 lo10 hi10 lo20 hi20 lo30 hi30 lo40 hi40 ///
    extrapolated using `extrapolated', replace

levelsof wealth_quar, local(qs)
foreach q of local qs {
    quietly summarize age if wealth_quar == `q'
    local maxage    = r(max)
    local win_start = `maxage' - 10

    foreach s in lp_y lo5 hi5 lo10 hi10 lo20 hi20 lo30 hi30 lo40 hi40 {
        quietly regress `s' age if wealth_quar == `q' & age >= `win_start'
        local b_`s' = _b[age]
        quietly summarize `s' if wealth_quar == `q' & age == `maxage'
        local last_`s' = r(mean)
    }

    post handle (`q') (`maxage') ///
        (`last_lp_y') ///
        (`last_lo5') (`last_hi5') ///
        (`last_lo10') (`last_hi10') ///
        (`last_lo20') (`last_hi20') ///
        (`last_lo30') (`last_hi30') ///
        (`last_lo40') (`last_hi40') (1)

    local start = `maxage' + 1
    forvalues a = `start'(1)100 {
        local d = `a' - `maxage'
        foreach s in lp_y lo5 hi5 lo10 hi10 lo20 hi20 lo30 hi30 lo40 hi40 {
            local pred_`s' = `last_`s'' + `b_`s'' * `d'
        }
        post handle (`q') (`a') ///
            (`pred_lp_y') ///
            (`pred_lo5') (`pred_hi5') ///
            (`pred_lo10') (`pred_hi10') ///
            (`pred_lo20') (`pred_hi20') ///
            (`pred_lo30') (`pred_hi30') ///
            (`pred_lo40') (`pred_hi40') (1)
    }
}
postclose handle

use `observed', clear
append using `extrapolated'

* 8. Fan chart plots
foreach q in 1 2 3 4 {

    if `q' == 1 local col blue
    if `q' == 2 local col red
    if `q' == 3 local col green
    if `q' == 4 local col orange

    if `q' == 1 local ttl "Low: 1st Quartile"
    if `q' == 2 local ttl "Low-Middle: 2nd Quartile"
    if `q' == 3 local ttl "High-Middle: 3rd Quartile"
    if `q' == 4 local ttl "High: 4th Quartile"

    twoway ///
        (rarea lo5  hi5  age if wealth_quar==`q' & extrapolated==0, sort color(`col'%12) lwidth(none)) ///
        (rarea lo10 hi10 age if wealth_quar==`q' & extrapolated==0, sort color(`col'%15) lwidth(none)) ///
        (rarea lo20 hi20 age if wealth_quar==`q' & extrapolated==0, sort color(`col'%20) lwidth(none)) ///
        (rarea lo30 hi30 age if wealth_quar==`q' & extrapolated==0, sort color(`col'%25) lwidth(none)) ///
        (rarea lo40 hi40 age if wealth_quar==`q' & extrapolated==0, sort color(`col'%30) lwidth(none)) ///
        (rarea lo5  hi5  age if wealth_quar==`q' & extrapolated==1, sort color(`col'%6)  lwidth(none)) ///
        (rarea lo10 hi10 age if wealth_quar==`q' & extrapolated==1, sort color(`col'%8)  lwidth(none)) ///
        (rarea lo20 hi20 age if wealth_quar==`q' & extrapolated==1, sort color(`col'%10) lwidth(none)) ///
        (rarea lo30 hi30 age if wealth_quar==`q' & extrapolated==1, sort color(`col'%12) lwidth(none)) ///
        (rarea lo40 hi40 age if wealth_quar==`q' & extrapolated==1, sort color(`col'%15) lwidth(none)) ///
        (line lp_y age if wealth_quar==`q' & extrapolated==0, sort lcolor(`col') lwidth(medthick)) ///
        (line lp_y age if wealth_quar==`q' & extrapolated==1, sort lcolor(`col') lwidth(medthick) lpattern(dash)), ///
        ytitle("Net wealth") xtitle("Age") ///
        title("`ttl'") legend(off) ///
        name(p13_lq`q', replace)
}

graph combine p13_lq1 p13_lq2 p13_lq3 p13_lq4, cols(2) ///
    name(parent_lowess_extrapolated100, replace)

graph export "$resultspath\Figures\Parent_wealth_byage_lowess_trimmed_extrapolated100_last10_2026_26_05.pdf", replace

gen quartile_label = ""
replace quartile_label = "1st Quartile" if wealth_quar == 1
replace quartile_label = "2nd Quartile" if wealth_quar == 2
replace quartile_label = "3rd Quartile" if wealth_quar == 3
replace quartile_label = "4th Quartile" if wealth_quar == 4

* Band variables — each pair is a percentile range of the residual distribution
* (i.e. the spread of households around the LOWESS line at each age):
*
*   lo5  / hi5   = p5  to p95 — widest band,  middle 90% of households
*   lo10 / hi10  = p10 to p90 —               middle 80% of households
*   lo20 / hi20  = p20 to p80 —               middle 60% of households
*   lo30 / hi30  = p30 to p70 —               middle 40% of households
*   lo40 / hi40  = p40 to p60 — narrowest band, middle 20% of households
*
*   lp_y sits at the centre of all bands (the LOWESS trajectory)
*   In the fan chart: outermost layer = lightest, innermost = darkest

export excel quartile_label wealth_quar age lp_y ///
    lo5 hi5 lo10 hi10 lo20 hi20 lo30 hi30 lo40 hi40 extrapolated ///
    using "$resultspath\Parent_wealth_byage_lowess_fan_extrapolated100_2026_26_05.xlsx", ///
    firstrow(variables) replace