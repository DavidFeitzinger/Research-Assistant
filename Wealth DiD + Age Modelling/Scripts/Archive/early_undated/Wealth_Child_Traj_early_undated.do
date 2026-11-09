
*set directory for whole dofile

global datapath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling"

*set directory for whole dofile

global datapath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling\Data"
global resultspath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling\Results"


********************************************************************************
**#          		I. Cleaning and Variable Construction					  
********************************************************************************

import delimited "$datapath\df_long_format_everyone_2026_04_21.csv" , clear


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

decode survey_wave, gen(survey_year)
destring survey_year , replace

*birthyear and cohort

gen birthyear = survey_year - age
gen cohort10 = floor(birthyear/10)*10


*Number of waves per individual

duplicates report root_hhid
duplicates tag root_hhid , gen(num_waves)
replace num_waves=num_waves+1

sort root_hhid survey_wave

//Check number of waves across individual-waves and across individuals

//tab num_waves
/*
  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     45,508       54.59       54.59
          2 |     22,904       27.47       82.06
          3 |     10,344       12.41       94.47
          4 |      4,612        5.53      100.00
------------+-----------------------------------
      Total |     83,368      100.00
*/  

/*
preserve 

collapse num_waves , by(root_hhid_num)

tab num_waves
/*
  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     45,508       73.92       73.92
          2 |     11,452       18.60       92.53
          3 |      3,448        5.60       98.13
          4 |      1,153        1.87      100.00
------------+-----------------------------------
      Total |     61,561      100.00
*/
restore
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

drop if age > 70 //Note: not sure if to cap it at some age too?? Depends on our goal of until when to run the ABM in terms of child age. The further we estimate the ABM the worse the assumption gets that the child wealth trajectory is well estimated based on our 2010 - 2021 data

* drop if ... //Note: drop individual-waves from the point when they receive inheritance (include those with gifts because the ABM does not model that separately so we need to mix it up with wealth trajectory for now)



save "$datapath\child_wealth_traj_cleaned_2026_04_21.dta" , replace


xtset root_hhid_num survey_wave
xtdescribe
//n=54,186 individual-waves, n=40,219 individuals


/*
use "$datapath\parent_wealth_traj_cleaned_2026_04_21.dta" , clear

tab num_waves

  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     28,591       52.76       52.76
          2 |     15,039       27.75       80.52
          3 |      7,145       13.19       93.71
          4 |      3,411        6.29      100.00
------------+-----------------------------------
      Total |     54,186      100.00



collapse num_waves , by(root_hhid_num)

tab num_waves

  num_waves |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     28,591       71.09       71.09
          2 |      8,003       19.90       90.99
          3 |      2,655        6.60       97.59
          4 |        970        2.41      100.00
------------+-----------------------------------
      Total |     40,219      100.00
*/




























