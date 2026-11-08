// installing 

* ssc install heatplot
* ssc install palettes
* ssc install colrspace

********************************************************************************
** Intergen_Wealth_Transition.do
** Weighted version using hh_weight
********************************************************************************

clear all
set more off

* Paths
global projectpath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling"
global datapath    "$projectpath\Data"
global resultspath "$projectpath\Results"

cap mkdir "$resultspath"

********************************************************************************
** I. Load data
********************************************************************************

import excel "$datapath\df_long_format_everyone_2026_05_20.xlsx", ///
    firstrow clear
	
sort root_hhid wave

********************************************************************************
** II. ID and wave variables
********************************************************************************

encode root_hhid, gen(root_hhid_num)

gen survey_wave = .
replace survey_wave = 1 if wave == "w1"
replace survey_wave = 2 if wave == "w2"
replace survey_wave = 3 if wave == "w3"
replace survey_wave = 4 if wave == "w4"

label define survey_wave_lbl 1 "2010" 2 "2014" 3 "2017" 4 "2021", replace
label values survey_wave survey_wave_lbl

gen birthyear = survey_year - age
gen cohort10 = floor(birthyear/10)*10

duplicates tag root_hhid, gen(num_waves)
replace num_waves = num_waves + 1

sort root_hhid_num survey_year

********************************************************************************
** III. Wealth variables
********************************************************************************

drop if missing(net_wealth)
drop if missing(hh_weight) | hh_weight <= 0
drop if treated_gift == 1
* drop if age > 65

bysort root_hhid_num: egen wealth_avg = mean(net_wealth)

sort root_hhid_num survey_year
bysort root_hhid_num (survey_year): gen first_obs = _n == 1

bysort root_hhid_num (survey_year): gen age_first = age if _n == 1
bysort root_hhid_num (survey_year): replace age_first = age_first[1]

gen agefirst5 = floor(age_first/5)*5

gen wealth_quar_avg = .

levelsof agefirst5 if first_obs, local(agebins)

foreach a of local agebins {
    capture drop temp_q
    xtile temp_q = wealth_avg [pweight=hh_weight] ///
        if first_obs & agefirst5 == `a', nq(4)

    replace wealth_quar_avg = temp_q if first_obs & agefirst5 == `a'
}

bysort root_hhid_num (survey_year): replace wealth_quar_avg = wealth_quar_avg[1]
rename wealth_quar_avg wealth_quar

label define wealthq 1 "Child Q1" 2 "Child Q2" 3 "Child Q3" 4 "Child Q4", replace
label values wealth_quar wealthq

********************************************************************************
** IV. Parent wealth proxy via real inheritance
********************************************************************************

gen inh_1 = value_1 if type_transfer_1 == "Inheritance"
gen inh_2 = value_2 if type_transfer_2 == "Inheritance"
gen inh_3 = value_3 if type_transfer_3 == "Inheritance"

egen inheritance_person = rowtotal(inh_1 inh_2 inh_3)

gen has_inheritance = inheritance_person > 0 & !missing(inheritance_person)

xtile parent_quar_pos = inheritance_person [pweight=hh_weight] ///
    if has_inheritance, nq(4)

gen parent_group = 0 if inheritance_person == 0
replace parent_group = parent_quar_pos if has_inheritance

label define parentg ///
    0 "No inheritance" ///
    1 "Inheritance Q1" ///
    2 "Inheritance Q2" ///
    3 "Inheritance Q3" ///
    4 "Inheritance Q4", replace

label values parent_group parentg

********************************************************************************
** V. Collapse to one row per person
********************************************************************************

keep root_hhid_num country wealth_quar parent_group inheritance_person has_inheritance ///
     age_first wealth_avg num_waves first_obs hh_weight

bysort root_hhid_num: keep if _n == 1

keep if !missing(wealth_quar) & !missing(parent_group)

********************************************************************************
** VI. Checks
********************************************************************************

di "Number of individuals in final sample:"
count

di "Distribution of child wealth quartile:"
tab wealth_quar

di "Distribution of parent inheritance group:"
tab parent_group

di "Unweighted transition matrix:"
tab parent_group wealth_quar, row

********************************************************************************
** VII. Weighted transition matrix export
********************************************************************************

preserve

collapse (sum) cell_weight = hh_weight, by(parent_group wealth_quar)

fillin parent_group wealth_quar
replace cell_weight = 0 if missing(cell_weight)

sort parent_group wealth_quar

reshape wide cell_weight, i(parent_group) j(wealth_quar)

forvalues j = 1/4 {
    replace cell_weight`j' = 0 if missing(cell_weight`j')
}

egen row_total = rowtotal(cell_weight1 cell_weight2 cell_weight3 cell_weight4)

gen child_q1_share = cell_weight1 / row_total
gen child_q2_share = cell_weight2 / row_total
gen child_q3_share = cell_weight3 / row_total
gen child_q4_share = cell_weight4 / row_total

format child_q*_share %9.3f

export delimited parent_group child_q1_share child_q2_share child_q3_share child_q4_share ///
    using "$resultspath\intergen_wealth_transition_matrix_weighted.csv", replace

export excel parent_group child_q1_share child_q2_share child_q3_share child_q4_share ///
    using "$resultspath\intergen_wealth_transition_matrix_weighted.xlsx", ///
    firstrow(variables) replace

save "$resultspath\intergen_wealth_transition_matrix_weighted.dta", replace

restore

********************************************************************************
** VIII. Save person-level analysis file
********************************************************************************

save "$resultspath\intergen_wealth_transition_sample_weighted.dta", replace

********************************************************************************
** IX. Weighted heatmap
********************************************************************************

preserve

collapse (sum) cell_weight = hh_weight, by(parent_group wealth_quar)

fillin parent_group wealth_quar
replace cell_weight = 0 if missing(cell_weight)

sort parent_group wealth_quar

reshape wide cell_weight, i(parent_group) j(wealth_quar)

forvalues j = 1/4 {
    replace cell_weight`j' = 0 if missing(cell_weight`j')
}

egen row_total = rowtotal(cell_weight1 cell_weight2 cell_weight3 cell_weight4)

gen child_q1_share = cell_weight1 / row_total
gen child_q2_share = cell_weight2 / row_total
gen child_q3_share = cell_weight3 / row_total
gen child_q4_share = cell_weight4 / row_total

mkmat child_q1_share child_q2_share child_q3_share child_q4_share, matrix(share_mat)

matrix rownames share_mat = No_inheritance Inheritance_Q1 Inheritance_Q2 Inheritance_Q3 Inheritance_Q4
matrix colnames share_mat = Child_Q1 Child_Q2 Child_Q3 Child_Q4

heatplot share_mat, ///
    values(format(%4.2f)) ///
    cuts(0(.1)1) ///
    title("Weighted Inheritance to Child Wealth Transition Matrix") ///
    xtitle("Child Wealth Quartile") ///
    ytitle("Inheritance Group") ///
    color(hcl Blues, reverse)

graph export "$resultspath\transition_matrix_heatmap_weighted.png", replace

restore

di as result "Done. Weighted outputs saved in: $resultspath"


********************************************************************************
** X. Weighted transition matrices: total + by country
********************************************************************************

use "$resultspath\intergen_wealth_transition_sample_weighted.dta", clear

levelsof country, local(countries)

local groups "TOTAL `countries'"

foreach c of local groups {

    preserve

    if "`c'" != "TOTAL" {
        keep if country == "`c'"
    }

    collapse (sum) cell_weight = hh_weight, by(parent_group wealth_quar)

    fillin parent_group wealth_quar
    replace cell_weight = 0 if missing(cell_weight)

    sort parent_group wealth_quar
    reshape wide cell_weight, i(parent_group) j(wealth_quar)

    forvalues j = 1/4 {
        replace cell_weight`j' = 0 if missing(cell_weight`j')
    }

    egen row_total = rowtotal(cell_weight1 cell_weight2 cell_weight3 cell_weight4)

    gen child_q1_share = cell_weight1 / row_total
    gen child_q2_share = cell_weight2 / row_total
    gen child_q3_share = cell_weight3 / row_total
    gen child_q4_share = cell_weight4 / row_total

    format child_q*_share %9.3f

    export excel parent_group child_q1_share child_q2_share child_q3_share child_q4_share ///
        using "$resultspath\intergen_wealth_transition_matrix_weighted_by_country.xlsx", ///
        sheet("`c'") firstrow(variables) sheetreplace

    mkmat child_q1_share child_q2_share child_q3_share child_q4_share, matrix(share_mat)

    matrix rownames share_mat = No_inh Inh_Q1 Inh_Q2 Inh_Q3 Inh_Q4
    matrix colnames share_mat = C_Q1 C_Q2 C_Q3 C_Q4

    heatplot share_mat, ///
        values(format(%4.2f) size(vsmall)) ///
        cuts(0(.1)1) ///
        title("`c'", size(small)) ///
        xtitle("Child wealth quartile", size(vsmall)) ///
        ytitle("Inheritance group", size(vsmall)) ///
        xlabel(, labsize(vsmall)) ///
        ylabel(, labsize(vsmall)) ///
        color(hcl Blues, reverse) ///
        name(hm_`c', replace)

    graph export "$resultspath\transition_matrix_heatmap_weighted_`c'.png", replace

    restore
}

graph combine hm_TOTAL hm_DE hm_FR hm_IT, ///
    cols(2) ///
    title("Weighted Transition Matrices", size(small)) ///
    xsize(8) ysize(6)

graph export "$resultspath\transition_matrix_heatmaps_weighted_total_by_country_2x2.png", replace