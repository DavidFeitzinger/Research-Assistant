
// installing 

* ssc install heatplot
* ssc install palettes
* ssc install colrspace


********************************************************************************
** Intergen_Wealth_Transition.do
** Goal: build transition matrix from parent wealth proxy to child wealth quartile
********************************************************************************

clear all         // [NEW] Clear everything currently in memory
set more off      // [NEW] Prevent Stata from pausing output with "more"

* Paths
global projectpath "C:\Users\Gebruiker\OneDrive - Università Commerciale Luigi Bocconi\Master 2\Semester II\RA\Wealth DiD + Age Modelling"   // [NEW] Main project folder
global datapath    "$projectpath\Data"       // [NEW] Folder containing input data
global resultspath "$projectpath\Results"    // [NEW] Folder where outputs will be saved

cap mkdir "$resultspath"        // [NEW] Create results folder if it does not already exist

********************************************************************************
** I. Load data
********************************************************************************

import delimited "$datapath\df_long_format_everyone_2026_04_29.csv", clear   // [NEW] Import the long-format CSV dataset and replace anything in memory

sort root_hhid wave             // [FROM DOFILES] Sorting structure follows the dofiles

********************************************************************************
** II. ID and wave variables
********************************************************************************

* Numeric household/person ID
encode root_hhid, gen(root_hhid_num)   // [FROM DOFILES] Convert string ID into numeric ID

* Survey wave
gen survey_wave = .     // [FROM DOFILES] Create numeric survey wave variable
replace survey_wave = 1 if wave == "w1"   // [FROM DOFILES] Code wave 1
replace survey_wave = 2 if wave == "w2"   // [FROM DOFILES] Code wave 2
replace survey_wave = 3 if wave == "w3"   // [FROM DOFILES] Code wave 3
replace survey_wave = 4 if wave == "w4"   // [FROM DOFILES] Code wave 4

label define survey_wave_lbl 1 "2010" 2 "2014" 3 "2017" 4 "2021", replace   // [FROM DOFILES] Define wave-year labels
label values survey_wave survey_wave_lbl   // [FROM DOFILES] Attach labels to survey_wave

*decode survey_wave, gen(survey_year)   // [FROM DOFILES] Convert labelled wave to year string
*destring survey_year, replace          // [FROM DOFILES] Convert year string to numeric

* Birth year and cohort
gen birthyear = survey_year - age      // [FROM DOFILES] Approximate birth year
gen cohort10 = floor(birthyear/10)*10  // [FROM DOFILES] Create decade birth cohort

* Number of waves per individual
duplicates tag root_hhid, gen(num_waves)   // [FROM DOFILES] Count repeated observations by ID
replace num_waves = num_waves + 1          // [FROM DOFILES] Convert duplicate count into number of waves

sort root_hhid_num survey_year        // [FROM DOFILES] Sort by person ID and year

********************************************************************************
** III. Wealth variables
********************************************************************************

* Drop observations with missing wealth
drop if net_wealth == .
drop if treated_gift == 1
*drop if age > 65

* Average observed wealth per individual
bysort root_hhid_num: egen wealth_avg = mean(net_wealth)

* First observed age
sort root_hhid_num survey_year
bysort root_hhid_num (survey_year): gen first_obs = _n == 1

bysort root_hhid_num (survey_year): gen age_first = age if _n == 1
bysort root_hhid_num (survey_year): replace age_first = age_first[1]

gen agefirst5 = floor(age_first/5)*5

* Child wealth quartile based on average wealth, age-adjusted
egen wealth_quar_avg = xtile(wealth_avg) if first_obs, by(agefirst5) nq(4)
bysort root_hhid_num (survey_year): replace wealth_quar_avg = wealth_quar_avg[1]
rename wealth_quar_avg wealth_quar

label define wealthq 1 "Child Q1" 2 "Child Q2" 3 "Child Q3" 4 "Child Q4", replace
label values wealth_quar wealthq

********************************************************************************
** IV. Parent wealth proxy via real inheritance only
********************************************************************************

* Construct amount of real inheritance only
* exclude gifts here, following logic for the ABM shock

gen inh_1 = value_1 if type_transfer_1 == "Inheritance"   // [NEW] Keep transfer 1 amount only if it is real inheritance
gen inh_2 = value_2 if type_transfer_2 == "Inheritance"   // [NEW] Keep transfer 2 amount only if it is real inheritance
gen inh_3 = value_3 if type_transfer_3 == "Inheritance"   // [NEW] Keep transfer 3 amount only if it is real inheritance

* Sum real inheritance amounts across transfer slots
egen inheritance_person = rowtotal(inh_1 inh_2 inh_3)   // [NEW] Sum inheritance amounts across slots

* Set zero to missing so only real inheritors enter parent quartiles
replace inheritance_person = . if inheritance_person == 0   // [NEW] Treat zero inheritance as missing

* Indicator for having any real inheritance
gen has_inheritance = !missing(inheritance_person)   // [NEW] Flag real inheritors

* Build parent proxy quartiles from real inheritance amount
egen parent_quar = xtile(inheritance_person) if has_inheritance, nq(4)   // [NEW] Create parent proxy quartile from inheritance amount

label define parentq 1 "Parent Q1" 2 "Parent Q2" 3 "Parent Q3" 4 "Parent Q4", replace   // [NEW] Define labels for parent quartiles
label values parent_quar parentq   // [NEW] Attach labels to parent_quar

********************************************************************************
** V. Collapse to one row per person
********************************************************************************

keep root_hhid_num wealth_quar parent_quar inheritance_person has_inheritance ///
     age_first wealth_avg num_waves first_obs

bysort root_hhid_num: keep if _n == 1   // [ADAPTED] Person-level collapse for this task

* Keep only people with both child wealth quartile and parent proxy quartile
keep if !missing(wealth_quar) & !missing(parent_quar)   // [NEW] Restrict to usable transition sample

********************************************************************************
** VI. Checks
********************************************************************************

di "Number of individuals in final sample:"   // [NEW] Header for sample size check
count                                         // [NEW] Count final sample size

di "Distribution of child wealth quartile:"   // [NEW] Header for child quartile distribution
tab wealth_quar                               // [NEW] Tabulate child quartiles

di "Distribution of parent proxy quartile:"   // [NEW] Header for parent quartile distribution
tab parent_quar                               // [NEW] Tabulate parent quartiles

di "Transition matrix with row shares:"       // [NEW] Header for transition matrix
tab parent_quar wealth_quar, row              // [NEW] Display parent-child transition matrix with row shares

********************************************************************************
** VII. Export transition matrix
********************************************************************************

preserve   // [NEW] Preserve current dataset before creating export table

tabulate parent_quar wealth_quar, matcell(freq_mat)   // [NEW] Store transition frequencies in matrix

matrix rowtot = J(rowsof(freq_mat), 1, .)   // [NEW] Create empty matrix for row totals

forvalues i = 1/4 {                         // [NEW] Loop over parent quartile rows
    scalar rsum = freq_mat[`i',1] + freq_mat[`i',2] + freq_mat[`i',3] + freq_mat[`i',4]   // [NEW] Compute row total
    matrix rowtot[`i',1] = rsum            // [NEW] Save row total
}

matrix share_mat = J(4,4,.)                // [NEW] Create empty matrix for row shares
forvalues i = 1/4 {                        // [NEW] Loop over rows
    forvalues j = 1/4 {                    // [NEW] Loop over columns
        matrix share_mat[`i',`j'] = freq_mat[`i',`j'] / rowtot[`i',1]   // [NEW] Convert counts into row shares
    }
}

clear                                      // [NEW] Clear memory to create export dataset
svmat share_mat, names(share_mat)          // [NEW] Convert matrix into variables

gen parent_quartile = _n                   // [NEW] Create parent quartile row identifier
order parent_quartile share_mat1 share_mat2 share_mat3 share_mat4   // [NEW] Reorder variables

rename share_mat1 child_q1_share           // [NEW] Rename first child share column
rename share_mat2 child_q2_share           // [NEW] Rename second child share column
rename share_mat3 child_q3_share           // [NEW] Rename third child share column
rename share_mat4 child_q4_share           // [NEW] Rename fourth child share column

format child_q*_share %9.3f                // [NEW] Format shares to three decimals

export delimited using "$resultspath\intergen_wealth_transition_matrix.csv", replace   // [NEW] Export transition matrix as CSV

save "$resultspath\intergen_wealth_transition_matrix.dta", replace   // [NEW] Save transition matrix as Stata file

restore   // [NEW] Restore person-level dataset

********************************************************************************
** VIII. Save person-level analysis file
********************************************************************************

save "$resultspath\intergen_wealth_transition_sample.dta", replace   // [NEW] Save final person-level sample

di as result "Done. Outputs saved in: $resultspath"   // [NEW] Print completion message


********************************************************************************
** IX. Visualise transition matrix (heatmap style)
********************************************************************************

preserve

tabulate parent_quar wealth_quar, matcell(freq_mat)

matrix rowtot = J(rowsof(freq_mat), 1, .)

forvalues i = 1/4 {
    scalar rsum = freq_mat[`i',1] + freq_mat[`i',2] + freq_mat[`i',3] + freq_mat[`i',4]
    matrix rowtot[`i',1] = rsum
}

matrix share_mat = J(4,4,.)
forvalues i = 1/4 {
    forvalues j = 1/4 {
        matrix share_mat[`i',`j'] = freq_mat[`i',`j'] / rowtot[`i',1]
    }
}

matrix rownames share_mat = Parent_Q1 Parent_Q2 Parent_Q3 Parent_Q4
matrix colnames share_mat = Child_Q1 Child_Q2 Child_Q3 Child_Q4

heatplot share_mat, ///
    values(format(%4.2f)) ///
    cuts(0(.1)1) ///
    title("Parent to Child Wealth Transition Matrix") ///
    xtitle("Child Wealth Quartile") ///
    ytitle("Parent Wealth Quartile") ///
    color(hcl Blues, reverse)
	
graph export "$resultspath\transition_matrix_heatmap.png", replace

restore