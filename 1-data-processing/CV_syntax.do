********************************************************************************
********************************************************************************

clear all
global beg_path `"C:\XXX\XXX\XXX\CV_Replication\"' // Set the path of the working directory here 
set more off

**************************
**** Data Preparation **** 
**************************

set maxvar 10000, perm

*Uploading our dataset with turnout  
use  `"${beg_path}Comp_voting_new.dta"', clear

*Adding Quality of Government 
merge m:1 country year using `"${beg_path}Q_G_for_merge.dta"'
drop if _merge ==2 
drop _merge cname

*Adding V-dem
merge m:1 country year using `"${beg_path}V_Dem_v10_for_merge.dta"' 
drop if _merge ==2 
drop _merge 

*Adding state capacity from Hanson and Sigman 2021
merge m:1 country year using `"${beg_path}HansonSigman_source_extract.dta"'
drop if _merge ==2 
drop _merge 

*Adding Sanctions
merge 1:1 country year EL_TYPE multiple_el_id using `"${beg_path}Sanctions.dta"' 
drop if _merge ==2 
gen fine_GDP = monetaryfine_USD_2011/(gdp_pc/1000) // Fine standardized by GDP per capita in thousands of dollars 
gen max_fine_GDP = maximummonetaryfine_USD_2011/(gdp_pc/1000) // Maximal fine standardized by GDP per capita in thousands of dollars 
 
*Coding the sanctions variable as zero for countries that do not enforce CV 
replace maximummonetaryfine_salary = maximummonetaryfine_salary*CV_enforced if CV_enforced !=0
replace monetaryfine_salary = monetaryfine_salary*CV_enforced if CV_enforced !=0
replace fine_GDP  = maximummonetaryfine_salary*CV_enforced if CV_enforced !=0
replace max_fine_GDP  = max_fine_GDP*CV_enforced if CV_enforced !=0
replace non_monetary_sanct = non_monetary_sanct*CV_enforced if CV_enforced !=0

replace maximummonetaryfine_salary = 0 if CV_enforced==0
replace monetaryfine_salary = 0 if CV_enforced ==0
replace fine_GDP  = 0 if CV_enforced ==0
replace max_fine_GDP  = 0 if CV_enforced ==0
replace non_monetary_sanct = 0 if CV_enforced ==0

*Setting the TSCS structure
sort country year EL_TYPE multiple_el_id // Sorting observations by country, year, election type, and election date (if two elections of the same type held in the same year)
bysort country: gen el_num = _n // Generating variable election number by country 
tsset country el_num // Setting the TSCS structre 

*Combining variables Electoral System and Election Type
gen el_system = .
replace el_system = 4 if EL_TYPE==1 // Presidential elections
replace el_system = v2elparlel if EL_TYPE==0 // Parliamentary elections with different types of electoral systems
cap fre el_system 

//---------------------------------

*Completing the missing values using the literature
replace el_system = 2  if country == 10 & el_system ==. & EL_TYPE==0 & year == 2006 // Bolivia 2006 (Based on https://aceproject.org/regions-en/countries-and-territories/BO/reports/bolivia-constituent-assembly-election-and)
replace el_system = 1 if country == 2 & el_system ==. & EL_TYPE==0 // Argentina 1973-1999 (Based on Nohlen)
replace el_system = 0 if country == 21 & year == 1946 & EL_TYPE==0 // Costa Rica 1946 (Based on Nohlen)
replace el_system = 1 if country == 29 & year ==1997 & EL_TYPE==0 // Ecuador 1997 (Based on Nohlen)
replace el_system = 0 if country == 105 & inlist(year, 1965, 1968) & EL_TYPE==0 // Sudan (Based on Nohlen)
label define el_system 0"El. System: Majoritarian" 1"El. System: Proportional" 2"El. System: Mixed" 3"El. System: Other" 4"Presidential Election"
label values el_system el_system

*Creating year since 1945
gen y_since_1945 = year-1944 // Generating variable year since 1945 

*Creating a version of unsanctioned CV in which sanctioned CV is not nested 
gen CV_2 = CV 
replace CV_2 = 0 if CV_enforced!=0
lab var CV_2 "CV Unsanctioned"
lab var CV_enforced "CV Sanctioned"

*Generating turnout as a proportion (for some of the analyses) 
gen turnout = Turnout /100
lab var turnout "Turnout as a proportion"

//--------------------------------------

* Ln transformation of state capacity 
gen log_wdi_taxrev = ln(wdi_taxrev)
lab var log_wdi_taxrev "Tax revenue (\% of GDP, ln)"
 
*Labeling some of the key variables 
lab var monetaryfine_salary "Fine (share of monthly salary)"
lab var fine_GDP  "Fine (share of GDP in \textdollar1,000)"
lab var maximummonetaryfine_salary "Max Fine (share of monthly salary)"
lab var max_fine_GDP "Max Fine (share of GDP in \textdollar1,000)"
lab var non_monetary_sanct "Non-monetary sanction"
lab var wdi_mortnn "Neonatal mortality" 
lab var Decisiveness "Majority Status"
lab var Closeness "Closeness"
lab var Concurrent "Concurrent Election"
lab var CV "Compulsory Voting"
lab var CV_enforced "CV Sanctioned"
lab var Voting_age "Voting Age"
lab var EL_TYPE "Presidential Election" 
lab var log_el_size  "Electorate Size (ln)"
lab var log_gdp_pc "GDP p/c (ln)" 
lab var year "Year" 
lab var y_since_1945 "Year Since 1945" 

*Creating macros with independent variables 
global decades "decade_1940s decade_1950s decade_1960s decade_1970s decade_1980s decade_1990s decade_2000s" 
global new "pre_1974_new post_1974_new postcommunist_new "
global institutions "Decisiveness Closeness Concurrent ib1.el_system log_el_size "  

 
***********************************
**** Info for the introduction **** 
***********************************
 
*To identify countries that at any time in their history applied CV or CV_enforced
egen CV_ever = mean(CV), by(country) 
egen CV_e_ever = mean(CV_enforced), by(country) 


*CV countries in our dataset 
xtab country if CV_ever >0 // 26 countries 
* Switches
xtab country if CV_ever >0 & CV_ever <1 // 11 countries 
*CV in the 2010 
xtab country if CV >0  & year>2010 // 18 countries
 
********************************************************************************************
**** Figures 1-3: Does Turnout Chandge when CV (or sanctions) is (are) adopted/removed? **** 
********************************************************************************************

cap set scheme bw
preserve
sort country EL_TYPE year 
keep if (((CV==0 & CV[_n+1] ==1) | (CV==1 & CV[_n+1] ==0)) & country==country[_n+1] & EL_TYPE==EL_TYPE[_n+1]) | ///
(((CV==0 & CV[_n-1] ==1) | (CV==1 & CV[_n-1] ==0)) & country==country[_n-1] & EL_TYPE==EL_TYPE[_n-1])
list country year EL_TYPE CV CV_enforced Turnout
gen turnout_change = Turnout-Turnout[_n-1] if country==country[_n-1] & EL_TYPE==EL_TYPE[_n-1]
list turnout_change country year EL_TYPE CV CV_enforced Turnout
drop if inlist(country, 25, 113) // Czechoslovakia does not count (elections in 1946 and 1990), Turkey does not count (a military coup in between)
gen enf_dummy = "ne"
replace enf_dummy = "e" if CV_enforced == 1
gen turnout_baseline = Turnout[_n-1]
format turnout_baseline %9.2f
fre turnout_baseline
scalar b=turnout_baseline
fre turnout_baseline
decode country, gen(temp1)
decode EL_TYPE, gen(temp2)
tostring year, gen(temp3)
gen country_type = temp1+","+temp2+" "+temp3+", "+enf_dummy


*Adoption of CV

list turnout_change country year EL_TYPE CV CV_enforced Turnout

graph hbar turnout_change if CV==1 &  turnout_change!=.,  ///
   over(country_type, label(labsize(vsmall)) relabel(1 `" "{bf:Bulgaria}: legislative 2017 (CVU)" "turnout in 2014: 51.1 %" "' ///
   2 `" "{bf:Bulgaria}: presidential 2016 (CVU)" "turnout in 2011: 52.3 %" "' /// 
   3 `" "{bf:Cyprus}: legislative 1981 (CVS)" "turnout in 1976: 85.3 %" "'  /// 
    4 `" "{bf:Thailand}: legislative 2001 (CVU)" "turnout in 1996: 62.4 %" "'))    blabel(bar, size(vsmall)) ///
   ylabel(-44(4)44, labsize(vsmall)) yline(0, lpattern(dash)) ytitle("Change in voter turnout between the first post-reform and last pre-reform (t-1) election", size(vsmall) margin(medsmall)) 

cap mkdir `"${beg_path}Results"'	
cd `"${beg_path}Results"' //  
graph export "Figure_3.eps", replace  // 


*Removal of CV

graph hbar turnout_change if CV==0 & turnout_change!=.,  ///
   over(country_type, label(labsize(vsmall)) relabel(1 `" "{bf:Chile}: legislative 2013" "turnout in 2009: 86.7 % (CVS)" "' ///
   2 `" "{bf:Chile}: presidential 2013" "turnout in 2009: 87.2 % (CVS)"  "' /// 
   3 `" "{bf:Italy}: legislative 1994" "turnout in 1992: 87.3 % (CVU)" "'  /// 
    4 `" "{bf:Netherlands}: legislative 1971" "turnout in 1967: 94.9 % (CVS)" "'  /// 
    5 `" "{bf:Venezuela}: legislative 1993" "turnout in 1988: 81.7 % (CVS)" "' ///
	 6 `" "{bf:Venezuela}: presidential 1993" "turnout in 1988: 81.9 % (CVS)" "')  )  blabel(bar, size(vsmall)) ///
   ylabel(-44(4)44, labsize(vsmall)) yline(0, lpattern(dash)) ytitle("Change in voter turnout between the first post-reform (t) and last pre-reform (t-1) election", size(vsmall) margin(medsmall)) 

 
cd `"${beg_path}Results"' //  
graph export "Figure_1.eps", replace  // 
restore


*Removal of sanctions

preserve
sort country EL_TYPE year 
keep if CV==1 
keep if (((CV_enforced==0 & CV_enforced[_n+1] ==1) | (CV_enforced==1 & CV_enforced[_n+1] ==0)) & country==country[_n+1] & EL_TYPE==EL_TYPE[_n+1]) | ///
(((CV_enforced==0 & CV_enforced[_n-1] ==1) | (CV_enforced==1 & CV_enforced[_n-1] ==0)) & country==country[_n-1] & EL_TYPE==EL_TYPE[_n-1])
list country year EL_TYPE CV CV_enforced Turnout
gen turnout_change = Turnout-Turnout[_n-1] if country==country[_n-1] & EL_TYPE==EL_TYPE[_n-1] & CV_enforced[_n-1]==1 & CV==1
decode country, gen(temp1)
decode EL_TYPE, gen(temp2)
tostring year, gen(temp3)
gen country_type = temp1+","+temp2+" "+temp3
 
graph hbar turnout_change if turnout_change!=.,  ///
   over(country_type, label(labsize(vsmall)) relabel(1 `" "{bf:Cyprus}: legislative 2001 (CVU)" "turnout in 1996: 92.9 % (CVS)" "' ///
   2 `" "{bf:Cyprus}: presidential 2003 (CVU)" "turnout in 1998: 91.7 % (CVS)"  "' /// 
   3 `" "{bf:Greece}: legislative 2000 (CVU)" "turnout in 1996: 76.4 % (CVS)" "'  /// 
   4 `" "{bf:Honduras}: legislative 2005 (CVU)" "turnout in 2001: 66.6 % (CVS)" "'  /// 
   5 `" "{bf:Honduras}: legislative 2005 (CVU)" "turnout in 2001: 66.6 % (CVS)" "'))  /// 
   	 blabel(bar, size(vsmall)) ylabel(-44(4)44, labsize(vsmall)) yline(0, lpattern(dash)) ///
	 ytitle("Change in voter turnout between the first post-reform (t) and last pre-reform (t-1) election", size(vsmall) margin(medsmall))

cd `"${beg_path}Results"' //  
graph export "Figure_2.eps", replace  //  
restore

 
****************************************************
**** Table 2: Is Turnout higher in CV countries **** 
****************************************************

eststo clear
eststo M1: xtreg Turnout CV_2 CV_enforced, fe cluster(country) 
estadd local fixed "Yes" , replace
eststo M2: xtreg Turnout $decades $new $institutions CV_2 CV_enforced, fe cluster(country) 
estadd local fixed "Yes" , replace

list country year if el_system ==.


cd `"${beg_path}Results"'

esttab M1 M2 using Table_2.tex, one incelldelimite("\:")  stats(fixed N r2, labels("Country FE" "N" "R2") layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
"\multicolumn{1}{c}{$@$}" )  fmt(0 0 2)) replace b(2) se(2) wide staraux ///
		star(+ 0.1 * 0.05 ** 0.01 *** 0.001) eqlabels(none) order(CV_2 CV_enforced ///
		Decisiveness Closeness Concurrent 0.el_system 2.el_system 3.el_system 4.el_system log_el_size  pre_1974_new post_1974_new postcommunist_new  decade_1940s decade_1950s decade_1960s decade_1970s decade_1980s decade_1990s decade_2000s) ///
		drop(1.el_system) lab ///
		nolegend note("") alignment(d{10}) mtitle("Baseline" "Full Model") 

		
**********************************************************
**** Table 1 in the Appendix (Replication of Table 2) **** 
**********************************************************		
		
*Replications: GDP p/c, OLS
eststo clear
eststo M1: xtreg Turnout $decades $new $institutions CV_2 CV_enforced log_gdp_pc, fe cluster(country) 
estadd local fixed "Yes" , replace
eststo M2: reg Turnout CV_2 CV_enforced , vce(cluster country)
estadd local fixed "No" , replace
eststo M3: reg Turnout $decades $new $institutions CV_2 CV_enforced , vce(cluster country)
estadd local fixed "No" , replace		

preserve  // replication for Italy coded as a country with sanctioned CV
fre CV_2 CV_enforced if country == 51
replace CV_enforced = 1 if CV_2==1 & country == 51
replace CV_2 = 0 if country == 51
fre CV_2 CV_enforced if country == 51
eststo M4: xtreg Turnout $decades $new $institutions CV_2 CV_enforced, fe cluster(country) 	
estadd local fixed "Yes" , replace
eststo M5: xtreg Turnout $decades $institutions CV_2 CV_enforced if new==0, fe cluster(country) 	
estadd local fixed "Yes" , replace
restore 

esttab M1 M2 M3 M4 M5 using Table_A_1.tex, one incelldelimite("\:")  stats(fixed N r2, labels("Country FE" "N" "R2") layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
"\multicolumn{1}{c}{$@$}" )  fmt(0 0 2)) replace b(2) se(2) wide staraux ///
		star(+ 0.1 * 0.05 ** 0.01 *** 0.001) eqlabels(none) order(CV_2 CV_enforced ///
		Decisiveness Closeness Concurrent 0.el_system 2.el_system 3.el_system 4.el_system log_el_size log_gdp_pc pre_1974_new ///
		post_1974_new postcommunist_new  decade_1940s decade_1950s decade_1960s decade_1970s decade_1980s decade_1990s decade_2000s) ///
		drop(1.el_system) lab ///
		nolegend note("")  mtitle("FE - GDP p/c" "OLS - Baseline" "OLSE - Full Model" "Italy alt. coding" "Est. Dem. Only") alignment(d{10}) 
		

*******************************************
**** Table 3: Out of Sample Estimation **** 
*******************************************

gen l_turnout = logit(turnout) // Logit transformation of the dependent variable  

eststo M1: xtreg l_turnout $decades $institutions $new  ib5.e_regionpol if CV_ever==0, re cluster(country) 
estadd local random "Yes" , replace

esttab M1 using Table_3.tex, one incelldelimite("\:")  stats(random N r2_o, labels("Country RE" "N" "R2") layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{$@$}" "\multicolumn{1}{c}{$@$}")  fmt(0 0 2)) replace b(3) se(3) wide staraux ///
		star(+ 0.1 * 0.05 ** 0.01 *** 0.001) eqlabels(none) order(Decisiveness Closeness Concurrent 0.el_system 2.el_system 3.el_system 4.el_system log_el_size pre_1974_new post_1974_new postcommunist_new  decade_1940s decade_1950s decade_1960s decade_1970s decade_1980s decade_1990s decade_2000s ) ///
		 ///
		drop(5.e_regionpol 1.el_system) nomtitles lab ///
		nolegend note("") alignment(d{12}) 

			
// Out of sample estimation for countries with unsanctioned CV 
// The graph was not finally used (as requested by reviewers), but only reported verbally below Table 3
preserve
margins if CV_2==1, noesample post 	
matrix u = r(table)
gen u_coef = invlogit(u[1,1]) 
gen u_up = invlogit(u[5,1]) 
gen u_lp = invlogit(u[6,1]) 
fre   u_coef u_up u_lp
mean u_coef turnout if CV_2==1
matrix x = r(table)
gen mean_u = x[1,1]
collapse  u_coef u_up u_lp  mean_u
foreach var of varlist u_coef u_up u_lp  mean_u {
replace `var'= `var'*100
}
expand 2, gen(expanded)
replace u_coef = mean_u if expanded==0
replace u_up = . if expanded==0
replace u_lp =.  if expanded==0
gen id = _n
format  u_coef %9.1f
tw scatter u_coef id, mlabel(u_coef) || rcap  u_up u_lp id, xscale(r(.8 2.2)) xlabel(1`""Average in CVU""Elections""' 2`""Out-of-sample ""prediction (95 % CI)""' ) ///
ylabel(60(5)90) ylabel(,format(%9.0f))  title("Unsanctioned CV", size(small)) name(graph1, replace) ytitle(Turnout) ///
xtitle("") legend(off)
restore 

// Out of sample estimation for countries with sanctioned CV (graph not used in the end)
// The graph was not finally used (as requested by reviewers), but only reported verbally below Table 3
xtreg l_turnout $decades $institutions $new ib5.e_regionpol if CV_ever==0, re cluster(country) 
preserve
margins if CV_enforced==1, noesample post 	
matrix u = r(table)
gen u_coef = invlogit(u[1,1]) 
gen u_up = invlogit(u[5,1]) 
gen u_lp = invlogit(u[6,1]) 
fre   u_coef u_up u_lp
mean turnout if CV_enforced==1
matrix x = r(table)
gen mean_u = x[1,1]
collapse  u_coef u_up u_lp  mean_u
foreach var of varlist u_coef u_up u_lp  mean_u {
replace `var'= `var'*100
}
expand 2, gen(expanded)
replace u_coef = mean_u if expanded==0
replace u_up = . if expanded==0
replace u_lp =.  if expanded==0
gen id = _n
format  u_coef %9.1f
tw scatter u_coef id, mlabel(u_coef) || rcap  u_up u_lp id, xscale(r(.8 2.2)) xlabel(1`""Average in CVS""Elections""' 2`""Out-of-sample ""prediction (95 % CI)""' ) ///
ylabel(60(5)90) ylabel(,format(%9.0f))  title("Sanctioned CV", size(small)) name(graph2, replace) ytitle(Turnout) ///
xtitle("") legend(off)
restore 

graph combine graph1 graph2, ycommon ysize(2) xsize(3)
di 74.7-67.1 // 7.6		
di 86.0-71.6 // 14.4		
	
	
**********************************************
**** Table EA 6: Out of Sample Estimation **** 
**********************************************	
		
* Simulation documenting that our estimation strategy is better than available alternatives

frame create estimated trial number mse ae // Create an additional data frame "estimated" and specify the frame's variables
set seed 1234 // set a seed 

 forvalues i = 1/1000 {
preserve  
keep if CV_ever==0   
keep country 
duplicates drop
sample 30, count // Sample randomly 30 panels/countries for which we will estimate out-of-sample predictions 
fre country 
frame put country, into(sampled_panels)
restore

preserve 
frame sampled_panels: levelsof(country), sep(,) local(x) 
gen subset = 1 if inlist(country, `x') 
frame drop sampled_panels


reg l_turnout $decades ib5.e_regionpol if CV_ever==0 & subset ==.,  cluster(country) //  A naive OLS model including only regional and decade dummies
predict x_1_`i' 
gen predict_1_`i'  = invlogit(x_1_`i')*100  
gen a_error_1_`i' =  (Turnout - predict_1_`i')
su a_error_1_`i'
local ae = r(mean)
gen square_error_1_`i'  = (Turnout - predict_1_`i')^2
egen mean_square_error_1_`i'  = mean(square_error_1_`i') if subset ==1
su mean_square_error_1_`i'  
local mse = r(mean)
frame post estimated (1) (`i') (`mse') (`ae')

 
reg l_turnout $decades $institutions $new ib5.e_regionpol if CV_ever==0 & subset ==.,  cluster(country) //  An OLS model with all predictors from the fixed effects analysis
predict x_2_`i' 
gen predict_2_`i'  = invlogit(x_2_`i')*100  
gen a_error_2_`i' =  (Turnout - predict_2_`i')
su a_error_2_`i'
local ae = r(mean)
gen square_error_2_`i'  = (Turnout - predict_2_`i')^2
egen mean_square_error_2_`i'  = mean(square_error_2_`i') if subset ==1
su mean_square_error_2_`i'  
local mse = r(mean)
frame post estimated (2) (`i') (`mse') (`ae')


xtreg l_turnout $decades ib5.e_regionpol if CV_ever==0 & subset ==., re cluster(country) // A random effects estimator including only regional and decade dummies
predict x_3_`i' 
gen predict_3_`i'  = invlogit(x_3_`i')*100  
gen a_error_3_`i' =  (Turnout - predict_3_`i')
su a_error_3_`i'
local ae = r(mean)
gen square_error_3_`i'  = (Turnout - predict_3_`i')^2
egen mean_square_error_3_`i'  = mean(square_error_3_`i') if subset ==1
su mean_square_error_3_`i'  
local mse = r(mean)
frame post estimated (3) (`i') (`mse') (`ae')
 
xtreg l_turnout $decades $institutions $new  ib5.e_regionpol if CV_ever==0 & subset ==., re cluster(country) // A random effects estimator with all predictors 
predict x_4_`i' 
gen predict_4_`i'  = invlogit(x_4_`i')*100 
gen a_error_4_`i' =  (Turnout - predict_4_`i')
su a_error_4_`i'
local ae = r(mean)
gen square_error_4_`i'  = (Turnout - predict_4_`i')^2
egen mean_square_error_4_`i'  = mean(square_error_4_`i') if subset ==1
su mean_square_error_4_`i'  
local mse = r(mean)
frame post estimated (4) (`i') (`mse') (`ae')
 
restore 
}
 
// Results of the simulation 
frame estimated: tabstat mse ae, by(trial) save 
matrix x = ( r(Stat1) \ r(Stat2) \ r(Stat3)\r(Stat4)) 
matrix rowname x =  "OLS Dummies (Model 1)" "OLS Full (Model 2)" "RE Dummies (Model 3)" "RE Full (Model 4)"  
matrix colname x = "Mean Squared Error" "Arithmetic Error"
cd `"${beg_path}Results"'
esttab m(x, fmt(2 2)) using Table_A_6.tex, nomtitles replace 
		

******************************************
**** Table 4: Enforcement and Turnout **** 
******************************************	


eststo clear
eststo M1: xtreg Turnout $decades  $new $institutions CV_enforced c.Capacity c.CV_enforced#c.Capacity  CV_2 c.CV_2#c.Capacity, fe cluster(country)
estadd local fixed "Yes", replace
test _b[c.CV_enforced#c.Capacity] + _b[c.Capacity]  =0 // 0.0439
eststo M2: xtreg Turnout $decades  $new $institutions CV_enforced c.wdi_mortnn c.CV_enforced#c.wdi_mortnn CV_2 c.CV_2#c.wdi_mortnn, fe cluster(country)
estadd local fixed "Yes", replace
test _b[c.CV_enforced#c.wdi_mortnn] + _b[c.wdi_mortnn]  =0 //  0.0198
eststo M3: xtreg Turnout $decades  $new $institutions CV_enforced  CV_2  monetaryfine_salary, fe cluster(country)
estadd local fixed "Yes", replace
eststo M4: xtreg Turnout $decades  $new $institutions CV_enforced  CV_2  fine_GDP, fe cluster(country)
estadd local fixed "Yes", replace
eststo M5: xtreg Turnout $decades  $new $institutions CV_enforced  CV_2  maximummonetaryfine_salary, fe cluster(country)
estadd local fixed "Yes", replace
eststo M6: xtreg Turnout $decades  $new $institutions CV_enforced  CV_2 max_fine_GDP, fe cluster(country)
estadd local fixed "Yes", replace
eststo M7: xtreg Turnout $decades  $new $institutions CV_enforced CV_2 non_monetary_sanct, fe cluster(country)
estadd local fixed "Yes", replace

lab var Capacity "State Capacity"

esttab M1 M2 M3 M4 M5 M6 M7 using Table_4.tex, one incelldelimite("\:") stats(fixed N r2_o, labels("Country FE" "N" "R2") layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{$@$}")  fmt(0 0 2)) replace b(2) se(2) wide staraux ///
		star(+ 0.1 * 0.05 ** 0.01 *** 0.001) eqlabels(none) order( CV_enforced CV_2 c.CV_enforced#c.Capacity c.CV_2#c.Capacity Capacity c.CV_enforced#c.wdi_mortnn c.CV_2#c.wdi_mortnn wdi_mortnn monetaryfine_salary fine_GDP maximummonetaryfine_salary ///
		max_fine_GDP non_monetary_sanct Decisiveness Closeness Concurrent 0.el_system 2.el_system 3.el_system 4.el_system log_el_size pre_1974_new post_1974_new postcommunist_new  decade_1940s decade_1950s decade_1960s decade_1970s decade_1980s decade_1990s decade_2000s) ///
		varlabels( _cons Constant , elist(non_monetary_sanct \hline)) refcat(CV_enforced "\textit{Compulsory Voting \& Sanctions}" /// 
		Decisiveness "\textit{Controls}", nolabel) /// 
		drop( 1.el_system decade_1940s decade_1950s decade_1960s) lab  substitute("0.00 \: (.)" " " "0.00\:(.)" " " ) ///  
		nolegend note("") mgroups("State Capacity" "Sanctions", span prefix(\multicolumn{@span}{c}{) suffix(})  pattern(1 0 1 0 0 0 0) erepeat(\cmidrule(lr){@span})) mtitles("State Capacity" "Infant Mortality"  "Fine" "Fine" "Maximal Fine" "Maximal Fine" "Other Sanctions") alignment(d{10}) 

		
		
**************************************************
**** Table EA2: Robustness Checks for TAble 4 **** 
**************************************************

eststo clear 
eststo M1: xtreg Turnout $decades  $new $institutions  Capacity if CV_enforced==1, fe cluster(country)
estadd local fixed "Yes", replace
eststo M2: xtreg Turnout $decades  $new $institutions  wdi_mortnn if CV_enforced==1, fe cluster(country)
estadd local fixed "Yes", replace
eststo M3: xtreg Turnout $decades  $new $institutions  Capacity log_gdp_pc if CV_enforced==1, fe cluster(country)
estadd local fixed "Yes", replace
eststo M4: xtreg Turnout $decades  $new $institutions  wdi_mortnn log_gdp_pc if CV_enforced==1, fe cluster(country)
estadd local fixed "Yes", replace
preserve  // replication for Italy coded as country with enforced CV 
fre CV_2 CV_enforced if country == 51
replace CV_enforced = 1 if CV_2==1 & country == 51
replace CV_2 = 0 if  country == 51
fre CV_2 CV_enforced if country == 51
eststo M5: xtreg Turnout $decades  $new $institutions  Capacity if CV_enforced==1, fe cluster(country)
estadd local fixed "Yes", replace
eststo M6: xtreg Turnout $decades  $new $institutions  wdi_mortnn if CV_enforced==1, fe cluster(country)
estadd local fixed "Yes", replace
restore 
eststo M7: xtreg Turnout $decades  $new $institutions  Capacity if CV_enforced==1 & new==0, fe cluster(country)
estadd local fixed "Yes", replace
eststo M8: xtreg Turnout $decades  $new $institutions  wdi_mortnn if CV_enforced==1 & new==0, fe cluster(country)
estadd local fixed "Yes", replace


esttab M1 M2 M3 M4 M5 M6 M7 M8 using Table_A_2.tex, one incelldelimite("\:") stats(fixed N r2_o, labels("Country FE" "N" "R2") layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{$@$}")  fmt(0 0 2)) replace b(2) se(2) wide staraux ///
		star(+ 0.1 * 0.05 ** 0.01 *** 0.001) eqlabels(none) order(Capacity wdi_mortnn log_gdp_pc Decisiveness Closeness Concurrent 0.el_system 2.el_system 3.el_system 4.el_system log_el_size pre_1974_new post_1974_new postcommunist_new  decade_1940s decade_1950s decade_1960s decade_1970s decade_1980s decade_1990s decade_2000s) ///
		varlabels( _cons Constant) /// 
		drop( 1.el_system decade_1940s decade_1950s decade_1960s) lab  substitute("0.00 \: (.)" " " "0.00\:(.)" " " ) ///  
		nolegend note("") mtitles( "\shortstack{State Capacity\\}"   "\shortstack{Neonat. Mortality \\}" ///
		"\shortstack{State Capacity\\ Control for GDPpc}" "\shortstack{Neonat. Mortality\\Control for GDPpc}"  ///
		"\shortstack{State Capacity\\Italy Alt. Coding}"  "\shortstack{Neonat. Mortality\\Italy Alt. Coding}" ///
		"\shortstack{State Capacity\\Est. Dem. Only}"  "\shortstack{Neonat. Mortality\\Est. Dem. Only}") alignment(d{10}) 
		
		
*****************************************************************
**** Figure 4: Sanctioned Compulsory Voting & State Capacity **** 
*****************************************************************			
 	
*Graph 1
preserve
xtreg Turnout $decades $new $institutions CV_enforced c.Capacity c.CV_enforced#c.Capacity  CV_2 c.CV_2#c.Capacity, fe cluster(country)
su Capacity if CV_enforced==1, d 
list country Capacity year if CV_enforced==1 & Capacity <-.5 // Bolivia in 1985
list country Capacity year if CV_enforced==1 & Capacity >2.4 & Capacity!=. // Belgium in 2014 
margins, dydx(CV_enforced) at(CV_enforced==1 Capacity =(-.52(.02)2.52))  saving(y, replace)
use y, clear
tw  rarea  _ci_lb _ci_ub  _at17,  title("") ytitle("Effect of CV Sanctioned on Turnout (pp)") xtitle("State Capacity (Hanson & Sigman 2021)", margin(small)) lcolor(gs10) ylabel(-10(5)40)  xlabel(-.5(.5)2.5) legend(off) || line _margin  _at17, lpattern(full) || function y=0+(0*x), range(-.52 2.52) lpattern(dash) name(graph1, replace) title("Graph A: State Capacity")
restore 		
		
*Graph 2
preserve
xtreg Turnout $decades  $new $institutions CV_enforced c.wdi_mortnn c.CV_enforced#c.wdi_mortnn CV_2 c.CV_2#c.wdi_mortnn, fe cluster(country)
su wdi_mortnn if CV_enforced==1, d 
list country wdi_mortnn year if CV_enforced==1 & wdi_mortnn>45 // Bolivia in 1946
list country wdi_mortnn year if CV_enforced==1 & wdi_mortnn<2 // Luxembourg in 2013
margins, dydx(CV_enforced) at(CV_enforced==1 wdi_mortnn=(1.6 2(1)46))  saving(x, replace)
use x, clear
gen mortality = _at
replace mortality = 1.6 if mortality ==1
tw  rarea  _ci_lb _ci_ub mortality ,  title("") ytitle("Effect of CV Sanctioned on Turnout (pp)") xtitle("Neonatal Mortality (per 1,000 live births)", margin(small)) lcolor(gs10) ylabel(-10(5)40)  xlabel(0(5)45) legend(off) || line _margin mortality, lpattern(full) || function y=0+(0*x), range(-1 48) lpattern(dash) name(graph2, replace)  title("Graph B: Neonatal Mortality") 
restore 

*Figure 4
graph combine graph1 graph2, xsize(5) ysize(2) 
cd `"${beg_path}Results"'
graph export "Figure_4.eps", replace  // width(3000) 


		
********************************************************
**** Table 5: Does Turnout decline in CV countries? **** 
********************************************************

eststo clear
eststo M1: xtreg Turnout y_since_1945 $institutions $new  if CV==0, fe cluster(country) //  
estadd local fixed "Yes" , replace
eststo M2: xtreg Turnout y_since_1945 $institutions $new  if CV_2==1, fe cluster(country) // 
estadd local fixed "Yes" , replace
eststo M3: xtreg Turnout y_since_1945 $institutions $new  if CV_enforced==1, fe cluster(country) //  
estadd local fixed "Yes" , replace

cd `"${beg_path}Results"'

esttab M1 M2 M3 using Table_5.tex, one incelldelimite("\:")  stats(fixed N r2, labels("Country FE" "N" "R2") layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{$@$}")  fmt(0 0 2)) replace b(2) se(2) wide staraux ///
		star(+ 0.1 * 0.05 ** 0.01 *** 0.001) eqlabels(none) order(y_since_1945  ///  
		Decisiveness Closeness Concurrent 0.el_system 2.el_system 3.el_system 4.el_system log_el_size pre_1974_new post_1974_new postcommunist_new) substitute("0.00 \: (.)" " " "0.00\:(.)" " " ) ///
		drop( 1.el_system ) lab mtitles( "No CV" "CV Unsanctioned" "CV Sanctioned") ///
		nolegend note("") alignment(d{10}) 

		
*******************************************
**** Table EA3: Replication of Table 5 **** 
*******************************************
		
eststo clear
preserve 
keep if (CV_2==0 & CV_enforced==0) | CV_2==1 | CV_enforced ==1
eststo M1: xtreg Turnout c.y_since_1945 c.y_since_1945#i.CV_2 c.y_since_1945#i.CV_enforced CV_2 CV_enforced $institutions $new, fe cluster(country) //  
estadd local fixed "Yes" , replace
restore 

preserve  // replication for Italy coded as country with sanctioned CV
fre CV_2 CV_enforced if country == 51
replace CV_enforced = 1 if CV_2==1 & country == 51
replace CV_2 = 0 if  country == 51
fre CV_2 CV_enforced if country == 51
eststo M2: xtreg Turnout y_since_1945 $institutions $new  if CV_2==1, fe cluster(country) //  
estadd local fixed "Yes" , replace
eststo M3: xtreg Turnout y_since_1945 $institutions $new  if CV_enforced==1, fe cluster(country) //  
estadd local fixed "Yes" , replace
restore 
eststo M4: xtreg Turnout y_since_1945 $institutions  if CV_2==1 & new==0, fe cluster(country) //  
estadd local fixed "Yes" , replace
eststo M5: xtreg Turnout y_since_1945 $institutions  if CV_enforced==1 & new==0, fe cluster(country) //  
estadd local fixed "Yes" , replace

cd `"${beg_path}Results"'

esttab M1 M2 M3 M4 M5 using Table_A_3.tex, one incelldelimite("\:")  stats(fixed N r2, labels("Country FE" "N" "R2") layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{$@$}")  fmt(0 0 2)) replace b(2) se(2) wide staraux ///
		star(+ 0.1 * 0.05 ** 0.01 *** 0.001) eqlabels(none) order(y_since_1945  1.CV_2#c.y_since_1945 1.CV_enforced#c.y_since_1945  CV_2 CV_enforced ///  
		Decisiveness Closeness Concurrent 0.el_system 2.el_system 3.el_system 4.el_system log_el_size pre_1974_new post_1974_new postcommunist_new) substitute("0.00 \: (.)" " " "0.00\:(.)" " " ) ///
		varlabels(1.CV_2#c.y_since_1945 "Year Since 1945 $\times$ CV" 1.CV_enforced#c.y_since_1945 "Year Since 1945 $\times$ CV Enforced" _cons Constant) /// 
		drop( 1.el_system 0.CV_2#c.y_since_1945 0.CV_enforced#c.y_since_1945) lab  ///
		nolegend note("")  mtitle( "\shortstack{Interactions\\Trend X CV}" "\shortstack{CV Unsanctioned\\Italy alt. coding}" "\shortstack{CV Sanctioned\\Italy alt. coding}" ///
		"\shortstack{CV Unsanctioned\\Est. Dem. Only}" "\shortstack{CV Sanctioned\\Est. Dem. Only}")  alignment(d{10}) 
		
		

********************************************************
**** Figure 5: Does Turnout decline in CV countries? *** 
********************************************************

preserve 

foreach var of varlist decade_1950s-decade_2010s {
egen a_`var' = mean(`var'), by(country)
}
keep if a_decade_1980s > 0 & a_decade_1990s > 0 & a_decade_2000s >0 & a_decade_2010s >0

cap drop CV_ever CV_e_ever 
egen CV_ever = mean(CV), by(country) 
egen CV_e_ever = mean(CV_enforced), by(country) 

drop if CV_ever >0 & CV_ever <1
drop if CV_e_ever >0 & CV_e_ever <1

gen decade = .
replace decade = 1940 if decade_1940s == 1
replace decade = 1950 if decade_1950s == 1
replace decade = 1960 if decade_1960s == 1
replace decade = 1970 if decade_1970s == 1
replace decade = 1980 if decade_1980s == 1
replace decade = 1990 if decade_1990s == 1
replace decade = 2000 if decade_2000s == 1
replace decade = 2010 if decade_2010s == 1

tsset country el_num
ssc install xtab
xtab country if CV_ever==1 & CV_e_ever==0  // 2
list country year Turnout EL_TYPE if CV_ever==1 & CV_e_ever==0 
xtab  country if CV_ever==1 & CV_e_ever==1  // 7
list country year Turnout EL_TYPE if  CV_ever==1 & CV_e_ever==1 // We should drop Peru because there was a democratic breakdown in the 1990s
drop if country == 87
xtab  country if CV_ever==0 // 25
list country year Turnout EL_TYPE if CV_ever==0 // 25-2 We should drop South Korea and Panama (only 1 election in the 1980s, affected by democratization)
drop if country == 55 | country== 85
keep if decade >1970

collapse Turnout CV_ever CV_e_ever, by(decade country)
collapse Turnout , by(decade CV_ever CV_e_ever)

tw connected Turnout decade if CV_ever==0 & CV_e_ever==0, mlabel(Turnout)  mlabp(12) mlabf(%9.1f) ///
 || connected Turnout decade if CV_ever==1 & CV_e_ever==0, mlabel(Turnout)  mlabp(12) mlabf(%9.1f)  ///
 || connected Turnout decade if  CV_ever==1 & CV_e_ever==1, ylabel(60(5)90, labsize(small)) legend(order(1 "No CV (23 countries)" 2 "Unsanctioned CV (2 countries)" 3 "Sanctioned CV (6 countries)") size(small)  row(1)) /// 
 xscale(r(1979 2011)) xlabel(1980"1980s" 1990"1990s" 2000"2000s" 2010"2010s") xtitle("") ytitle(Turnout, margin(medsmall)) scale(*.7) ///
xsize(2.5) ysize(2)  mlabel(Turnout) mlabp(12) mlabf(%9.1f) ytitle("Turnout (%)")

cd `"${beg_path}Results"'
graph export "Figure_5.eps", replace  // 

restore	
 
		
*******************************************
**** Figure EA4: Descriptive Statistics *** 
*******************************************

tab el_system, gen(el_sys)
fre el_sys*

lab var el_sys3 "El. System: Mixed"  
lab var el_sys1 "El. System: Majoritarian"
lab var el_sys2 "El. System: Proportional"
lab var el_sys4 "El. System: Other" 
lab var el_sys5 "Presidential Election"


cd `"${beg_path}Results"'

ssc install sutex2 

sutex2 year Turnout $decades $new Decisiveness Closeness Concurrent el_sys1-el_sys5 log_el_size CV_enforced CV_2 log_gdp_pc /// 
Capacity wdi_mortnn monetaryfine_salary fine_GDP maximummonetaryfine_salary ///
max_fine_GDP non_monetary_sanct, varlab tabular minmax saving(Table_A_4.tex) replace digits(3) 

list country year maximummonetaryfine_USD_2011 gdp_pc max_fine_GDP  if max_fine_GDP >50 & max_fine_GDP!=.


*****************************************
**** Table EA5: Time Trend in Turnout *** 
*****************************************

**** Time Trend ***
eststo clear
eststo M1: xtreg Turnout $decades, fe 
estadd local fixed "Yes", replace

esttab M1 using Table_A_5.tex, one incelldelimite("\:") stats(fixed N r2_o, labels("Country FE" "N" "R2") layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{$@$}")  fmt(0 0 2)) replace b(2) se(2) wide staraux ///
		star(+ 0.1 * 0.05 ** 0.01 *** 0.001) eqlabels(none) order(decade_1940s decade_1950s decade_1960s decade_1970s /// 
		decade_1980s decade_1990s decade_2000s)  varlabels( _cons Constant) /// 
		 lab  substitute("0.00 \: (.)" " " "0.00\:(.)" " " ) /// 5.e_regionpol
		nolegend note("") mtitles( "Turnout by Decade") alignment(d{10}) 
		


***********************************************************
**** Regression Diagnostics (Report in Data \& Methods) *** 
***********************************************************

tsset country el_num
tab country, gen(country_dum)
 
*Hausman  
xtreg Turnout $new Decisiveness Closeness Concurrent el_sys1 el_sys3 el_sys4 el_sys5 log_el_size CV_2 CV_enforced, fe 
estimates store fixed
xtreg Turnout $new Decisiveness Closeness Concurrent el_sys1 el_sys3 el_sys4 el_sys5 log_el_size CV_2 CV_enforced, re 
estimates store random
hausman fixed random, sigmamore //  0.0222
 
* Tests for autocorrelation 
xtserial Turnout $decades $new Decisiveness Closeness Concurrent el_sys1 el_sys3 el_sys4 el_sys5 log_el_size CV_2 CV_enforced country_dum* if el_num !=. // p = 0.0194   

* Tests non-stationarity  
xtunitroot fisher Turnout if el_num !=., pperron lags(1)  // 0.0000
xtunitroot fisher Turnout if el_num !=. , pperron lags(2) // 0.0000
xtunitroot fisher Turnout if el_num !=., pperron lags(3) // 0.0000

***********Tests for contemporaneous correlation (the null hypothesis is no contemporaneous correlation) 
egen max_el_number = max(el_num), by(country)
*For as many observations as possibles
xtreg Turnout $decades $new Decisiveness Closeness Concurrent el_sys1 el_sys3 el_sys4 el_sys5 log_el_size CV_2 CV_enforced if max_el_num > 2, fe 
xtcsd, pesaran //Pr =0.4689
*For countries with a large number of time points 
xtreg Turnout $decades $new Decisiveness Closeness Concurrent el_sys1 el_sys3 el_sys4 el_sys5 log_el_size CV_2 CV_enforced if max_el_num > 26, fe 	
xtcsd, pesaran //Pr =0.6820
 
	
	
	


