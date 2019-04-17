log using "613 PS5", replace

* Alex Nestorov
* ECON 613
* Assignment #5

set more off, perm
set scrollbufsize 2000000
cd "/Users/ainestorov/Documents/ECON 613"
pwd
ssc install estout

******************************************
************* PROBLEM SET 2 **************
******************************************

************* Exercise 1: Data Creation **************
set seed 15
set obs 10000
gen X1 = runiform(1, 3)
gen X2 = rgamma(3, 2)
gen X3 = rbinomial(1, 0.3)
gen eps = rnormal(2, 1)
gen Y = 0.5 + 1.2*X1 + (-0.9)*X2 + 0.1*X3 + eps
egen Y_mean = mean(Y)
gen ydum = 0
replace ydum = 1 if Y>Y_mean
global xvars "X1 X2 X3"

************* Exercise 2: OLS **************
* correlation b/n Y and X1
cor Y X1

* regress Y on X
reg Y $xvars
eststo PS2_OLS1

* calculate standard errors using bootstrap
bootstrap, reps(49): reg Y $xvars
eststo PS2_OLS2

bootstrap, reps(499): reg Y $xvars
eststo PS2_OLS3

* table of results
esttab PS2_OLS1 PS2_OLS2 PS2_OLS3, se(3) nonumbers title(OLS Regressions) ///
mtitles("Normal OLS" "Bootstrap 49" "Bootstrap 499")

************* Exercise 3: Numerical Optimization **************
probit ydum $xvars
eststo PS2_Probit

************* Exercise 4: Discrete Choice **************
* logit regression
logit ydum $xvars
eststo PS2_Logit

* OLS regression
reg ydum $xvars
eststo PS2_LP

* table of results from Exercises 3&4
esttab PS2_Probit PS2_Logit PS2_LP, se(3) title(Discrete Choice Regressions) ///
nonumbers mtitles("Probit" "Logit" "Linear Prob")

************* Exercise 5: Marginal Effects **************
* marginal effects of probit
quietly probit ydum $xvars
margins, dydx(*) atmeans post
eststo PS2_P_MEs 

* marginal effects of logit
quietly logit ydum $xvars
margins, dydx(*) atmeans post
eststo PS2_L_MEs

* calculate standard errors of both probit and logit using bootstrap
bootstrap, reps(99): probit ydum $xvars
margins, dydx(*) atmeans post
eststo PS2_P_MEs_B99

bootstrap, reps(99): logit ydum $xvars
margins, dydx(*) atmeans post
eststo PS2_L_MEs_B99

* table of results
esttab PS2_P_MEs PS2_P_MEs_B99 PS2_L_MEs PS2_L_MEs_B99, se(5) ///
title(Marginal Effects) nonumbers equations(1) ///
mtitles("Probit Delta MEs" "Probit Boot MEs" "Logit Delta MEs" "Logit Boot MEs")

clear

******************************************
************* PROBLEM SET 3 **************
******************************************

************* Exercise 1: Data Description **************
import delimited "/Users/ainestorov/Documents/ECON 613/margarine data.csv"
global product "ppk_stk pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub ppk_tub pfl_tub phse_tub"

* average and dispersion in price
tabstat $product , s(mean var) c(s)

* market share by product choice
tab choice

* market share by brand choice
gen brand = choice
replace brand = 1 if choice == 8
replace brand = 3 if choice == 9
replace brand = 4 if choice == 10
tab brand

* market share by brand type
gen type = 1
replace type = 2 if choice > 6
tab type

* mapping between choice and income
tab income choice, row nofreq

* mapping between choice and family size
tab fam_size choice, row nofreq

************* Exercise 2: First Model **************
* conditional logit
reshape long sel p, i(id)
asclogit sel p, case(id) alternatives(_j)
eststo PS3_clogit
esttab PS3_clogit, noomitted nostar unstack compress se(3) ///
nonumbers title(Conditional Logit Regression)

************* Exercise 3: Second Model **************
* multinomial logit
global demos "income fs3_4 fs5 college whtcollar retired"
mlogit choice $demos
eststo PS3_mlogit
esttab PS3_mlogit, noomitted nostar unstack compress se(4) ///
nonumbers title(Multinomial Logit Regression)

************* Exercise 4: Marginal Effects **************
* conditional logit
quietly asclogit sel p, case(id) alternatives(_j)
estat mfx

* multinomial logit
quietly mlogit choice $demos
mfx

************* Exercise 5: IIA **************
* mixed logit
asmixlogit sel p, case(id) alternatives(_j) casevars($demos)
eststo PS3_mxdlogit1
esttab PS3_mxdlogit1, noomitted nostar unstack compress se(3) ///
nonumbers title(Mixed Logit Regression)

* remove one choice (5)
drop if choice == 5
drop if _j == 5
drop pgen_stk

* mixed logit using removed data
asmixlogit sel p, case(id) alternatives(_j) casevars($demos)
eststo PS3_mxdlogit2
esttab PS3_mxdlogit2, noomitted nostar unstack compress se(3) ///
nonumbers title(Mixed Logit Regression Without Choice = 5)

* test statistic and IIA test
hausman PS3_mxdlogit1 PS3_mxdlogit2, alleqs constant

clear

******************************************
************* PROBLEM SET 4 **************
******************************************

************* Exercise 1: Data **************
import delimited "/Users/ainestorov/Documents/ECON 613/Koop-Tobias.csv"

keep personid logwage timetrnd
reshape wide logwage, i(personid) j(timetrnd)
set seed 15
sample 5, count
list, compress
clear

************* Exercise 2: Random Effects **************
import delimited "/Users/ainestorov/Documents/ECON 613/Koop-Tobias.csv"

xtset personid timetrnd
xtreg logwage educ potexper
eststo PS4_REs
esttab PS4_REs, se(3) nonumbers title(Random Effects Model)
clear

************* Exercise 3: Fixed Effects Model **************
* between estimator
import delimited "/Users/ainestorov/Documents/ECON 613/Koop-Tobias.csv"

xtset personid timetrnd
xtreg logwage educ potexper, be
eststo PS4_between

* within estimator - ignore the constant, this is the average of individual FEs
xtreg logwage educ potexper, fe
eststo PS4_within

* first time difference estimator
reg d.(logwage educ potexper), noconstant
eststo PS4_firstdiff

* results of estimators for comparison
esttab PS4_between PS4_within PS4_firstdiff, se(3) nonumbers ///
title(Fixed Effects Model) mtitles("Between Est" "Within Est" "First Diff Est")

************* Exercise 4: Understanding Fixed Effects **************
set seed 15
keep personid educ potexper logwage timetrnd ability mothered fathered ///
brknhome siblings
reshape wide educ logwage potexper, i(personid) j(timetrnd)
sample 100, count
reshape long

* calculate estimated individual fixed effects
xtreg logwage educ potexper, fe
predict uhat, u
drop if missing(uhat)
duplicates drop uhat, force

* regress estimated individual fixed effects on invariant variables
global invars "ability mothered fathered brknhome siblings"
reg uhat $invars
eststo PS4_ind_FE

* calculate standard errors of above regression using bootstrap
bootstrap, reps(99): reg uhat $invars
eststo PS4_ind_FE_boot

* results of regular and bootstrapped regressions
esttab PS4_ind_FE PS4_ind_FE_boot, se(4) title(Individual Fixed Effect ///
Regressions) nonumbers mtitles("Normal Ind FEs" "Bootstrap 99")

log close
