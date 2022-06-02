**************************
* crime_regressions.do
* run regressions on different types of crimes, paying special attention to
* 	murder
* Lia Yin
* 6-2-2022
**************************
cd "D:\OneDrive - bc.edu\project_SYG"
* log using crime_regressions.smcl, replace

use "data\murder.dta", clear

encode statename, g(state)
order statename state

xtset state year

* compare regressions with ucr data and with shr data
merge 1:1 statename year using "..\Data\SHR\shr.dta", nogen
order mur_shr_r - just_r, after(murderrate)

* correct for missing data problem from SHR
g murder_weight = murderrate/mur_shr_r
drop if murder_weight > 5
g murderrateupw = mur_up_r*murder_weight
g murderratepw = mur_p_r*murder_weight


** generate region-by-year fixed effects (68 total)
forvalues j = 2000/2016{
	gen year`j'=(year==`j')
	gen r`j'1=year`j'*northeast
	gen r`j'2=year`j'*midwest
	gen r`j'3=year`j'*south
	gen r`j'4=year`j'*west
	drop year`j'
}

** generate state-specific linear trends (50 total)
forvalues j=1/50{
	sort state year
	by state: gen trend_`j'=_n
	replace trend_`j'=0 if `j'~=state
}

* generate logs of crime variables
local crimes murderrate mur_shr_r murderrateupw murderratepw

foreach crime in `crimes'{
	g l_`crime' = log(`crime')
}

* generate logs of control variables
gen l_perc_police = log(perc_police)
gen l_income=log(income)
gen l_prisoner=log(prisoner)
gen l_lagprisoner=log(lagprisoner)
gen l_assistance=log(assistance)
gen l_welfare=log(welfare)

* generate state population weight
bys state: egen popwt = mean(population)


* define local macro
	* log crime
local log_crimes l_murderrate l_mur_shr_r l_murderrateupw l_murderratepw
	* demographics
local demo per_wm15 per_bm15 per_wm25 per_bm25 
	* govt spending on welfare
local spending l_assistance l_welfare
	* time-varying controls
local xvar l_perc_police unemployrt poverty l_income l_prisoner l_lagprisoner `demo' `spending' 

	* state linear trend
unab lintrend: trend_1-trend_50
	* region-quarter fixed effects
unab region: r20001-r20164
 
	* generate prelaw variable
local prelaw prelaw
	* generate cumlaw variable to capture post trend
local cum_law cum_law

**# Summary Statistics
	
					**********************************
					*								 *
					*		Summary Statistics		 *
					*								 *
					**********************************
eststo without_law: estpost sum murderrate if law == 0
eststo with_law: estpost sum murderrate if law == 1
eststo diff: estpost ttest murderrate if law == 0 | law == 1, by(law)

#delimit ;
esttab without_law with_law diff, 
	cells("max(pattern(1 1 0) fmt(2)) min(pattern(1 1 0) fmt(2))
	mean(pattern(1 1 0) fmt(2)) sd(pattern(1 1 0) fmt(2))
	b(star pattern(0 0 1) fmt(2)) t(pattern(0 0 1) par fmt(2))") replace ;
#delimit cr

* summary statistics for the control variables
sum perc_police
sum perc_police [aw=population]

sum unemployrt
sum unemployrt [aw=population]

sum poverty
sum poverty [aw=population]

sum income
sum income [aw=population]

sum prisoner
sum prisoner [aw=population]

sum exp_subsidy
sum exp_subsidy [aw=population]

sum exp_pubwelfare
sum exp_pubwelfare [aw=population]

sum blackm_15_24
sum blackm_15_24 [aw=population]

sum whitem_15_24
sum whitem_15_24 [aw=population]

sum blackm_25_44
sum blackm_25_44 [aw=population]

sum whitem_25_44
sum whitem_25_44 [aw=population]

sum perc_gun
sum perc_gun [aw=population]

**# Regressions

					***********************************
					*                                 *
					*            Regressions          *
					*                                 *
					***********************************


					***********************************
					*		with Law-Year Dummies		*
					*		(main specification)		*
					***********************************
foreach crime in `log_crimes' {
	qui xtreg `crime' law i.year [aweight=popwt],fe vce(cluster state)
	eststo `crime'1
	qui xtreg `crime' law i.year `region' [aweight=popwt], fe vce(cluster state)
	eststo `crime'2
	qui xtreg `crime' law i.year `region' `xvar' [aweight=popwt],fe vce(cluster state)
	eststo `crime'3

	* last three columns - option 2: adding in law_year dummy
	qui xtreg `crime' law i.year `region' `xvar' law_year [aweight=popwt], ///
		fe vce(cluster state)
	eststo `crime'4
	qui xtreg `crime' law i.year `region' `xvar' law_year `lintrend' ///
		[aweight=popwt], fe vce(cluster state)
	eststo `crime'5
	qui xtreg `crime' law i.year `region' `xvar' law_year `lintrend' `cum_law' ///
		[aweight=popwt], fe vce(cluster state) 
		// control for long-term effects of the law
	eststo `crime'6

	esttab `crime'1 `crime'2 `crime'3 `crime'4 `crime'5 `crime'6, ///
		keep(law law_year) starlevels(* 0.1 ** 0.05 *** 0.01) ///
		cells(b(star fmt(%9.3f)) se(par)) scalars(F r2) replace
}
					
					***********************************
					*		with Pre-law Dummies	  *
					***********************************
foreach crime in `log_crimes' {
	qui xtreg `crime' law i.year [aweight=popwt],fe vce(cluster state)
	eststo `crime'1
	qui xtreg `crime' law i.year `region' [aweight=popwt], fe vce(cluster state)
	eststo `crime'2
	qui xtreg `crime' law i.year `region' `xvar' [aweight=popwt],fe vce(cluster state)
	eststo `crime'3
	* last three columns - option 1: adding in prelaw dummy
	qui xtreg `crime' law i.year `region' `xvar' prelaw [aweight=popwt], fe vce(cluster state)
	eststo `crime'4
	qui xtreg `crime' law i.year `region' `xvar' prelaw `lintrend' [aweight=popwt], fe vce(cluster state)
	eststo `crime'5
	* control for long-term effects of the law
	qui xtreg `crime' law i.year `region' `xvar' prelaw `lintrend' `cum_law' [aweight=popwt], fe vce(cluster state) 
	eststo `crime'6
	* output regression table
	esttab `crime'1 `crime'2 `crime'3 `crime'4 `crime'5 `crime'6, keep(law prelaw) starlevels(* 0.1 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par)) scalars(F r2) replace
}



					*******************************************
					*		with Around Law-Year Dummies	  *
					*******************************************
foreach crime in `log_crimes' {
	qui xtreg `crime' law i.year [aweight=popwt],fe vce(cluster state)
	eststo `crime'1
	qui xtreg `crime' law i.year `region' [aweight=popwt], fe vce(cluster state)
	eststo `crime'2
	qui xtreg `crime' law i.year `region' `xvar' [aweight=popwt],fe vce(cluster state)
	eststo `crime'3

	* last three columns - option 3: adding in around law year dummy
	qui xtreg `crime' law i.year `region' `xvar' law_year law_b_year law_a_year ///
		[aweight=popwt], fe vce(cluster state)
	eststo `crime'4
	qui xtreg `crime' law i.year `region' `xvar' law_year law_b_year law_a_year ///
		`lintrend' [aweight=popwt], fe vce(cluster state)
	eststo `crime'5
	qui xtreg `crime' law i.year `region' `xvar' law_year law_b_year law_a_year ///
		`lintrend' `cum_law' [aweight=popwt], fe vce(cluster state) 
		// control for long-term effects of the law
	eststo `crime'6

	* using table_`crime'_around_law_year.tex
	esttab `crime'1 `crime'2 `crime'3 `crime'4 `crime'5 `crime'6, ///
	keep (law) starlevels(* 0.1 ** 0.05 *** 0.01) ///
	cells(b(star fmt(%9.3f)) se(par)) scalars(F r2) replace
}


**## Compare coefficients

						************************
						* compare coefficients *
						************************
set matsize 5000
						
rename l_murderratepw lmur1
rename l_murderrateupw lmur2

order l_perc_police unemployrt poverty l_income l_prisoner l_lagprisoner ///
	per_wm15 - per_bm25 l_assistance l_welfare, after(population)

reshape long lmur, i(state year) j(subscript)			


* law year
qui xtreg lmur subscript##(c.law i.year) [aweight=popwt], fe vce(cluster state) 
eststo lmur1
qui xtreg lmur subscript##(c.law i.year r20001 - r20164) [aweight=popwt], fe vce(cluster state) 
eststo lmur2
qui xtreg lmur subscript##(c.law i.year r20001 - r20164 ///
	c.(l_perc_police - l_welfare)) [aweight=popwt], fe vce(cluster state) 
eststo lmur3
qui xtreg lmur subscript##(c.law i.year r20001 - r20164 ///
	c.(l_perc_police - l_welfare) law_year) [aweight=popwt], fe vce(cluster state)
eststo lmur4
qui xtreg lmur subscript##(c.law year r20001 - r20164 ///
	c.(l_perc_police - l_welfare) law_year c.(trend_1-trend_50)) ///
	[aweight=popwt], fe vce(cluster state)
eststo lmur5
qui xtreg lmur subscript##(c.law i.year r20001 - r20164 ///
	c.(l_perc_police - l_welfare) law_year c.(trend_1-trend_50) c.cum_law) ///
	[aweight=popwt], fe vce(cluster state) 
eststo lmur6
esttab lmur1 lmur2 lmur3 lmur4 lmur5 lmur6, keep(2.subscript#c.law) ///
	starlevels(* 0.1 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par)) scalars(F r2) replace

	
* pre-law
qui xtreg lmur subscript##(c.law i.year) [aweight=popwt], fe vce(cluster state) 
eststo lmur1
qui xtreg lmur subscript##(c.law i.year r20001 - r20164) [aweight=popwt], fe vce(cluster state) 
eststo lmur2
qui xtreg lmur subscript##(c.law i.year r20001 - r20164 ///
	c.(l_perc_police - l_welfare)) [aweight=popwt], fe vce(cluster state) 
eststo lmur3
qui xtreg lmur subscript##(c.law i.year r20001 - r20164 ///
	c.(l_perc_police - l_welfare) prelaw) [aweight=popwt], fe vce(cluster state)
eststo lmur4
qui xtreg lmur subscript##(c.law year r20001 - r20164 ///
	c.(l_perc_police - l_welfare) prelaw c.(trend_1-trend_50)) ///
	[aweight=popwt], fe vce(cluster state)
eststo lmur5
qui xtreg lmur subscript##(c.law i.year r20001 - r20164 c.(l_perc_police - l_welfare) ///
	prelaw c.(trend_1-trend_50) c.cum_law) [aweight=popwt], fe vce(cluster state) 
eststo lmur6
esttab lmur1 lmur2 lmur3 lmur4 lmur5 lmur6, keep(2.subscript#c.law) ///
	starlevels(* 0.1 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par)) scalars(F r2) replace

	
* around law-year
qui xtreg lmur subscript##(c.law i.year) [aweight=popwt], fe vce(cluster state) 
eststo lmur1
qui xtreg lmur subscript##(c.law i.year r20001 - r20164) [aweight=popwt], fe vce(cluster state) 
eststo lmur2
qui xtreg lmur subscript##(c.law i.year r20001 - r20164 ///
	c.(l_perc_police - l_welfare)) [aweight=popwt], fe vce(cluster state) 
eststo lmur3
qui xtreg lmur subscript##(c.law i.year r20001 - r20164 ///
	c.(l_perc_police - l_welfare) law_year law_a_year law_b_year) [aweight=popwt], ///
	fe vce(cluster state)
eststo lmur4
qui xtreg lmur subscript##(c.law year r20001 - r20164 ///
	c.(l_perc_police - l_welfare) law_year law_a_year law_b_year c.(trend_1-trend_50)) ///
	[aweight=popwt], fe vce(cluster state)
eststo lmur5
qui xtreg lmur subscript##(c.law i.year r20001 - r20164 ///
	c.(l_perc_police - l_welfare) law_year law_a_year law_b_year c.(trend_1-trend_50) c.cum_law) ///
	[aweight=popwt], fe vce(cluster state) 
eststo lmur6
esttab lmur1 lmur2 lmur3 lmur4 lmur5 lmur6, keep(2.subscript#c.law) ///
	starlevels(* 0.1 ** 0.05 *** 0.01) cells(b(star fmt(%9.3f)) se(par)) scalars(F r2) replace
	
capture log close
clear all
