/*==============================================================================

	 PROGRAM:		Cash for Carbon Analysis

	 INITIAL DATE:	March 30, 2017

===============================================================================

 This do-file creates the following tables and figures:
 ** Tables
	Table 1: Summary statistics for treatment and control groups
	Table 2: Program enrollment, compliance, and payments
	Table 3: Effect of the PES program on tree cover
	Table 4: Effects of the PES program on secondary outcomes

 ** Supplementary material tables
	Table S1: Sample attrition
	Table S2: Correlates of sample attrition
	Table S3: Correlates of program enrollment in treatment group
	Table S4: Effect of the PES program on tree-planting
	Table S5: Effect of the PES program on tree cover with unweighted regressions
	Table S6: Effect of the PES program on tree cover, removing outliers
	Table S7: Effect of the PES program on tree cover using different-sized land circles
	Table S8: Effect of the PES program on tree cover in subsample with baseline satellite data prior to randomization
	Table S9: Heterogeneous effects of the PES program on tree cover
	Table S10: Testing for spillover effects and anticipation effects

 ** Supplementary material figures
	Figure S3: Proportion of village and PFO polygons with available tree-classification data
	Figure S4: Reasons for not enrolling in the program
	Figure S5: Distribution of control group's baseline to endline change in tree cover
*/



** Specify directory
cap cd "C:\Users\Seema\Dropbox\PES_data\PES_Analysis\Endline_Reg\Files for public release"
cap cd "C:\Users\ajn433\Dropbox\PES_data\PES_Analysis\Endline_Reg\Files for public release"
cap cd "C:\Users\sbb261\Dropbox\PES_data\PES_Analysis\Endline_Reg\Files for public release"

cap log close
clear all

set matsize 2400

set more off
pause on


	********************************************************
	********************	Tables	************************
	********************************************************


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table 1: Summary statistics for treatment and control groups
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

	cap file close sumstat
	file open sumstat using "Test tables and figures/tab_balance.tex", write replace

	file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\hspace{2mm}}@{\extracolsep{3pt}}{l}*{3}{>{\centering\arraybackslash}m{1.8cm}}@{}}  \toprule" _n
	file write sumstat "\addlinespace[4mm]" _n
	file write sumstat "& Treatment & Control & Std. diff. \\" _n
	file write sumstat "& (1) & (2) & (3)  \\" _n
	file write sumstat "\midrule" _n

	foreach var of varlist hhead_age hhead_educ ihs_land_area forest_area any_tree_cut cut_cult cut_timber ///
		cut_emerg ihs_tim_price_1yr rent disputewith3 envprog_bi envprob_9 opi_hurtenv_agr ///
		base_fcover_vil perc_forest_vil fcover_change9111_vil base_fcover_act perc_forest fcover_change9111  {

		if "`var'"=="base_fcover_act1" local weight "[aw = SPct_Data_act]"
		else local weight ""

		if "`var'"=="base_fcover_act" restore
		if "`var'"=="base_fcover_vil" {
			preserve
			bysort village: gen N=_n
			keep if N==1 // one observation per village
			}

		local l: variable label `var'

		sum `var' if treat==1 `weight'
			local diff1=r(mean)
			local m1: di %7.3f r(mean)
			local sd1: di %7.3f r(sd)
			local sd1=strtrim("`sd1'")

		sum `var' if treat==0 `weight'
			local diff2=r(mean)
			local m2: di %7.3f r(mean)
			local sd2: di %7.3f r(sd)
			local sd2=strtrim("`sd2'")

		areg `var' treat `weight', absorb(subcounty) cluster(village)
			local diff=_b[treat]
			local t=_b[treat]/_se[treat]
			local p=2*ttail(e(df_r), abs(`t'))
				if `p'<=.10 local stars "*"
				if `p'<=.05 local stars "**"
				if `p'<=.01 local stars "***"
				else if `p'>.10 local stars ""

		sum `var' `weight'
			local sdiff: di %7.3f (`diff')/r(sd)
			local p1 "`sdiff'`stars'"

		file write sumstat " `l' & `m1' & `m2' & `p1' \\  " _n
		file write sumstat " & [`sd1'] & [`sd2'] &  \\  \addlinespace   " _n
	}


	count if treat==1
		local n1: di %7.0fc r(N)
	count if treat==0
		local n2: di %7.0fc r(N)

	bysort village: gen N=_n
	count if treat==1 & N==1
		local v1: di %7.0fc r(N)
	count if treat==0 & N==1
		local v2: di %7.0fc r(N)

	file write sumstat "\midrule " _n

	file write sumstat "Observations (forest owners) & `n1' & `n2' &   \\" _n
	file write sumstat " Number of villages & `v1' & `v2' &   \\" _n

	file write sumstat "\bottomrule" _n
	file write sumstat "\end{tabular}" _n
	file close sumstat


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table 2: Program take-up
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


	use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil

	eststo clear
	local n=1
	foreach var in  Ftakeup conserved Famt_paid Fperc_paid {

		eststo: areg `var' treat `strat', absorb(subcounty) cluster(village)
			quietly: sum `var' if treat==0
			estadd scalar varmean=r(mean)

			local varlab`n': variable label `var'
			local n=`n'+1
		}

	local vars treat

	#delimit ;
	esttab using "Test tables and figures/tab_takeup.tex",
		b(%7.3f) se star(* .1 ** .05 *** .01) nonotes nomtitles number brackets replace gaps label booktabs style(tex)
		keep(`vars') order(`vars')
		prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
		\begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{9}{>{\centering\arraybackslash}m{2.6cm}}}
		\toprule \addlinespace[3mm]
		 & `varlab1' & `varlab2' & `varlab3' & `varlab4' \\")
		stats(varmean N, labels("Control group mean" "Observations") fmt(%7.3f %7.0fc));
	#delimit cr

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table 3: Effect of PES program on forest cover
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

		use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil
		local controls_act photo1991_act photo2011_act pfo_satdate_*
		local ihs_controls_act ihs_photo1991_act ihs_photo2011_act pfo_satdate_*

		eststo clear

	// PFO-level
	foreach stub in _act {
		// Changes
		eststo: areg change_fcover`stub' treat `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "No"
				quietly: sum change_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)

		eststo: areg change_fcover`stub' treat photo1991_vil photo2011_vil `controls`stub'' `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum change_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)

		eststo: areg ihs_diff_fcover`stub' treat ihs_photo1991_vil ihs_photo2011_vil `ihs_controls`stub'' `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum ihs_diff_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)
		}

	// Village-level
		bysort village: gen N=_n
		keep if N==1 // one observation per village

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil
		local controls photo1991_vil photo2011_vil village_satdate_*
		local ihs_controls ihs_photo1991_vil ihs_photo2011_vil village_satdate_*

	eststo: areg change_fcover_vil treat `strat' [aw = SPct_Data_vil], absorb(subcounty) cluster(village)
		estadd local control "No"
			quietly: sum change_fcover_vil if treat==0 [aw = SPct_Data_vil]
		estadd scalar varmean=r(mean)

	eststo: areg change_fcover_vil treat total_photo1991_act total_photo2011_act `controls' `strat' [aw = SPct_Data_vil], absorb(subcounty) cluster(village)
		estadd local control "Yes"
			quietly: sum change_fcover_vil if treat==0 [aw = SPct_Data_vil]
		estadd scalar varmean=r(mean)

	eststo: areg ihs_diff_fcover_vil treat ihs_total_photo1991_act ihs_total_photo2011_act `ihs_controls' `strat' [aw = SPct_Data_vil], absorb(subcounty) cluster(village)
		estadd local control "Yes"
			quietly: sum ihs_diff_fcover_vil if treat==0 [aw = SPct_Data_vil]
		estadd scalar varmean=r(mean)

	 #delimit ;
	esttab est4 est5 est6 est1 est2 est3 using "Test tables and figures/tab_sat_main.tex", 
		b(a3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets replace gaps label booktabs style(tex)
		keep(treat)
		prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
		\begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{8}{>{\centering\arraybackslash}m{2cm}}}
		\toprule \addlinespace[3mm]
		& \multicolumn{3}{c}{Village boundaries} &\multicolumn{3}{c}{PFO-level land circles}  \\
		\cmidrule(lr){2-4} \cmidrule(lr){5-7}
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Log of tree cover
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ IHS of tree cover \\ ")
		stats(varmean control N,
		labels("Control group mean" "Control variables" "Observations")
		fmt(%7.3fc %3.0s %7.0fc));
	#delimit cr

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table 4: Effects of PES program on secondary outcomes
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil

		// Dummy out baseline outcome
		foreach var in any_tree_cut any_fence_f ihs_food_cons ihs_nofood_cons {
			gen `var'_flag=mi(`var')
				qui: sum `var'
				replace `var'=`r(mean)' if mi(`var')
				}



	summ treat if treat
		local t_N = r(N)
	summ treat if treat==0
		local c_N = r(N)

	gen miss = 0
	local i=0
	set seed 232016
	gen x=runiform() // to brake Lee bounds ties

	foreach v in any_tree_cut allow_firewood change_patrol_inc any_fence_f ihs_food_cons ihs_nofood_cons {
		if "`v'"=="any_tree_cut" local stub "_1yr"
		else local stub ""

			local ++i
		local col_list "`col_list' & (`i')"
		local l`i': variable label F`v'`stub'
		local label_list "`label_list' & `l`i''"
		quietly: sum F`v'`stub' if treat==0
			local mean`i': di %7.3f r(mean)
			local sd`i'=trim(string(r(sd), "%7.3f"))
		local mean_list "`mean_list' & `mean`i''"
		local sd_list "`sd_list' & [`sd`i'']"

		gen TLO_`v' = 0		// obsns to trim for lower bound
		gen TUP_`v' = 0		// obsns to trim for upper bound
		gen TABS_`v' = 0	// obsns to trim for lower bound in absolute value terms

		cap confirm variable `v'		// Check if we have baseline outcome for this variable; if so, control for it
		if _rc {
			areg F`v' treat `strat', absorb(subcounty) cluster(village)
			}
		else {
			areg F`v'`stub' treat `v' `strat' `v'_flag, absorb(subcounty) cluster(village)
			}

		local posneg = sign(_b[treat])	// Is treatment effect with untrimmed sample positive or negative?
		replace miss = (F`v'`stub'== .)		// This is so when sorting data below and using _n, don't have to worry if Stata puts missing values first or last

		summ F`v'`stub' if treat==1
		local t_n = r(N)
		local t_rate = `t_n'/`t_N'		// attrition rate for treatment group
		summ F`v'`stub' if treat==0
		local c_n = r(N)
		local c_rate = `c_n'/`c_N'		// attrition rate for control group

		local trim_rate = abs(`c_rate' - `t_rate')	// % of obsns to trim
		di _n "Trim rate:  `trim_rate'" _n


		if `c_rate' > `t_rate' {  // trim from control group
			local trim_n = round(`trim_rate'* `c_N')	// number of obsns to trim
			cap confirm variable `v'
			if _rc gsort miss treat F`v'`stub' x						// want to tag observations in either treat or control group that are non-missing and high or low values
			else gsort miss treat F`v'`stub' -`v' x
			by miss treat: replace TLO_`v' = 1 if _n <= `trim_n' & miss==0 & treat==0	// trim low values from control group to get lower bound
			cap confirm variable `v'
			if _rc gsort miss treat -F`v'`stub' x
			else gsort miss treat -F`v'`stub' `v' x
			by miss treat: replace TUP_`v' = 1 if _n <= `trim_n' & miss==0 & treat==0
			}

		if `c_rate' < `t_rate' {  // trim from treat group
			local trim_n = round(`trim_rate'* `t_N')	// number of obsns to trim
			cap confirm variable `v'
			if _rc gsort miss treat F`v'`stub' x						// want to tag observations in either treat or control group that are non-missing and high or low values
			else gsort miss treat F`v'`stub' -`v' x
			by miss treat: replace TUP_`v' = 1 if _n <= `trim_n' & miss==0 & treat==1	// trim low values from treat group to get upper bound
			cap confirm variable `v'
			if _rc gsort miss treat -F`v'`stub' x
			else gsort miss treat -F`v'`stub' `v' x
			by miss treat: replace TLO_`v' = 1 if _n <= `trim_n' & miss==0 & treat==1
			}

		* Lower bound in absolute value terms depends on whether coefficient is positive or negative (unlikely to be exactly 0, but if so, full sample gives lower abs value bound)
		if `posneg'==1 {
				replace TABS_`v' = TLO_`v' 	// if treatment effect is positive, lower bound in absolute value terms is actual lower bound

				}
		if `posneg'==-1 {
				replace TABS_`v' = TUP_`v' // if treatment effect is negative, lower bound in absolute value terms is actual upper bound
				}

		cap confirm variable `v'		// Check if we have baseline outcome
		if _rc {
			di _n "*****   Full sample   *****" _n
			areg F`v' treat `strat', absorb(subcounty) cluster(village)
					local OBS`i': di %7.0fc e(N)
				local obs_list "`obs_list' & `OBS`i''"

				foreach treat in treat {
					local `treat'_B`i': di %7.3f _b[`treat']
					local `treat'_SE`i'=trim(string(_se[`treat'], "%7.3f"))

					* stars
					local t=(_b[`treat'])/(_se[`treat'])
					local p=2*ttail(e(df_r), abs(`t'))
						if `p'<=.10 local stars "*"
						if `p'<=.05 local stars "**"
						if `p'<=.01 local stars "***"
						else if `p'>.10 local stars ""

					local `treat'_B`i' "``treat'_B`i''`stars'"
					local `treat'_coeff_list "``treat'_coeff_list' & ``treat'_B`i''"
					local `treat'_se_list "``treat'_se_list' & [``treat'_SE`i'']"
					local base_coeff_list "`base_coeff_list' & "
					local base_se_list "`base_se_list' & "
				}

			di _n "*****   Lower bound   *****" _n
			areg F`v' treat `strat' if TLO_`v'==0, absorb(subcounty) cluster(village)
					local OBSlee`i': di %7.0fc e(N)
				local obslee_list "`obslee_list' & `OBSlee`i''"
				local treat_B`i': di %7.3f _b[treat]
				local treat_SE`i'=trim(string(_se[treat], "%7.3f"))

				* stars
				local t=(_b[treat])/(_se[treat])
				local p=2*ttail(e(df_r), abs(`t'))
					if `p'<=.10 local stars "*"
					if `p'<=.05 local stars "**"
					if `p'<=.01 local stars "***"
					else if `p'>.10 local stars ""

				local leelow_B`i' "`treat_B`i''`stars'"
				local leelow_coeff_list "`leelow_coeff_list' & `leelow_B`i''"
				local leelow_se_list "`leelow_se_list' & [`treat_SE`i'']"

			di _n "*****   Upper bound" _n
			areg F`v' treat `strat' if TUP_`v'==0, absorb(subcounty) cluster(village)

				local treat_B`i': di %7.3f _b[treat]
				local treat_SE`i'=trim(string(_se[treat], "%7.3f"))

				* stars
				local t=(_b[treat])/(_se[treat])
				local p=2*ttail(e(df_r), abs(`t'))
					if `p'<=.10 local stars "*"
					if `p'<=.05 local stars "**"
					if `p'<=.01 local stars "***"
					else if `p'>.10 local stars ""

				local leeup_B`i' "`treat_B`i''`stars'"
				local leeup_coeff_list "`leeup_coeff_list' & `leeup_B`i''"
				local leeup_se_list "`leeup_se_list' & [`treat_SE`i'']"
			}
		else {
			cap drop base
			gen base=`v'
			di _n "*****   Full sample   *****" _n
			areg F`v'`stub' treat base `strat' `v'_flag, absorb(subcounty) cluster(village)
					local OBS`i': di %7.0fc e(N)
				local obs_list "`obs_list' & `OBS`i''"

				foreach treat in treat base {
					local `treat'_B`i': di %7.3f _b[`treat']
					local `treat'_SE`i'=trim(string(_se[`treat'], "%7.3f"))

					* stars
					local t=(_b[`treat'])/(_se[`treat'])
					local p=2*ttail(e(df_r), abs(`t'))
						if `p'<=.10 local stars "*"
						if `p'<=.05 local stars "**"
						if `p'<=.01 local stars "***"
						else if `p'>.10 local stars ""

					local `treat'_B`i' "``treat'_B`i''`stars'"
					local `treat'_coeff_list "``treat'_coeff_list' & ``treat'_B`i''"
					local `treat'_se_list "``treat'_se_list' & [``treat'_SE`i'']"
				}

			di _n "*****   Lower bound   *****" _n
			areg F`v'`stub' treat  base `strat' `v'_flag if TLO_`v'==0, absorb(subcounty) cluster(village)
					local OBSlee`i': di %7.0fc e(N)
				local obslee_list "`obslee_list' & `OBSlee`i''"
				local treat_B`i': di %7.3f _b[treat]
				local treat_SE`i'=trim(string(_se[treat], "%7.3f"))

				* stars
				local t=(_b[treat])/(_se[treat])
				local p=2*ttail(e(df_r), abs(`t'))
					if `p'<=.10 local stars "*"
					if `p'<=.05 local stars "**"
					if `p'<=.01 local stars "***"
					else if `p'>.10 local stars ""

				local leelow_B`i' "`treat_B`i''`stars'"
				local leelow_coeff_list "`leelow_coeff_list' & `leelow_B`i''"
				local leelow_se_list "`leelow_se_list' & [`treat_SE`i'']"

			di _n "*****   Upper bound   *****" _n
			areg F`v'`stub' treat  base `strat' `v'_flag if TUP_`v'==0, absorb(subcounty) cluster(village)

				local treat_B`i': di %7.3f _b[treat]
				local treat_SE`i'=trim(string(_se[treat], "%7.3f"))

				* stars
				local t=(_b[treat])/(_se[treat])
				local p=2*ttail(e(df_r), abs(`t'))
					if `p'<=.10 local stars "*"
					if `p'<=.05 local stars "**"
					if `p'<=.01 local stars "***"
					else if `p'>.10 local stars ""

				local leeup_B`i' "`treat_B`i''`stars'"
				local leeup_coeff_list "`leeup_coeff_list' & `leeup_B`i''"
				local leeup_se_list "`leeup_se_list' & [`treat_SE`i'']"
			}
		}

	cap file close sumstat
	file open sumstat using "Test tables and figures/tab_treat_treecut_landuse_socioeco.tex", write replace

	file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{9}{>{\centering\arraybackslash}m{2.35cm}}@{}}  \toprule" _n
	file write sumstat "\addlinespace[3mm]" _n
	file write sumstat "`label_list'  \\" _n
	file write sumstat "`col_list'  \\" _n
	file write sumstat "\midrule" _n


	file write sumstat "Treated `treat_coeff_list'   \\     " _n
	file write sumstat "		`treat_se_list' 	\\  \addlinespace   " _n

	file write sumstat "Baseline outcome `base_coeff_list'   \\     " _n
	file write sumstat "					`base_se_list' 	\\  \addlinespace   " _n
	file write sumstat "\addlinespace[5mm]" _n


	file write sumstat "Lee bound (lower) `leelow_coeff_list'   \\     " _n
	file write sumstat "					`leelow_se_list' 	\\  \addlinespace   " _n

	file write sumstat "Lee bound (upper) `leeup_coeff_list'   \\     " _n
	file write sumstat "					`leeup_se_list' 	\\  \addlinespace   " _n

	file write sumstat "\midrule " _n
	file write sumstat "\addlinespace" _n
	file write sumstat "Control group mean `mean_list'  \\" _n
	file write sumstat "Control group SD `sd_list'  \\" _n

	file write sumstat "Observations `obs_list'  \\" _n
	file write sumstat "Observations (Lee bounds)  `obslee_list'  \\" _n

	file write sumstat "\bottomrule" _n
	file write sumstat "\end{tabular}" _n
	file close sumstat
	macro drop _all


	********************************************************************************
	********************	Supplementary material Tables	************************
	********************************************************************************

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table S1: Sample attrition
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	use "Data/PES_analysis_science.dta", clear
	keep if sample_gps==1

	cap file close sumstat
	file open sumstat using "Test tables and figures/tab_PFO_numbers.tex", write replace

	file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\hspace{2mm}}@{\extracolsep{3pt}}{l}*{3}{>{\centering\arraybackslash}m{1.6cm}}@{}}  \toprule" _n
	file write sumstat "& \multicolumn{3}{c}{Number of PFOs} \\" _n
	file write sumstat "\cmidrule(lr){2-4} \\" _n
	file write sumstat "& Treatment group & Control group & Total  \\" _n
	file write sumstat "\midrule" _n

		count if treat==1
		local n11:di %7.0fc r(N)
		count if treat==0
		local n12:di %7.0fc r(N)
		count
		local n13: di %7.0fc r(N)
	file write sumstat "Baseline survey (with GPS location of PFO home) & `n11' & `n12' & `n13' \\" _n
	file write sumstat "\addlinespace[4mm]" _n

		count if base_fcover_act!=. & treat==1
		local n21: di %7.0fc r(N)
		count if base_fcover_act!=. & treat==0
		local n22: di %7.0fc r(N)
		count if base_fcover_act!=.
		local n23: di %7.0fc r(N)
	file write sumstat "Baseline survey and satellite data for PFO land circle & `n21' & `n22' & `n23' \\"_n
	file write sumstat "\addlinespace[2mm]" _n

			count if base_fcover_act==. & hh_has_land==0 & treat==1
			local n31: di %7.0fc r(N)
			count if base_fcover_act==. & hh_has_land==0 & treat==0
			local n32: di %7.0fc r(N)
			count if base_fcover_act==. & hh_has_land==0 & merge_act!=1
			local n33: di %7.0fc r(N)
		file write sumstat "\hspace{1cm} HH reports owning no land & `n31' & `n32' & `n33' \\"_n
		file write sumstat "\addlinespace[2mm]" _n

			count if hh_has_land==1 & land_area==. & treat==1
			local n91: di %7.0fc r(N)
			count if hh_has_land==1 & land_area==. & treat==0
			local n92: di %7.0fc r(N)
			count if hh_has_land==1 & land_area==.
			local n93: di %7.0fc r(N)
		file write sumstat "\hspace{1cm} Didn't report land area & `n91' & `n92' & `n93' \\"_n
		file write sumstat "\addlinespace[2mm]" _n

			count if merge_act==1 & base_fcover_act==. & treat==1
			local n41: di %7.0fc r(N)
			count if merge_act==1 & base_fcover_act==. & treat==0
			local n42: di %7.0fc r(N)
			count if merge_act==1 & base_fcover_act==.
			local n43: di %7.0fc r(N)
	file write sumstat "\hspace{1cm} Missing tree-classification data for entire PFO land circle & `n41' & `n42' & `n43' \\"_n
	file write sumstat "\addlinespace[4mm]" _n

		count if did_endline==1 & treat==1
		local n51: di %7.0fc r(N)
		count if did_endline==1 & treat==0
		local n52: di %7.0fc r(N)
		count if did_endline==1
		local n53: di %7.0fc r(N)
	file write sumstat "Baseline survey and endline survey & `n51' & `n52' & `n53' \\"_n
	file write sumstat "\addlinespace[4mm]" _n

		count if did_endline==1 & base_fcover_act!=. & treat==1
		local n61: di %7.0fc r(N)
		count if did_endline==1 & base_fcover_act!=. & treat==0
		local n62: di %7.0fc r(N)
		count if did_endline==1 & base_fcover_act!=.
		local n63: di %7.0fc r(N)
	file write sumstat "Baseline survey, satellite data for PFO land circle, and endline survey & `n61' & `n62' & `n63' \\"_n
	file write sumstat "\addlinespace[4mm]" _n

	file write sumstat "\bottomrule" _n
	file write sumstat "\end{tabular}" _n
	file close sumstat


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Appendix Table 2: Determinants of sample attrition
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil

	// Make comparison dummies
		gen miss_endline=0
			replace miss_endline=1 if did_endline==0
		gen inv_miss_endline=(miss_endline==0)

		gen miss_satellite=0
			replace miss_satellite=1 if base_fcover_act==.
		gen inv_miss_satellite=(miss_satellite==0)

	cap file close sumstat
	file open sumstat using "Test tables and figures/tab_attrition.tex", write replace

	file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\extracolsep{-12pt}}{l}*{6}{>{\centering\arraybackslash}m{1.9cm}}@{}}  \toprule" _n
	file write sumstat "\addlinespace[2mm]" _n
	file write sumstat "& All PFOs & PFOs with missing tree classification data & PFOs with missing endline data & Std. diff. (1-2) & Adj. std. diff. (1-2) & Std. diff. (1-3) \\" _n
	file write sumstat "& (1) & (2) & (3) & (4) & (5) & (6)  \\" _n
	file write sumstat "\midrule" _n

	foreach var of varlist hhead_age hhead_educ ihs_land_area forest_area any_tree_cut cut_cult cut_timber ///
		cut_emerg ihs_tim_price_1yr rent disputewith3 envprog_bi envprob_9 opi_hurtenv_agr ///
		  treat Ftakeup base_fcover_act perc_forest fcover_change9111{

		if "`var'"=="Ftakeup" local cond "if treat==1"
			else local cond ""
		if "`var'"=="Ftakeup" local cond1 "& treat==1"
			else local cond1 ""
		if "`var'"=="base_fcover_act1" local weight "[aw = SPct_Data_med]"
			else local weight ""

		local l: variable label `var'

		sum `var' `cond' `weight'
			local diff1=r(mean)
			local m1: di %7.3f r(mean)
			local sd1: di %7.3f r(sd)
			local sd1=strtrim("`sd1'")

		sum `var' if inv_miss_endline==0 `cond1' `weight'
			local diff2=r(mean)
			local m2: di %7.3f r(mean)
			local sd2: di %7.3f r(sd)
			local sd2=strtrim("`sd2'")

		sum `var' if inv_miss_satellite==0 `cond1'
			local diff3=r(mean)
			local m3: di %7.3f r(mean)
			local sd3: di %7.3f r(sd)
			local sd3=strtrim("`sd3'")

		areg `var' inv_miss_endline `cond' `weight', absorb(subcounty) cluster(village)
			local diff=_b[inv_miss_endline]
			local t=_b[inv_miss_endline]/_se[inv_miss_endline]
			local p=2*ttail(e(df_r), abs(`t'))
				if `p'<=.10 local stars "*"
				if `p'<=.05 local stars "**"
				if `p'<=.01 local stars "***"
				else if `p'>.10 local stars ""

		sum `var' if !mi(inv_miss_endline) `cond1'
			local sdiff: di %7.3f (`diff')/r(sd)
			local p1 "`sdiff'`stars'"

		areg `var' inv_miss_satellite `cond', absorb(subcounty) cluster(village)
			local diff=_b[inv_miss_satellite]
			local t=_b[inv_miss_satellite]/_se[inv_miss_satellite]
			local p=2*ttail(e(df_r), abs(`t'))
				if `p'<=.10 local stars "*"
				if `p'<=.05 local stars "**"
				if `p'<=.01 local stars "***"
				else if `p'>.10 local stars ""

		sum `var' if !mi(inv_miss_satellite) `cond1'
			local sdiff: di %7.3f (`diff')/r(sd)
			local p2 "`sdiff'`stars'"

		areg `var' inv_miss_satellite ihs_land_area `cond', absorb(subcounty) cluster(village)
			local diff=_b[inv_miss_satellite]
			local t=_b[inv_miss_satellite]/_se[inv_miss_satellite]
			local p=2*ttail(e(df_r), abs(`t'))
				if `p'<=.10 local stars "*"
				if `p'<=.05 local stars "**"
				if `p'<=.01 local stars "***"
				else if `p'>.10 local stars ""

		sum `var' if !mi(inv_miss_satellite) `cond1'
			local sdiff: di %7.3f (`diff')/r(sd)
			local p3 "`sdiff'`stars'"


		if "`var'"=="base_fcover_act" | "`var'"=="base_fcover_act1" | "`var'"=="perc_forest" | "`var'"=="fcover_change9111" {
			file write sumstat " `l' & `m1' &  & `m2' &  & & `p1' \\  " _n
			file write sumstat " & [`sd1'] &  & [`sd2'] & & & \\  \addlinespace[1.5mm]   " _n
			}
		else if "`var'"=="ihs_land_area" {
			file write sumstat " `l' & `m1' & `m3' & `m2' & `p2' &  & `p1' \\  " _n
			file write sumstat " & [`sd1'] & [`sd3'] & [`sd2'] & & & \\  \addlinespace[1.5mm]   " _n
			}
		else {
			file write sumstat " `l' & `m1' & `m3' & `m2' & `p2' & `p3' & `p1' \\  " _n
			file write sumstat " & [`sd1'] & [`sd3'] & [`sd2'] & & & \\  \addlinespace[1.5mm]   " _n
			}
	}

	file write sumstat "\midrule " _n

	count
		local n1: di %7.0fc r(N)
	count if inv_miss_endline==0
		local n2: di %7.0fc r(N)
	count if inv_miss_satellite==0
		local n3: di %7.0fc r(N)

	file write sumstat "Observations & `n1' & `n3' & `n2' & & & \\" _n

	file write sumstat "\bottomrule" _n
	file write sumstat "\end{tabular}" _n
	file close sumstat


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table S3: Correlates of program enrollment in treatment group
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil

	local survey hhead_age hhead_educ ihs_land_area forest_area any_tree_cut cut_cult cut_timber ///
					cut_emerg ihs_tim_price rent disputewith3 envprog_bi envprob_9 opi_hurtenv_agr
	local satellite base_fcover_act fcover_change9111
	local takeupdet hhead_age hhead_educ ihs_land_area forest_area any_tree_cut cut_cult cut_timber ///
					cut_emerg ihs_tim_price rent disputewith3 envprog_bi envprob_9 opi_hurtenv_agr  ///
					fcover_change9111
	local controls2 ihs_land_area cut_emerg opi_hurtenv_agr  base_fcover_act

	foreach var in `controls2'{
		local flag_list2 `flag_list2' `var'_flag
		}

	preserve
		// Dummy out
		foreach var in `survey' `satellite'{
			gen `var'_flag=mi(`var')
				qui: sum `var'
				replace `var'=`r(mean)' if mi(`var')
				local flag_list1 `flag_list1' `var'_flag
				}

	eststo clear
	eststo: areg Ftakeup `survey' `satellite' `flag_list1' `strat' if treat==1, absorb(subcounty) cluster(village)
		estadd local note2 "Yes"
		estadd local sample "Treatment group"

	eststo: areg Ftakeup `controls2' `flag_list2' `strat' if treat==1, absorb(subcounty) cluster(village)
		estadd local note2 "Yes"
		estadd local sample "Treatment group"

	restore

		// Dummy out
		foreach var in `takeupdet' {
			gen `var'_flag=mi(`var')
				qui: sum `var'
				replace `var'=`r(mean)' if mi(`var')
				local flag_list3 `flag_list3' `var'_flag
				}

		eststo: areg change_fcover_act `strat' `takeupdet' `flag_list3' [aw = SPct_Data_act] if treat==0 & partid!=2182, absorb(subcounty) cluster(village)
			estadd local sample "Control group"

		predict predicted_fcover
			label var predicted_fcover "Predicted change in tree cover"

		eststo: areg Ftakeup predicted_fcover if treat==1, absorb(subcounty) cluster(village)
			estadd local sample "Treatment group"

	#delimit ;
		esttab using "Test tables and figures/tab_takeup_det.tex",
			b(%7.3f) se star(* .1 ** .05 *** .01) nonotes nomtitles number brackets replace gaps label booktabs style(tex)
			keep(`survey' `satellite' predicted_fcover) order(`survey' `satellite')
			prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
			\begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{4}{>{\centering\arraybackslash}m{2cm}}}
			\toprule \addlinespace[3mm]
			 & Enrolled & Enrolled & $\Delta\!\!\!$ Tree cover (ha) & Enrolled   \\")
			stats(sample r2 N, labels("Sample" "R\textsuperscript{2}" "Observations") fmt(%3.0s %7.3f %7.0fc));
		#delimit cr


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table S4: Effect of the PES program on tree-planting
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


	use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil

	summ treat if treat
		local t_N = r(N)
	summ treat if treat==0
		local c_N = r(N)

	gen miss = 0
	local i=0
	set seed 232016
	gen x=runiform() // to brake Lee bounds ties

	foreach v in reforestation_option cswct1_Reforestation cswct2_TotalplantPlt cswct2_TotalSurv Fplant_any{
			local ++i
		local col_list "`col_list' & (`i')"
		local l`i': variable label `v'
		local label_list "`label_list' & `l`i''"
		quietly: sum `v' if treat==0
			local mean`i': di %7.3f r(mean)
			local sd`i'=trim(string(r(sd), "%7.3f"))
		local mean_list "`mean_list' & `mean`i''"
		local sd_list "`sd_list' & [`sd`i'']"

		gen TLO_`v' = 0		// obsns to trim for lower bound
		gen TUP_`v' = 0		// obsns to trim for upper bound
		gen TABS_`v' = 0	// obsns to trim for lower bound in absolute value terms

		areg `v' treat `strat', absorb(subcounty) cluster(village)


		local posneg = sign(_b[treat])	// Is treatment effect with untrimmed sample positive or negative?
		replace miss = (`v'== .)		// This is so when sorting data below and using _n, don't have to worry if Stata puts missing values first or last

		summ `v' if treat==1
		local t_n = r(N)
		local t_rate = `t_n'/`t_N'		// attrition rate for treatment group
		summ `v' if treat==0
		local c_n = r(N)
		local c_rate = `c_n'/`c_N'		// attrition rate for control group

		local trim_rate = abs(`c_rate' - `t_rate')	// % of obsns to trim
		di _n "Trim rate:  `trim_rate'" _n


		if `c_rate' > `t_rate' {  // trim from control group
			local trim_n = round(`trim_rate'* `c_N')	// number of obsns to trim
			gsort miss treat `v' x						// want to tag observations in either treat or control group that are non-missing and high or low values
			by miss treat: replace TLO_`v' = 1 if _n <= `trim_n' & miss==0 & treat==0	// trim low values from control group to get lower bound
			gsort miss treat -`v' x
			by miss treat: replace TUP_`v' = 1 if _n <= `trim_n' & miss==0 & treat==0
			}

		if `c_rate' < `t_rate' {  // trim from treat group
			local trim_n = round(`trim_rate'* `t_N')	// number of obsns to trim
			gsort miss treat `v' x						// want to tag observations in either treat or control group that are non-missing and high or low values
			by miss treat: replace TUP_`v' = 1 if _n <= `trim_n' & miss==0 & treat==1	// trim low values from treat group to get upper bound
			gsort miss treat -`v' x
			by miss treat: replace TLO_`v' = 1 if _n <= `trim_n' & miss==0 & treat==1
			}

		* Lower bound in absolute value terms depends on whether coefficient is positive or negative (unlikely to be exactly 0, but if so, full sample gives lower abs value bound)
		if `posneg'==1 {
				replace TABS_`v' = TLO_`v' 	// if treatment effect is positive, lower bound in absolute value terms is actual lower bound

				}
		if `posneg'==-1 {
				replace TABS_`v' = TUP_`v' // if treatment effect is negative, lower bound in absolute value terms is actual upper bound
				}

			di _n "*****   Full sample   *****" _n
			areg `v' treat `strat', absorb(subcounty) cluster(village)
					local OBS`i': di %7.0fc e(N)
				local obs_list "`obs_list' & `OBS`i''"

				foreach treat in treat {
					local `treat'_B`i': di %7.3f _b[`treat']
					local `treat'_SE`i'=trim(string(_se[`treat'], "%7.3f"))

					* stars
					local t=(_b[`treat'])/(_se[`treat'])
					local p=2*ttail(e(df_r), abs(`t'))
						if `p'<=.10 local stars "*"
						if `p'<=.05 local stars "**"
						if `p'<=.01 local stars "***"
						else if `p'>.10 local stars ""

					local `treat'_B`i' "``treat'_B`i''`stars'"
					local `treat'_coeff_list "``treat'_coeff_list' & ``treat'_B`i''"
					local `treat'_se_list "``treat'_se_list' & [``treat'_SE`i'']"
					local base_coeff_list "`base_coeff_list' & "
					local base_se_list "`base_se_list' & "
				}
		if "`v'"=="Fplant_any" {
			di _n "*****   Lower bound   *****" _n
			areg `v' treat `strat' if TLO_`v'==0, absorb(subcounty) cluster(village)
					local OBSlee`i': di %7.0fc e(N)
				local obslee_list "`obslee_list' & `OBSlee`i''"
				local treat_B`i': di %7.3f _b[treat]
				local treat_SE`i'=trim(string(_se[treat], "%7.3f"))

				* stars
				local t=(_b[treat])/(_se[treat])
				local p=2*ttail(e(df_r), abs(`t'))
					if `p'<=.10 local stars "*"
					if `p'<=.05 local stars "**"
					if `p'<=.01 local stars "***"
					else if `p'>.10 local stars ""

				local leelow_B`i' "`treat_B`i''`stars'"
				local leelow_coeff_list "`leelow_coeff_list' & `leelow_B`i''"
				local leelow_se_list "`leelow_se_list' & [`treat_SE`i'']"

			di _n "*****   Upper bound" _n
			areg `v' treat `strat' if TUP_`v'==0, absorb(subcounty) cluster(village)

				local treat_B`i': di %7.3f _b[treat]
				local treat_SE`i'=trim(string(_se[treat], "%7.3f"))

				* stars
				local t=(_b[treat])/(_se[treat])
				local p=2*ttail(e(df_r), abs(`t'))
					if `p'<=.10 local stars "*"
					if `p'<=.05 local stars "**"
					if `p'<=.01 local stars "***"
					else if `p'>.10 local stars ""

				local leeup_B`i' "`treat_B`i''`stars'"
				local leeup_coeff_list "`leeup_coeff_list' & `leeup_B`i''"
				local leeup_se_list "`leeup_se_list' & [`treat_SE`i'']"

			di _n "*****   Bound on magnitude" _n
			areg `v' treat `strat' if TABS_`v'==0, absorb(subcounty) cluster(village)
			}
		}

	cap file close sumstat
	file open sumstat using "Test tables and figures/tab_treat_treeplant.tex", write replace

	file write sumstat "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{9}{>{\centering\arraybackslash}m{1.9cm}}@{}}  \toprule" _n
	file write sumstat "\addlinespace[3mm]" _n
	file write sumstat "`label_list'  \\" _n
	file write sumstat "`col_list'  \\" _n
	file write sumstat "\midrule" _n


	file write sumstat "Treated `treat_coeff_list'   \\     " _n
	file write sumstat "		`treat_se_list' 	\\  \addlinespace   " _n
	file write sumstat "\addlinespace[5mm]" _n


	file write sumstat "Lee bound (lower) & & & & `leelow_coeff_list'   \\     " _n
	file write sumstat "					& & & & `leelow_se_list' 	\\  \addlinespace   " _n

	file write sumstat "Lee bound (upper) & & & & `leeup_coeff_list'   \\     " _n
	file write sumstat "					& & & & `leeup_se_list' 	\\  \addlinespace   " _n

	file write sumstat "\midrule " _n
	file write sumstat "\addlinespace" _n
	file write sumstat "Control group mean `mean_list'  \\" _n
	file write sumstat "Control group SD `sd_list'  \\" _n

	file write sumstat "Observations `obs_list'  \\" _n
	file write sumstat "Observations (Lee bounds)  & & & & `obslee_list'  \\" _n

	file write sumstat "\bottomrule" _n
	file write sumstat "\end{tabular}" _n
	file close sumstat
	macro drop _all


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table S5: Effect of the PES program on tree cover with unweighted regressions
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

		use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil
		local controls_act photo1991_act photo2011_act pfo_satdate_*
		local ihs_controls_act ihs_photo1991_act ihs_photo2011_act pfo_satdate_*

		eststo clear

	// PFO-level
	foreach stub in _act {
		// Changes
		eststo: areg change_fcover`stub' treat `strat' , absorb(subcounty) cluster(village)
			estadd local control "No"
				quietly: sum change_fcover`stub' if treat==0
			estadd scalar varmean=r(mean)

		eststo: areg change_fcover`stub' treat photo1991_vil photo2011_vil `controls`stub'' `strat', absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum change_fcover`stub' if treat==0
			estadd scalar varmean=r(mean)

		eststo: areg ihs_diff_fcover`stub' treat ihs_photo1991_vil ihs_photo2011_vil `ihs_controls`stub'' `strat', absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum ihs_diff_fcover`stub' if treat==0
			estadd scalar varmean=r(mean)
		}

	// Village-level
		bysort village: gen N=_n
		keep if N==1 // one observation per village

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil
		local controls photo1991_vil photo2011_vil village_satdate_*
		local ihs_controls ihs_photo1991_vil ihs_photo2011_vil village_satdate_*

	eststo: areg change_fcover_vil treat `strat' , absorb(subcounty) cluster(village)
		estadd local control "No"
			quietly: sum change_fcover_vil if treat==0
		estadd scalar varmean=r(mean)

	eststo: areg change_fcover_vil treat total_photo1991_act total_photo2011_act `controls' `strat', absorb(subcounty) cluster(village)
		estadd local control "Yes"
			quietly: sum change_fcover_vil if treat==0
		estadd scalar varmean=r(mean)

	eststo: areg ihs_diff_fcover_vil treat ihs_total_photo1991_act ihs_total_photo2011_act `ihs_controls' `strat', absorb(subcounty) cluster(village)
		estadd local control "Yes"
			quietly: sum ihs_diff_fcover_vil if treat==0
		estadd scalar varmean=r(mean)

	 #delimit ;
	esttab est4 est5 est6 est1 est2 est3 using "Test tables and figures/tab_sat_noweight.tex",
		b(a3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets replace gaps label booktabs style(tex)
		keep(treat)
		prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
		\begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{8}{>{\centering\arraybackslash}m{2cm}}}
		\toprule \addlinespace[3mm]
		& \multicolumn{3}{c}{Village boundaries} &\multicolumn{3}{c}{PFO-level land circles}  \\
		\cmidrule(lr){2-4} \cmidrule(lr){5-7}
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Log of tree cover
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ IHS of tree cover \\ ")
		stats(varmean control N,
		labels("Control group mean" "Control variables" "Observations")
		fmt(%7.3fc %3.0s %7.0fc));
	#delimit cr



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table S6: Effect of the PES program on tree cover, removing outliers
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

		use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil
		local controls_act photo1991_act photo2011_act pfo_satdate_*
		local controls_med photo1991_med photo2011_med pfo_satdate_*
		local ihs_controls_act ihs_photo1991_act ihs_photo2011_act pfo_satdate_*
		local ihs_controls_med ihs_photo1991_med ihs_photo2011_med pfo_satdate_*

		eststo clear

	// PFO-level (dropping top 1%)
	foreach stub in _act{
		preserve
		summ base_fcover`stub', detail
		drop if base_fcover`stub'>`r(p99)'

		eststo: areg change_fcover`stub' treat `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "No"
				quietly: sum change_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)

		eststo: areg change_fcover`stub' treat photo1991_vil photo2011_vil `controls`stub'' `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum change_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)

		eststo: areg ihs_diff_fcover`stub' treat ihs_photo1991_vil ihs_photo2011_vil `ihs_controls`stub'' `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum ihs_diff_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)
		restore
		}

	// PFO-level (Median)
	foreach stub in _med{
		eststo: areg change_fcover`stub' treat `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "No"
				quietly: sum change_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)

		eststo: areg change_fcover`stub' treat photo1991_vil photo2011_vil `controls`stub'' `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum change_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)

		eststo: areg ihs_diff_fcover`stub' treat ihs_photo1991_vil ihs_photo2011_vil `ihs_controls`stub'' `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum ihs_diff_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)
		}

	 #delimit ;
	esttab using "Test tables and figures/tab_sat_top99_med.tex",
		b(a3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets replace gaps label booktabs style(tex)
		keep(treat)
		prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
		\begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{8}{>{\centering\arraybackslash}m{2cm}}}
		\toprule \addlinespace[3mm]
		&\multicolumn{3}{c}{PFO-level land circles} &\multicolumn{3}{c}{PFO-level land circles} \\
		&\multicolumn{3}{c}{(dropping top 1\%)} &\multicolumn{3}{c}{(median-sized)} \\
		\cmidrule(lr){2-4} \cmidrule(lr){5-7}
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ IHS of tree cover
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ IHS of tree cover\\ ")
		stats(varmean control N,
		labels("Control group mean" "Control variables" "Observations")
		fmt(%7.3fc %3.0s %7.0fc));
	#delimit cr



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table S7: Effect of the PES program on tree cover using different-sized land circles
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

		use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil
		local controls_act_x1 photo1991_act_x1 photo2011_act_x1 pfo_satdate_*
		local controls_act_x3 photo1991_act_x3 photo2011_act_x3 pfo_satdate_*
		local ihs_controls_act_x1 ihs_photo1991_act_x1 ihs_photo2011_act_x1 pfo_satdate_*
		local ihs_controls_act_x3 ihs_photo1991_act_x3 ihs_photo2011_act_x3 pfo_satdate_*

		eststo clear

	// PFO-level
	foreach stub in _act_x1 _act_x3{

		eststo: areg change_fcover`stub' treat `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "No"
				quietly: sum change_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)

		eststo: areg change_fcover`stub' treat photo1991_vil photo2011_vil `controls`stub'' `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum change_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)

		eststo: areg ihs_diff_fcover`stub' treat ihs_photo1991_vil ihs_photo2011_vil `ihs_controls`stub'' `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum ihs_diff_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)
		}


	 #delimit ;
	esttab using "Test tables and figures/tab_sat_circlesize.tex",
		b(a3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets replace gaps label booktabs style(tex)
		keep(treat)
		prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
		\begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{8}{>{\centering\arraybackslash}m{2cm}}}
		\toprule \addlinespace[3mm]
		&\multicolumn{3}{c}{PFO-level land circles (x1)} &\multicolumn{3}{c}{PFO-level land circles (x3)} \\
		\cmidrule(lr){2-4} \cmidrule(lr){5-7}
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ IHS of tree cover
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ IHS of tree cover \\ ")
		stats(varmean control N,
		labels("Control group mean" "Control variables" "Observations")
		fmt(%7.3fc %3.0s %7.0fc));
	#delimit cr

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table S8: Effect of the PES program on tree cover in subsample with baseline satellite data prior to randomization
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

		use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1
			preserve
		drop if dummyout==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil
		local controls_act photo1991_act photo2011_act pfo_satdate_*
		local ihs_controls_act ihs_photo1991_act ihs_photo2011_act pfo_satdate_*

		eststo clear

	// PFO-level (subsample with baseline QB done before randomization)
	foreach stub in _act{
		// Changes
		eststo: areg change_fcover`stub' treat `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "No"
				quietly: sum change_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)

		eststo: areg change_fcover`stub' treat photo1991_vil photo2011_vil `controls`stub'' `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum change_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)

		eststo: areg ihs_diff_fcover`stub' treat ihs_photo1991_vil ihs_photo2011_vil `ihs_controls`stub'' `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local control "Yes"
				quietly: sum ihs_diff_fcover`stub' if treat==0 [aw = SPct_Data`stub']
			estadd scalar varmean=r(mean)
		}

	// Village-level
	restore
		bysort village: gen N=_n
		keep if N==1 // one observation per village
		drop if dummyout_vill==1 //Drop villages for which the centroid was imaged after the lottery.

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil
		local controls photo1991_vil photo2011_vil village_satdate_*
		local ihs_controls ihs_photo1991_vil ihs_photo2011_vil village_satdate_*

	eststo: areg change_fcover_vil treat `strat' [aw = SPct_Data_vil], absorb(subcounty) cluster(village)
		estadd local control "No"
			quietly: sum change_fcover_vil if treat==0 [aw = SPct_Data_vil]
		estadd scalar varmean=r(mean)

	eststo: areg change_fcover_vil treat total_photo1991_act total_photo2011_act `controls' `strat' [aw = SPct_Data_vil], absorb(subcounty) cluster(village)
		estadd local control "Yes"
			quietly: sum change_fcover_vil if treat==0 [aw = SPct_Data_vil]
		estadd scalar varmean=r(mean)

	eststo: areg ihs_diff_fcover_vil treat ihs_total_photo1991_act ihs_total_photo2011_act `ihs_controls' `strat' [aw = SPct_Data_vil], absorb(subcounty) cluster(village)
		estadd local control "Yes"
			quietly: sum ihs_diff_fcover_vil if treat==0 [aw = SPct_Data_vil]
		estadd scalar varmean=r(mean)


	 #delimit ;
	esttab est4 est5 est6 est1 est2 est3 using "Test tables and figures/tab_sat_BLbeforelot.tex",
		b(a3) se(3) star(* .1 ** .05 *** .01) nonotes nomtitles number brackets replace gaps label booktabs style(tex)
		keep(treat)
		prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
		\begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{8}{>{\centering\arraybackslash}m{2cm}}}
		\toprule \addlinespace[3mm]
		& \multicolumn{3}{c}{Village boundaries} &\multicolumn{3}{c}{PFO-level land circles} \\
		\cmidrule(lr){2-4} \cmidrule(lr){5-7}
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Log of tree cover
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ IHS of tree cover \\ ")
		stats(varmean control N,
		labels("Control group mean" "Control variables" "Observations")
		fmt(%7.3fc %3.0s %7.0fc));
	#delimit cr



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table S9: Heterogeneous effects of the PES program on tree cover
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

		use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

		local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil
		local controls photo1991_act photo2011_act pfo_satdate_*
		local takeupdet hhead_age hhead_educ ihs_land_area forest_area any_tree_cut cut_cult cut_timber ///
						cut_emerg ihs_tim_price rent disputewith3 envprog_bi envprob_9 opi_hurtenv_agr  ///
						fcover_change9111

		// Predicted change in tree cover
		foreach var in `takeupdet' {
			gen `var'_flag=mi(`var')
				qui: sum `var'
				replace `var'=`r(mean)' if mi(`var')
				local flag_list3 `flag_list3' `var'_flag
				}

		areg change_fcover_act `strat' `takeupdet' `flag_list3' [aw = SPct_Data_act] if treat==0 & partid!=2182, absorb(subcounty) cluster(village)

		* Prediction for treatment uses all control observations (except partid=2182)
			predict predicted_fcover //if treat==1
				label var predicted_fcover "Predicted change in tree cover"

		* Prediction for control uses leave-out-out approach
			sort treat partid
			count if treat==0
			local N=`r(N)'
		forval x=1/`r(N)' {
			areg change_fcover_act `strat' `takeupdet' `flag_list3' [aw = SPct_Data_act] if treat==0 & _n!=`x'& partid!=2182, absorb(subcounty) cluster(village)
				local r2=`r2'+`e(r2)'
				predict predicted_fcover_control if _n==`x'
				replace predicted_fcover=predicted_fcover_control if _n==`x'
				drop predicted_fcover_control
			}

		* Interaction terms
		foreach var in hi_satbasefarea perc_forest any_tree_cut cut_cult cut_timber cut_emerg ihs_tim_price predicted_fcover {
			gen treatX`var'= treat*`var'
				local l: variable label `var'
				label var treatX`var' "Treat $\times$ `l'"
			}

	eststo clear
	foreach het in hi_satbasefarea perc_forest any_tree_cut cut_cult cut_timber cut_emerg ihs_tim_price predicted_fcover{
		cap drop het treatXhet
		gen het=`het'
		gen treatXhet=treatX`het'
		if "`het'"!="predicted_fcover" eststo: areg change_fcover_act treat photo1991_vil photo2011_vil `controls' treatXhet het `strat' [aw = SPct_Data_act], absorb(subcounty) cluster(village)
		if "`het'"=="predicted_fcover" eststo: areg change_fcover_act treat photo1991_vil photo2011_vil `controls' treatXhet het `strat' [aw = SPct_Data_act] if partid!=2182, absorb(subcounty) cluster(village)
		}

		label var treatXhet "Treat $\times$ Characteristic"
		label var het "Characteristic"

	 #delimit ;
	esttab using "Test tables and figures/tab_act_weight_hetero.tex",
		b(3) se star(* .1 ** .05 *** .01) nonotes nomtitles number brackets replace gaps label booktabs style(tex)
		keep(treatXhet treat het) order(treatXhet treat het)
		prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
		\begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{8}{>{\centering\arraybackslash}m{2cm}}}
		\toprule \addlinespace[3mm]
		& \multicolumn{8}{c}{\textit{Heterogeneous treatment effects on $\Delta\!\!\!$ Tree cover (ha) by:}} \\
		\cmidrule(lr){2-9}\\
		& Above-median tree cover in land circle & \% of land circle with tree cover
		& Cut any trees in the last 3 years & Cut trees to clear land for cultivation & Cut trees for timber products
		& Cut trees for emergency/lumpy expenses & IHS of total revenue from cut trees & Predicted change in tree cover\\ ")
		stats(N,
		labels("Observations")
		fmt(%7.0fc));
	#delimit cr



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Table S10: Testing for spillover effects and anticipation effects
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	use "Data/PES_analysis_science.dta", clear
		keep if sample_gps==1

	local strat num_PFOs_vil inc_pc_vil dist_road_vil l1_area_vil
	local het prog_likely prog_cont2015plus
	local controls_act photo1991_act photo2011_act pfo_satdate_*
	local controls_med photo1991_med photo2011_med pfo_satdate_*
	local controls photo1991_vil photo2011_vil village_satdate_* total_base_fcover_act

	// Dummy out baseline timber dealer visits
	foreach var in timvisit_y{
		gen `var'_flag=mi(`var')
			qui: sum `var'
			replace `var'=`r(mean)' if mi(`var')
		}

		* Interaction terms
		foreach var in dist_reserve cont_forest {
			gen treatX`var'= treat*`var'
				local l: variable label `var'
				label var treatX`var' "Treat $\times$ `l'"
			}

	bysort village: gen N=_n // for village-level regression

	eststo clear

	foreach stub in _act {

		eststo: areg change_fcover`stub' treat photo1991_vil photo2011_vil `controls`stub'' treatXdist_reserve dist_reserve `strat' [aw = SPct_Data`stub'], absorb(subcounty) cluster(village)
			estadd local sample "PFOs"

		eststo: areg change_fcover_vil treat total_photo1991_act total_photo2011_act `controls' treatXcont_forest cont_forest `strat' [aw = SPct_Data_vil] if N==1, absorb(subcounty) cluster(village)
			estadd local sample "Villages"

		eststo: areg change_fcover`stub' treat_5km photo1991_vil photo2011_vil `controls`stub'' `strat' [aw = SPct_Data`stub'] if treat==0, absorb(subcounty) cluster(village)
			estadd local sample "Control PFOs"
		}

	eststo: areg Ftimvisit_y treat timvisit_y `strat' timvisit_y_flag, absorb(subcounty) cluster(village)
			estadd local sample "PFOs"

	eststo: areg Ftimvisit_inc treat `strat', absorb(subcounty) cluster(village)
			estadd local sample "PFOs"

	foreach hetero in `het'{
		eststo: areg change_fcover_act photo1991_vil photo2011_vil `controls_act' `strat' `hetero' [aw = SPct_Data_act], absorb(subcounty) cluster(village)
			if "`hetero'"=="prog_likely" estadd local sample "Control PFOs"
			else estadd local sample "Treatment PFOs"
		}

	 #delimit ;
	esttab using "Test tables and figures/tab_sat_shifting.tex",
		b(3) se star(* .1 ** .05 *** .01) nonotes nomtitles number brackets replace gaps label booktabs style(tex)
		keep(treat treatXdist_reserve treatXcont_forest treat_5km prog_likely prog_cont2015plus)
		order(treat treatXdist_reserve treatXcont_forest treat_5km prog_likely prog_cont2015plus)
		prehead("{\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
		\begin{tabular}{@{\hskip\tabcolsep\extracolsep\fill}l*{8}{>{\centering\arraybackslash}m{1.8cm}}}
		\toprule \addlinespace[3mm]
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha)
		& Get visits from timber dealers & Increase in timber dealer visits last 2 years
		& $\Delta\!\!\!$ Tree cover (ha) & $\Delta\!\!\!$ Tree cover (ha) \\ ")
		stats(sample N,
		labels("Sample" "Observations")
		fmt(%3.0s %7.0fc));
	#delimit cr



	***********************************************************
	********************	Figures	   ************************
	***********************************************************


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Figure S3: Proportion of village and PFO polygons with available tree-classification data
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	use "Data/PES_analysis_science.dta", clear
	keep if sample_gps==1

	preserve
		bysort village: gen N=_n
		keep if N==1 // one observation per village
		tempfile x
		save `x', replace
	restore

	app using `x', gen(vill)

	#delim ;
	twoway hist SPct_Data_act if vill==0, scheme(indo) bin(10) fc(gs12) fi(50) lc(white) xtitle(`"{fontface "Helvetica":Percent of observation with available tree-classification data}"') ytitle(`"{fontface "Helvetica":Percent}"')
	|| hist SPct_Data_vil if vill==1, bin(9) fc(none) lc(black) lw(medthick) legend(on col(1) lab(1 `"{fontface "Helvetica":PFO circles}"') lab(2 `"{fontface "Helvetica":Village boundaries}"')) ;
	graph export "Test tables and figures\pct_avail_act_vil_overlain.pdf", replace;
	#delim cr



* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Figure S4: Reasons for not enrolling in the program
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	use "Data/PES_analysis_science.dta", clear
	keep if sample_gps==1

	#delimit ;
	graph pie, over(notakeup_why) graphregion(color(white)) plabel(_all percent, format(%4.1fc) color(white) size(medsmall) gap(5)) ///
	legend(on position(6) cols(2) size(small)) ///
	li(lwidth(.4) lcolor(gs12)) pie(1, color(navy)) pie(2, color(khaki)) ///
	pie(3, color(forest_green));
	graph export "Test tables and figures\fig_takeup.pdf", replace;
	#delimit cr


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*		Figure S5: Distribution of control group's baseline to endline change in tree cover
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

	use "Data/PES_analysis_science.dta", clear
	keep if sample_gps==1

	//xlines at 32% in control distribution from both sides
	sort treat ln_diff_fcover_act
	count if treat==0 & !mi(ln_diff_fcover_act) //valid var for control sample
	gen bot=ceil(r(N)*0.32)
	gen top=floor(r(N)*0.68)

	sum change_fcover_act if _n==bot
	local bot=r(mean)
	sum change_fcover_act if _n==top
	local top=r(mean)

	gen x=0 if _n==1
	replace x=`bot' if _n==2
	drop top bot

	twoway (kdensity change_fcover_act [aw = SPct_Data_act] if treat==0, range(-1 1) lw(medthick)), ///
		ytitle("Density") xtitle("Change in tree cover (ha)") legend(off) ///
		xtick(-1(0.2)1) xlabel(-1(0.2)1) scheme(plotplain) ///
		xline(`bot', lc(gs11) lw(thick) lp(shortdash)) xline(`top', lc(gs11) lw(thick) lp(shortdash))
	graph export "Test tables and figures\kdensity_diff_fcover_cg.pdf", replace
