*** This code replicates Figures 1-3 presented in Kim and Urpelainen "The Polarization of American Environmental Policy: A Regression Discontinuity Analysis of Senate and House Votes, 1971-2013" Review of Policy Research

clear all
cd "~/Dropbox/Legislators Voting/Replication Package"
use "Legislative Vote on Environmental Bills.dta"

*** Figure 2:  Graphical Summary of Marginal Effects of Democrats across Time, Issue Areas, and Regions
	
	gen n = _n	

	gen x_axis = 1 if _n < 6
	replace x_axis = 2 if _n > 5 & _n < 12
	replace x_axis = 3 if _n > 11 & _n < 18
	replace x_axis = 4 if _n > 17 & _n < 23
	replace x_axis = 5 if _n > 22 & _n < 29
	replace x_axis = 6 if _n > 28 & _n < 35
	
	label define label 1 "Time" 2 "Issue" 3 "Region" 4 "Time" 5 "Issue" 6 "Region"
	label values x_axis label  
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if year > 1969 & year < 1979 & office == "House", vce(cluster name)
	gen coefficient = _b[dem_win] if _n == 1
	gen marker = "1970s" if _n == 1

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if year > 1979 & year < 1989 & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 2
	replace marker = "1980s" if _n == 2
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if year > 1989 & year < 2000 & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 3
	replace marker = "1990s" if _n == 3

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if year > 1999 & year < 2010 & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 4
	replace marker = "2000s" if _n == 4

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if year > 2009  & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 5
	replace marker = "2010s" if _n == 5
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if lands == 1 & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 6
	replace marker = "Lands" if _n == 6

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if toxics == 1 & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 7
	replace marker = "Toxics" if _n == 7
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if water == 1 & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 8
	replace marker = "Water" if _n == 8
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if dirty_energy == 1 & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 9
	replace marker = "Dirty Energy" if _n == 9
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if clean_energy == 1 & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 10
	replace marker = "Clean Energy" if _n == 10
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if climate_change == 1 & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 11
	replace marker = "Climate Change" if _n == 11
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Atlantic" & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 12
	replace marker = "Atlantic" if _n == 12

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Midwest" & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 13
	replace marker = "Midwest" if _n == 13
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Northeast" & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 14
	replace marker = "Northeast" if _n == 14

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Northwest" & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 15
	replace marker = "Northwest" if _n == 15

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Southeast"  & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 16
	replace marker = "Southeast" if _n == 16

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Southwest"  & office == "House", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 17
	replace marker = "Southwest" if _n == 17

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if year > 1969 & year < 1979 & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 18
	replace marker = "1970s" if _n == 18

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if year > 1979 & year < 1989 & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 19
	replace marker = "1980s" if _n == 19
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if year > 1989 & year < 2000 & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 20
	replace marker = "1990s" if _n == 20
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if year > 1999 & year < 2010 & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 21
	replace marker = "2000s" if _n == 21
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if year > 2009  & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 22
	replace marker = "2010s" if _n == 22
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if lands == 1 & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 23
	replace marker = "Lands" if _n == 23

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if toxics == 1 & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 24
	replace marker = "Toxics" if _n == 24
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if water == 1 & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 25
	replace marker = "Water" if _n == 25

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if dirty_energy == 1 & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 26
	replace marker = "Dirty Energy" if _n == 26

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if clean_energy == 1 & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 27
	replace marker = "Clean Energy" if _n == 27
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if climate_change == 1 & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 28
	replace marker = "Climate Change" if _n == 28

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Atlantic" & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 29
	replace marker = "Atlantic" if _n == 29

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Midwest" & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 30
	replace marker = "Midwest" if _n == 30
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Northeast" & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 31
	replace marker = "Northeast" if _n == 31
	
	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Northwest" & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 32
	replace marker = "Northwest" if _n == 32

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Southeast"  & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 33
	replace marker = "Southeast" if _n == 33

	reg pro_env dem_win dem_share dem_share_sq dem_share_cube if region == "Southwest"  & office == "Senate", vce(cluster name)
	replace coefficient = _b[dem_win] if _n == 34
	replace marker = "Southwest" if _n == 34
	
	egen clock = mlabvpos(coefficient x_axis)
	
	twoway (scatter coefficient x_axis, mcolor(black) msize(small) mlabel(marker) mlabsize(small) mlabcolor(black)), yscale(range(0 1)) ylabel(0(0.2)1) /// 
		xscale(range(0.5 6.5)) xlabel(, labels valuelabel noticks) xtitle(House                                               Senate, size(medium))  graphregion(fcolor(white)) ytitle(Coefficients)

*** Figure 3 (Upper): Graphical illustration of marginal effects of democrats conditional on the share of oil and gas PAC contributions for Democrat (Standardized)

regress pro_env dem_win dem_win_z2_oil z2_dem_oilgas_share dem_win_z2_diff_pro z2_diff_pro dem_win_z2_fossil_resource z2_fossil_resource i.year, vce(cluster name)
summarize z2_dem_oilgas_share if e(sample) == 1

gen marginal_effects = .

forvalues i = 1(1)22{
	scalar z2_oilgas_share = (-9 + `i')/10
	predictnl ci`i' = _b[dem_win] + _b[dem_win_z2_oil]*z2_oilgas_share, ci(ci_l`i' ci_u`i') 
	replace marginal_effects = _b[dem_win] + _b[dem_win_z2_oil]*z2_oilgas_share if _n == `i'
}

gen ci_l = .
gen ci_u = .
forvalues i = 1(1)22 {
	replace ci_l = ci_l`i' if _n == `i'
	replace ci_u = ci_u`i' if _n == `i'
}
	
gen x = (-9 + _n)/10

label var x "Share of Oil and Gas PAC for Democrat (Standardized)"
label var marginal_effects "Marginal Effects of Democrats"

twoway (rarea ci_u ci_l x if x<=1.3, color(gs8)) ///
	   (line marginal_effects x if x <=1.3, legend(off) fcolor(white) lcolor(black) lpattern(dash) lwidth(med) yscale(range(0 1)) ylabel(0(0.2)1) xlabel(-0.8(0.2)1.3)), xline(0, lpattern(dash) lcolor(gs10)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ytitle("Marginal Effects of Democrats", size(medlarge))

*** Figure 3 (Lower): Marginal Effects Graph (Close Elections)

drop marginal_effects
drop ci_l* ci_u* ci* x

regress pro_env dem_win dem_win_z2_oil z2_dem_oilgas_share dem_win_z2_diff_pro z2_diff_pro dem_win_z2_fossil_resource z2_fossil_resource i.year if dem_share > 49.5 & dem_share < 50.5, vce(cluster name)


gen marginal_effects = .

forvalues i = 1(1)22{
	scalar z2_oilgas_share = (-9 + `i')/10
	predictnl ci`i' = _b[dem_win] + _b[dem_win_z2_oil]*z2_oilgas_share, ci(ci_l`i' ci_u`i') 
	replace marginal_effects = _b[dem_win] + _b[dem_win_z2_oil]*z2_oilgas_share if _n == `i'
}

gen ci_l = .
gen ci_u = .
forvalues i = 1(1)22 {
	replace ci_l = ci_l`i' if _n == `i'
	replace ci_u = ci_u`i' if _n == `i'
}
	
gen x = (-9 + _n)/10

label var x "Share of Oil and Gas PAC for Democrat (Standardized)"
label var marginal_effects "Marginal Effects of Democrats"

twoway (rarea ci_u ci_l x if x<=1.3, color(gs8)) ///
	   (line marginal_effects x if x <=1.3, legend(off) fcolor(white) lcolor(black) lpattern(dash) lwidth(med) yscale(range(0 1)) ylabel(0(0.2)1) xlabel(-0.8(0.2)1.3)),  xline(0, lpattern(dash) lcolor(gs10)) graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white)) ytitle("Marginal Effects of Democrats", size(medlarge))
		
*** Figure 1:  Graphical Summary of Marginal Effects of Democrats across Time, Issue Areas, and Regions
 
    *** Note that you need to install cmogram package before running the following lines
	*** ssc install cmogram
	
bysort year name: egen mean_pro_env = mean(pro_env)
duplicates drop year name, force

cmogram mean_pro_env dem_share if office == "Senate", cut(50) scatter line(50) qfit graphopts (ytitle(Mean of Pro-Environmental Voting) yscale(range(0 1)) xtitle(Vote Share of Democrats (Senate)))

cmogram mean_pro_env dem_share if office == "House", cut(50) scatter line(50) qfit graphopts (ytitle(Mean of Pro-Environmental Voting) yscale(range(0 1)) xtitle(Vote Share of Democrats (House)))



		
