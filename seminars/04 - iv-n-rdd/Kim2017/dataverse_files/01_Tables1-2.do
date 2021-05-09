*** This code replicates Tables 1-2 presented in Kim and Urpelainen "The Polarization of American Environmental Policy: A Regression Discontinuity Analysis of Senate and House Votes, 1971-2013" Review of Policy Research

clear all
cd "~/Dropbox/Legislators Voting/Replication Package" /*** Please change this to your own directory ***/
use "Legislative Vote on Environmental Bills.dta"

*** Table 1: Analyses of Legislative Voting on Environmental Bills by Senators and House Representatives

    *** OLS (Row 1) 

	eststo clear
	eststo: reg pro_env dem_win if office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb if office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb i.year if office == "Senate", vce(cluster name)

	eststo: reg pro_env dem_win if office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb if office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb i.year if office == "House", vce(cluster name)
	
esttab using "OLSAll.tex", booktabs label keep(dem_win) nodepvars se(3) b(3) replace star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles ("Senate" "" "" "House" "" "") compress nogaps

	*** RDD, 2nd-Order Polynomial (Row 2) 

	eststo clear
	eststo: reg pro_env dem_win dem_share dem_share_sq  if office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq  i.state_abb if office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq  i.state_abb i.year if office == "Senate", vce(cluster name)

	eststo: reg pro_env dem_win dem_share dem_share_sq  if office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq  i.state_abb if office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq  i.state_abb i.year if office == "House", vce(cluster name)

esttab using "2PolyAll.tex", booktabs label keep(dem_win) nodepvars se(3) b(3) replace star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles ("Senate" "" "" "House" "" "") compress nogaps

	*** RDD, 3nd-Order Polynomial (Row 3) 
	
	eststo clear
	eststo: reg pro_env dem_win dem_share dem_share_sq dem_share_cube if office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq dem_share_cube i.state_abb if office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq dem_share_cube i.state_abb i.year if office == "Senate", vce(cluster name)

	eststo: reg pro_env dem_win dem_share dem_share_sq dem_share_cube if office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq dem_share_cube i.state_abb if office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq dem_share_cube i.state_abb i.year if office == "House", vce(cluster name)

esttab using "3PolyAll.tex", booktabs label keep(dem_win) nodepvars se(3) b(3) replace star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles ("Senate" "" "" "House" "" "") compress nogaps

	*** RDD, 4th-Order Polynomial (Row 4) 
	
	eststo clear
	eststo: reg pro_env dem_win dem_share dem_share_sq dem_share_cube dem_share_fourth if office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq dem_share_cube dem_share_fourth i.state_abb if office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq dem_share_cube dem_share_fourth i.state_abb i.year if office == "Senate", vce(cluster name)

	eststo: reg pro_env dem_win dem_share dem_share_sq dem_share_cube dem_share_fourth if office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq dem_share_cube dem_share_fourth i.state_abb if office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win dem_share dem_share_sq dem_share_cube dem_share_fourth i.state_abb i.year if office == "House", vce(cluster name)
	
esttab using "4PolyAll.tex", booktabs label keep(dem_win) nodepvars se(3) b(3) replace star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles ("Senate" "" "" "House" "" "") compress nogaps

	*** RDD, 3% Margin (Row 5) 
	
	eststo clear
	eststo: reg pro_env dem_win if dem_share > 48.5 & dem_share < 51.5 & office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb if dem_share > 48.5 & dem_share < 51.5 & office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb i.year if dem_share > 48.5 & dem_share < 51.5 & office == "Senate", vce(cluster name)

	eststo: reg pro_env dem_win if dem_share > 48.5 & dem_share < 51.5 & office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb if dem_share > 48.5 & dem_share < 51.5 & office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb i.year if dem_share > 48.5 & dem_share < 51.5 & office == "House", vce(cluster name)

esttab using "3MarginAll.tex", booktabs label keep(dem_win) nodepvars se(3) b(3) replace star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles ("Senate" "" "" "House" "" "") compress nogaps

	*** RDD, 2% Margin (Row 6) 
	
	eststo clear
	eststo: reg pro_env dem_win if dem_share > 49 & dem_share < 51 & office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb if dem_share > 49 & dem_share < 51 & office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb i.year if dem_share > 49 & dem_share < 51 & office == "Senate", vce(cluster name)
	
	eststo: reg pro_env dem_win if dem_share > 49 & dem_share < 51 & office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb if dem_share > 49 & dem_share < 51 & office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb i.year if dem_share > 49 & dem_share < 51 & office == "House", vce(cluster name)

esttab using "2MarginAll.tex", booktabs label keep(dem_win) nodepvars se(3) b(3) replace star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles ("Senate" "" "" "House" "" "") compress nogaps

	*** RDD, 1% Margin (Row 7)
	
	eststo clear
	eststo: reg pro_env dem_win if dem_share > 49.5 & dem_share < 50.5 & office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb if dem_share > 49.5 & dem_share < 50.5 & office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb i.year if dem_share > 49.5 & dem_share < 50.5 & office == "Senate", vce(cluster name)
	
	eststo: reg pro_env dem_win if dem_share > 49.5 & dem_share < 50.5 & office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb if dem_share > 49.5 & dem_share < 50.5 & office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb i.year if dem_share > 49.5 & dem_share < 50.5 & office == "House", vce(cluster name)

esttab using "1MarginAll.tex", booktabs label keep(dem_win) nodepvars se(3) b(3) replace star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles ("Senate" "" "" "House" "" "") compress nogaps

	*** RDD, 0.5% Margin (Row 8)
	
	eststo clear
	eststo: reg pro_env dem_win if dem_share > 49.75 & dem_share < 50.25  & office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb if dem_share > 49.75 & dem_share < 50.25  & office == "Senate", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb i.year if dem_share > 49.75 & dem_share < 50.25  & office == "Senate", vce(cluster name)
	
	eststo: reg pro_env dem_win if dem_share > 49.75 & dem_share < 50.25 & office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb if dem_share > 49.75 & dem_share < 50.25 & office == "House", vce(cluster name)
	eststo: xi: reg pro_env dem_win i.state_abb i.year if dem_share > 49.75 & dem_share < 50.25 & office == "House", vce(cluster name)

esttab using "0.5MarginAll.tex", booktabs label keep(dem_win) nodepvars se(3) b(3) replace star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles ("Senate" "" "House" "") compress nogaps
	
*** Table 2: State-level Polarization of Public Opinion, Oil/Gas PAC Contributions, and Natural Resource Endowment as Modifying Factors

	eststo clear
	eststo: regress pro_env dem_win dem_win_z2_diff_pro z2_diff_pro i.year, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_diff_pro z2_diff_pro dem_share dem_share_sq i.year, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_diff_pro z2_diff_pro dem_share dem_share_sq dem_share_cube dem_share_fourth i.year, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_diff_pro z2_diff_pro i.year if dem_share > 48.5 & dem_share < 51.5, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_diff_pro z2_diff_pro i.year if dem_share > 49.5 & dem_share < 50.5, vce(cluster name)

esttab using "StandardizedInteractionPublicOpinion.tex", replace f ///
	booktabs b(3) se(3) eqlabels(none) alignment(C C) collabels(none) ///
	mtitles ("OLS" "2nd-Order" "4th-Order" "3\% Margin" "1\% Margin") ///
	keep(dem_win dem_win_z2_diff_pro z2_diff_pro) nonum nodepvars cells("b(fmt(3)star)" "se(fmt(3)par)") label star(* 0.10 ** 0.05 *** 0.01)

	
	eststo clear
	eststo: regress pro_env dem_win dem_win_z2_oil z2_dem_oilgas_share i.year, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_oil z2_dem_oilgas_share dem_share dem_share_sq i.year, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_oil z2_dem_oilgas_share dem_share dem_share_sq dem_share_cube dem_share_fourth i.year, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_oil z2_dem_oilgas_share i.year if dem_share > 48.5 & dem_share < 51.5, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_oil z2_dem_oilgas_share i.year if dem_share > 49.5 & dem_share < 50.5, vce(cluster name)

esttab using "StandardizedInteractionPAC.tex", replace f ///
	booktabs b(3) se(3) eqlabels(none) alignment(C C) collabels(none) ///
	mtitles ("" "" "" "" "") ///
	keep(dem_win dem_win_z2_oil z2_dem_oilgas_share) nodepvars cells("b(fmt(3)star)" "se(fmt(3)par)") label star(* 0.10 ** 0.05 *** 0.01)

	eststo clear
	eststo: regress pro_env dem_win dem_win_z2_fossil_resource z2_fossil_resource i.year, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_fossil_resource z2_fossil_resource dem_share dem_share_sq i.year, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_fossil_resource z2_fossil_resource dem_share dem_share_sq dem_share_cube dem_share_fourth i.year, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_fossil_resource z2_fossil_resource i.year if dem_share > 48.5 & dem_share < 51.5, vce(cluster name)
	eststo: regress pro_env dem_win dem_win_z2_fossil_resource z2_fossil_resource i.year if dem_share > 49.5 & dem_share < 50.5, vce(cluster name)

esttab using "StandardizedInteractionResource.tex", replace f ///
	booktabs b(3) se(3) eqlabels(none) alignment(C C) collabels(none) ///
	mtitles ("" "" "" "" "") ///
	keep(dem_win dem_win_z2_fossil_resource z2_fossil_resource) nonum nodepvars cells("b(fmt(3)star)" "se(fmt(3)par)") label star(* 0.10 ** 0.05 *** 0.01)
	
