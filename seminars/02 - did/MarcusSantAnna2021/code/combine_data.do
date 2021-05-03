//combine data for regression

cd "$main"
use compliancedata/npdes_downloads/npdesdata_2.dta, clear

*merge with policy adoption data
merge m:1 state_code using policydata/policydata.dta, keepusing(authyear)
	tab _merge
	drop if _merge==2
	drop _merge

replace authyear = . if authyear==2018 //outside the sample and not included in the paper
	
gen after = (year>=authyear)
gen tau = year-authyear
	
*merge with corruption data
merge m:1 state_code using corruptiondata/corruptiondata.dta
	drop _merge
*merge with state expenditure data
merge 1:1 state_code year using otherdata/statebudgetdata/stateexpenddata.dta
	drop _merge

drop d_num* tf sc inspect enforce

label var inspectrate "Inspection Rate"
label var enforcerate "Enforcement Rate"
label var violrate "Violation Rate"
label var totfac "Total Facilities Ever"
label var authyear "Year of State Authorization"
label var after "After State Authorization"
label var meancorrupt "Corruption Score"
label var corrupt "Above Median Corruption"
label var major "Major Polluter"
label var tau "Years relative to state authorization"
label var expend_health "State Budget: Health"
label var expend_natres "State Budget: Natural Resources"
label var expend_other "State Budget: Other"
	
//save
save	"$main\createddata\grooms_st_2.dta", replace
