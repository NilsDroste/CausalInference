//clean state expenditure data
//source: https://www.census.gov/programs-surveys/gov-finances/data/historical-data.html
//file: State_Govt_Fin

cd "$main\otherdata\statebudgetdata\"

insheet using expendituresB.csv, clear comma names
tempfile b
save `b'

insheet using expendituresA.csv, clear comma names
merge 1:1 name year4  using `b'
	drop _merge

rename year4 year
keep if year>=1976 & year<=2008

gen state_code = substr(name, 1,2)
replace state_code="" if state_code=="EX" | state_code=="US"
drop if state_code==""

destring totalexpenditure healthtotalexpend totalnatrestotexp, replace ignore(",")

//merge in pop data
merge 1:1 state_code year using ..\popdata\popdata.dta
	drop if _merge==2
	drop _merge

gen expend_health = healthtotalexpend
gen expend_natres = totalnatrestotexp
gen expend_other = totalexpenditure-expend_health-expend_natres

foreach var of varlist expend_* {
replace `var' = `var'/pop
}

keep state_code year expend_*
save stateexpenddata.dta, replace
