//Cleans policy timing data

cd "$main\policydata\"

insheet using dates_cleaned.csv, comma names clear
foreach var of varlist auth* {
gen date`var' = date(`var', "MDY")
drop `var'
format date`var' %td
}
gen authyear = year(dateauth_npdes)
drop if state_code==""
save policydata.dta, replace
