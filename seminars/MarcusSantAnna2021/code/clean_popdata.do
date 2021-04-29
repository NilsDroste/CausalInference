//CLEAN POPULATION DATA
//Source: NBER (https://www.nber.org/data/seer_u.s._county_population_data.html)
//unadjusted data


cd "$main\otherdata\popdata\"

//collapse to state level
use uswbo19ages.dta, clear

collapse (sum) pop ,by(st year)
keep if year>=1976 & year<=2008
rename st state_code

save popdata.dta, replace
