//CLEAN CORRUPTION DATA
cd "$main\corruptiondata\"
insheet using rawdata.csv, comma names clear
keep if state_code!=""
drop if state_code=="PR" | state_code=="VI" | state_code=="DC"



//reshape 
reshape long y, i(state_code) j(year)
rename y corruptcount
//merge in pop data
merge 1:1 state_code year using ..\otherdata\popdata\popdata.dta
	drop if _merge==2
	drop _merge

//average corruption
bysort state_code: egen meancorrupt = mean(corruptcount/(pop/100000))

egen tag = tag(state_code)
keep if tag==1
sort meancorrupt

sum meancorrupt, detail
gen corrupt =(meancorrupt>r(p50))

keep state_code statename meancorrupt corrupt

//save
save corruptiondata.dta, replace
