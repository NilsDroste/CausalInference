
cd "$main\compliancedata\npdes_downloads\"

//ENFORCEMENT ACTION
local enforcement = 1
if `enforcement'==1 {
insheet using NPDES_FORMAL_ENFORCEMENT_ACTIONS.csv, comma clear names 
tempfile formal
save `formal'
insheet using NPDES_INFORMAL_ENFORCEMENT_ACTIONS.csv, comma clear names 
append using `formal'
gen achieveddate = date(achieved_date, "MDY")
gen settledate = date(settlement_entered_date, "MDY")
format achieveddate settledate %td
gen year = year(achieveddate)
replace year = year(settledate) if year==.
drop if year<1900 | year>2019
gen enforce = 1
collapse (max) enforce, by(npdes_id year)
save enforcements.dta, replace
}
//NPDES INSPECTIONS
local inspection = 1
if `inspection'==1 {
insheet using NPDES_INSPECTIONS.csv, comma clear names

gen begindate = date(actual_begin_date, "MDY")
gen enddate = date(actual_end_date, "MDY")
format begindate enddate %td
replace begindate=. if year(begindate)<1900

gen yr1 = year(begindate)
gen yr2 = year(enddate)
replace yr2 =. if yr1==yr2 //start and end in same year

reshape long yr, i(npdes_id activity_id comp_monitor_type_code) j(year)
drop if yr==.
drop year
rename yr year
egen facid = group(npdes_id activity_id comp_monitor_type_code)
tsset facid year
count
tsfill
count

bysort facid: carryforward npdes_id, gen(temp)
drop npdes_id
gsort facid -year
bysort facid: carryforward temp, gen(npdes_id)
gen inspect = 1
drop if year<1900
collapse (max) inspect, by(npdes_id year)
drop if npdes_id=="" | year==.
save inspections.dta, replace
}
//PERMITS
insheet using ICIS_PERMITS.csv, comma names clear
gen major = (major_minor_status_flag=="M") //Major v. non-major dischargers
rename external_permit_nmbr npdes_id
gen originalissuedate = date(original_issue_date, "MDY")
format originalissuedate %td

gen npd = (permit_type_code=="NPD")

collapse (min) originalissuedate (max) major npd (firstnm) permit_type_code , by(npdes_id)

merge 1:1 npdes_id using icis_facilities.dta, keepusing(state_code)
drop if _merge!=3
drop _merge

tempfile major
save `major'

//VIOLATIONS
insheet using NPDES_QNCR_HISTORY.csv, clear comma names
tostring yearqtr, replace
gen year = substr(yearqtr,1,4)
gen qtr = substr(yearqtr,5,1)
drop if year=="2020" | year=="2030"
collapse (max) num* ,by(npdes_id year)
destring year, replace
//MERGE WITH INSPECTIONS, ENFORCEMENTS, MAJOR/MINOR STATUS
merge 1:1 npdes_id year using enforcements.dta
	tab _merge
	drop _merge
merge 1:1 npdes_id year using inspections.dta
	tab _merge
	drop _merge
merge m:1 npdes_id using `major'
	drop if _merge==2 
	drop _merge
	
*gen state_code = substr(npdes_id,1,2)
drop if state_code==""
drop if state_code=="AS" | state_code=="GE" | state_code=="GM" | state_code=="GU" | state_code=="MP" | state_code=="PR" | state_code=="VI" | state_code=="DC" | state_code=="CZ" | state_code=="GB" | state_code=="JA" | state_code=="SR" | state_code=="MW" | state_code=="NN"  | state_code=="05"

egen tag = tag(npdes_id)
bysort state: egen totfac = sum(tag)
keep if year<=2008 & year>=1976
foreach var of varlist num* {
gen d_`var' = (`var'>0)
replace d_`var' = . if `var'==.
}

collapse (sum) d_* inspect enforce (firstnm) totfac major, by(state year)
sum totfac
gen violrate = (d_nume90q+d_numcvdt+d_numsvcd+d_numpsch)/totfac
gen inspectrate = inspect/totfac
gen enforcerate = enforce/totfac

egen state = group(state_code)
tsset state year
tsfill, full
bysort state: carryforward totfac, gen(tf)
bysort state: carryforward state_code, gen(sc)
drop totfac state_code
gsort state -year
bysort state: carryforward tf, gen(totfac)
bysort state: carryforward sc, gen(state_code)

replace violrate = 0 if violrate==.
replace inspectrate = 0 if inspectrate==.
replace enforcerate = 0  if enforcerate==.
sum  inspectrate violrate  enforcerate totfac

save npdesdata_2.dta, replace
