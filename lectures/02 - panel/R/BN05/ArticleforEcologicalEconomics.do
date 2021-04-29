**************************************************************************************************
*** This do file creates the replication results for 
*** Environmental Pressure Group Strength and Air Pollution: An Empirical Analysis							 	*/
*** Seth Binder																*/
*** Eric Neumayer (LSE)																			*/
*** 																							*/
*** Published in: Ecological Economics, 55 (4), 2005, pp. 527-538												*/
**************************************************************************************************
**************************************************************************************************
/* Note: 
You have to change "local DIR" to the directory you copy the original stata files contained 	*/
/* in the zip file and then run the do file. 													*/
**************************************************************************************************

drop _all
clear matrix
clear mata

capture net install outreg2, from(http://fmwww.bc.edu/RePEc/bocode/o)			/* checks whether outreg2 is installed 		*/

***********************************************************************************
local DIR = "C:\Research\Environment\"  /*change relative path to the directory where the files are located */
cd "`DIR'"
***********************************************************************************

* Table 1
use "Binder smoke.dta", replace
su lnsmoke
use "Binder spm.dta", replace
su lnspm
use "Binder SO2.dta", replace
su lnso2 lnengopc lnenergy gini lngdp lngdpsq lngdpcu polity lnliter


* Table 2
use "Binder SO2.dta", replace
xi: reg lnso2med lnengopc lnenergy lngdp lngdpsq polity lnliter area indust residential lndens cencity coast i.year, robust
outreg lnengopc lnenergy lngdp lngdpsq polity lnliter using c:\table2, replace 3aster
xi: xtreg lnso2med lnengopc lnenergy lngdp lngdpsq polity lnliter area indust residential lndens cencity coast i.year, re
outreg lnengopc lnenergy lngdp lngdpsq polity lnliter using c:\table2, append 3aster
use "Binder smoke.dta", replace
xi: reg lnsmoke lnengopc lnenergy lngdp lngdpsq polity lnliter areaunkn indus residential cityunkn centcity lndens coast i.year, robust
outreg lnengopc lnenergy lngdp lngdpsq polity lnliter using c:\table2, append 3aster
xi: xtreg lnsmoke lnengopc lnenergy lngdp lngdpsq polity lnliter areaunkn indus residential cityunkn centcity lndens coast i.year, re
outreg lnengopc lnenergy lngdp lngdpsq polity lnliter using c:\table2, append 3aster
use "Binder spm.dta", replace
xi: reg lnspm lnengopc lnenergy lngdp lngdpsq lngdpcu polity lnliter resid cityunkn centcity lndens coast i.year, robust
outreg lnengopc lnenergy lngdp lngdpsq lngdpcu polity lnliter using c:\table2, append 3aster
xi: xtreg lnspm lnengopc lnenergy lngdp lngdpsq polity lnliter resid cityunkn centcity lndens coast i.year, re
outreg lnengopc lnenergy lngdp lngdpsq polity lnliter using c:\table2, append 3aster


* Table 4
use "Binder SO2.dta", replace
xi: ivreg2 lnso2med lnenergy lngdp lngdpsq polity lnliter area indust residential lndens cencity coast i.year (lnengopc = lningopc lningoparticip), robust 
outreg lnengopc lnenergy lngdp lngdpsq polity lnliter using c:\table4, replace 3aster
xi: xtivreg lnso2med lnenergy lngdp lngdpsq polity lnliter area indust residential lndens cencity coast i.year (lnengopc = lningopc lningoparticip), re
outreg lnengopc lnenergy lngdp lngdpsq polity lnliter using c:\table4, append 3aster
use "Binder smoke.dta", replace
xi: ivreg2 lnsmoke  lnenergy lngdp lngdpsq polity lnliter areaunkn indus residential cityunkn centcity lndens coast i.year (lnengopc = ingoparticip ingopc), robust
outreg lnengopc lnenergy lngdp lngdpsq polity lnliter using c:\table4, append 3aster
xi: xtivreg lnsmoke  lnenergy lngdp lngdpsq polity lnliter areaunkn indus residential cityunkn centcity lndens coast i.year (lnengopc = ingoparticip ingopc), re
outreg lnengopc lnenergy lngdp lngdpsq polity lnliter using c:\table4, append 3aster
use "Binder spm.dta", replace
xi: ivreg2 lnspm lnenergy lngdp lngdpsq lngdpcu polity lnliter resid cityunkn centcity lndens coast i.year (lnengopc = ingoparticip lnengopc72), robust 
outreg lnengopc lnenergy lngdp lngdpsq lngdpcu polity lnliter using c:\table4, append 3aster
xi: xtivreg lnspm lnenergy lngdp lngdpsq lngdpcu polity lnliter resid cityunkn centcity lndens coast i.year (lnengopc = ingoparticip lnengopc72), re
outreg lnengopc lnenergy lngdp lngdpsq lngdpcu polity lnliter using c:\table4, append 3aster


* Table 5
use "Binder SO2.dta", replace
xi: reg lnso2med lnengopc lnenergy lngini lngdp lngdpsq lngdpcu polity lnliter area indust residential lndens cencity coast i.year, robust
outreg lnengopc lnenergy lngini lngdp lngdpsq lngdpcu polity lnliter using c:\table5, replace 3aster
xi: xtreg lnso2med lnengopc lnenergy lngini lngdp lngdpsq lngdpcu polity lnliter area indust residential lndens cencity coast i.year, re
outreg lnengopc lnenergy lngini lngdp lngdpsq lngdpcu polity lnliter using c:\table5, append 3aster
use "Binder smoke.dta", replace
xi: reg lnsmoke lnengopc lnenergy lngini lngdp lngdpsq lngdpcu polity lnliter areaunkn indus residential cityunkn centcity lndens coast i.year, robust
outreg lnengopc lnenergy lngini lngdp lngdpsq lngdpcu polity lnliter using c:\table5, append 3aster
use "Binder spm.dta", replace
xi: reg lnspm lnengopc lnenergy lngini lngdp lngdpsq lngdpcu polity lnliter resid cityunkn centcity lndens coast i.year, robust
outreg lnengopc lnenergy lngini lngdp lngdpsq lngdpcu polity lnliter using c:\table5, append 3aster



