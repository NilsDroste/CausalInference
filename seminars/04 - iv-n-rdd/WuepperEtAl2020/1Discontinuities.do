
use "Main.dta" , clear

***********

***PREP

gen pair = Country+"_"+Neighbor

gen pair2 = Neighbor+"_"+Country

gen first = cond(pair < pair2, pair, pair2)

gen second = cond(pair2 < pair, pair, pair2)

egen border = group(first second)

tab border

*Start

***********

*************

xi: regress z_altitude HIGHER_E distXhigher distXlower X Y XY i.border if Dist < 60000, vce(cluster border)

est store a

xi: regress z_altitude HIGHER_E distXhigher distXlower X Y XY i.border if Dist < 30000, vce(cluster border)

est store b

xi: regress z_temperature HIGHER_E distXhigher distXlower X Y XY i.border if Dist < 60000, vce(cluster border)

est store c

xi: regress z_temperature HIGHER_E distXhigher distXlower X Y XY i.border if Dist < 30000, vce(cluster border)

est store d

xi: regress z_precipitation HIGHER_E distXhigher distXlower X Y XY i.border if Dist < 60000, vce(cluster border)

est store e

xi: regress z_precipitation HIGHER_E distXhigher distXlower X Y XY i.border if Dist < 30000, vce(cluster border)

est store f

coefplot a b c d e f , keep(HIGHER_E) xline(0) levels( 95 ) ciopts(recast(. rcap))


***

 rdplot log_N1 effects_distance if Dist < 95000, nbins(400 400) p(1)
 
  ****************** YIELD GAP ***

  bysort border: egen border_gap = mean(norm_gap)
  
  bysort Country border: egen country_gap = mean(norm_gap)

  gen more_gap_e = 1 if country_gap < border_gap
  
  replace more_gap_e = 0 if more_gap_e ==.
  
  gen dist_mge = more_gap_e * dist
  


forvalue i = 1/289 {

tab border if border == `i'

capture noisily reg norm_gap more_gap_e dist dist_mge Vegetation if border == `i' & dist< 30000  , vce(robust)
 
capture noisily replace more_gap_e = more_gap_e + 2 if _b[more_gap_e] < 0 &  border == `i'

capture noisily replace more_gap_e = 1 if more_gap_e == 2

capture noisily replace more_gap_e = 0 if more_gap_e == 3
 
}


 drop  dist_mge
 
 gen dist_mge = more_gap_e * dist
 
 gen G_DIST = Dist
 
 replace G_DIST = Dist *(-1) if more_gap_e==1

 
 *****
 
gen  Country_More_GAP = Country if  more_gap_e ==1

replace  Country_More_GAP = Neighbor if more_gap_e ==0

gen  Country_Less_GAP = Neighbor if more_gap_e ==1

replace  Country_Less_GAP = Country if  more_gap_e ==0

gen Border_GAP = .

gen Border_GAP_SIG = .

gen Border_G_VEG = .

gen Border_G_VEG_SIG = .


 *****************************************************************************
 
forvalue i = 1/289 {

tab border if border == `i'

capture noisily reg norm_gap more_gap_e dist dist_mge Vegetation if border == `i' & dist< 30000  , vce(robust)

capture noisily replace Border_GAP = _b[more_gap_e] if  border == `i'

capture noisily replace Border_GAP_SIG = ( _b[more_gap_e] / _se[more_gap_e] ) if  border == `i'

capture noisily reg Vegetation more_gap_e dist dist_mge if border == `i' & dist< 30000  , vce(robust)

capture noisily replace Border_G_VEG = _b[more_gap_e] if  border == `i'

capture noisily replace Border_G_VEG_SIG = ( _b[more_gap_e] / _se[more_gap_e] ) if  border == `i'

}

gen gap_distance_e = dist

replace gap_distance_e = dist * (-1) if more_gap_e ==1


  ****************** WATER POLLUTION ***
  

  bysort border: egen border_N1 = mean(log_N1)
  
  bysort Country border: egen country_N1 = mean(log_N1)

  gen more_N1_e = 1 if country_N1 < border_N1
  
  replace more_N1_e = 0 if more_N1_e ==.
  
  gen dist_mN1e = more_N1_e * dist
  
 

forvalue i = 1/289 {

tab border if border == `i'

capture noisily reg log_N1 more_N1_e dist dist_mN1e Vegetation if border == `i' & dist< 30000 , vce(robust)

capture noisily replace more_N1_e = more_N1_e + 2 if _b[more_N1_e] < 0 &  border == `i'

capture noisily replace more_N1_e = 1 if more_N1_e == 2

capture noisily replace more_N1_e = 0 if more_N1_e == 3
 
 
}


 drop  dist_mN1e
 
 gen dist_mN1e = more_N1_e * dist
 
 gen N_DIST = Dist
 
 replace N_DIST = Dist *(-1) if more_N1_e==1


 *****
 

gen  Country_More_N1 = Country if  more_N1_e ==1

replace  Country_More_N1 = Neighbor if more_N1_e ==0

gen  Country_Less_N1 = Neighbor if  more_N1_e ==1

replace  Country_Less_N1 = Country if more_N1_e ==0

gen Border_N1 = .

gen Border_N1_SIG = .

gen Border_N1_VEG = .

gen Border_N1_VEG_SIG = .

*********************
 
forvalue i = 1/289 {

tab border if border == `i'

capture noisily reg log_N1 more_N1_e dist dist_mN1e Vegetation if border == `i' & dist< 30000 , vce(robust)

capture noisily replace Border_N1 = _b[more_N1_e] if  border == `i'

capture noisily replace Border_N1_SIG =  ( _b[more_N1_e] / _se[more_N1_e] ) if  border == `i'

capture noisily reg Vegetation more_N1_e dist dist_mN1e  if border == `i' & dist< 30000 , vce(robust)

capture noisily replace Border_N1_VEG = _b[more_N1_e] if  border == `i'

capture noisily replace Border_N1_VEG_SIG =  ( _b[more_N1_e] / _se[more_N1_e] ) if  border == `i'

}


gen N1_distance_e = dist

replace N1_distance_e = dist * (-1) if more_N1_e ==1


 ****************** CROPLAND POLLUTION ***
  

  bysort border: egen border_LN1 = mean(log_LN1)
  
  bysort Country border: egen country_LN1 = mean(log_LN1)

  gen more_LN1_e = 1 if country_LN1 < border_LN1
  
  replace more_LN1_e = 0 if more_LN1_e ==.
  
  gen dist_mLN1e = more_LN1_e * dist
  
 
forvalue i = 1/289 {

tab border if border == `i'

capture noisily reg log_LN1 more_LN1_e dist dist_mLN1e Vegetation if border == `i' & dist< 30000  , vce(robust)

capture noisily replace more_LN1_e = more_LN1_e + 2 if _b[more_LN1_e] < 0 &  border == `i'

capture noisily replace more_LN1_e = 1 if more_LN1_e == 2

capture noisily replace more_LN1_e = 0 if more_LN1_e == 3

}


 drop  dist_mLN1e
 
 gen dist_mLN1e = more_LN1_e * dist

 
 ***** 
 
gen  Country_More_LN1 = Country if  more_LN1_e ==1

replace  Country_More_LN1 = Neighbor if  more_LN1_e ==0

gen  Country_Less_LN1 = Neighbor if more_LN1_e ==1

replace  Country_Less_LN1 = Country if more_LN1_e ==0

gen Border_LN1 = .

gen Border_LN1_SIG = .

gen Border_LN1_VEG = .

gen Border_LN1_VEG_SIG = .

********************************************************************************

 
forvalue i = 1/289 {

tab border if border == `i'

capture noisily reg log_LN1 more_LN1_e dist dist_mLN1e Vegetation if border == `i' & dist< 30000  , vce(robust)

capture noisily replace Border_LN1 = _b[more_LN1_e] if  border == `i'

capture noisily replace Border_LN1_SIG = ( _b[more_LN1_e] / _se[more_LN1_e] )  if  border == `i'

capture noisily reg Vegetation more_LN1_e dist dist_mLN1e  if border == `i' & dist< 30000  , vce(robust)

capture noisily replace Border_LN1_VEG = _b[more_LN1_e] if  border == `i'

capture noisily replace Border_LN1_VEG_SIG = ( _b[more_LN1_e] / _se[more_LN1_e] )  if  border == `i'

}


gen LN1_distance_e = dist

replace LN1_distance_e = dist * (-1) if more_LN1_e ==1

gen LN_DIST = Dist
 
replace LN_DIST = Dist *(-1) if more_LN1_e==1


*******MAIN RESULTS*********

xi: cmp(norm_gap =  more_N1_e dist dist_mN1e X Y XY i.border)(log_N1 = more_N1_e dist dist_mN1e X Y XY  i.border) if Dist < 20000, indicator(1 1) vce(cluster border)

xi: cmp(norm_gap =  more_N1_e dist dist_mN1e Altitude P_dry MeanT Bedrock Twet Prec SOC X Y XY i.border)(log_N1 = more_N1_e dist dist_mN1e  CHICKEN CATTLE PIGS PEOPLE X Y XY i.border) if Dist < 20000, indicator(1 1) vce(cluster border)

xi: cmp(norm_gap =  more_N1_e dist dist_mN1e X Y XY i.border)(log_N1 =  more_N1_e dist dist_mN1e X Y XY  i.border) if VEG_JUMP==0 & Dist < 20000, indicator(1 1) vce(cluster border)

xi: cmp(norm_gap =  more_N1_e dist dist_mN1e Altitude P_dry MeanT Bedrock Twet Prec SOC X Y XY i.border)(log_N1 =  more_N1_e dist dist_mN1e CHICKEN CATTLE PIGS PEOPLE  X Y XY  i.border) if VEG_JUMP==0 & Dist < 20000, indicator(1 1) vce(cluster border)

xi: cmp(norm_gap =  more_N1_e dist dist_mN1e  X Y XY i.border)(log_N1 =  more_N1_e dist dist_mN1e  X Y XY i.border) if Dist < 20000 & dataquality>.25 , indicator(1 1) vce(cluster border)

xi: cmp(norm_gap =  more_N1_e dist dist_mN1e Altitude P_dry MeanT Bedrock Twet Prec SOC X Y XY i.border)(log_N1 =  more_N1_e dist dist_mN1e  CHICKEN CATTLE PIGS PEOPLE X Y XY i.border) if Dist < 20000 & dataquality>.25 , indicator(1 1) vce(cluster border)


***FOR EXPLANATIONS SEE SECOND DO FILE***