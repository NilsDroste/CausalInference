
use "Explanations.dta" 


regress TO_LAND zgdp zgdp2 wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store a

regress TO_LAND zeconomic_growth zeconomic_growth2 wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store b

regress TO_LAND zpopulation wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store c

regress TO_LAND zpopulation_growth wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store d

regress TO_LAND zc_kun_wiqreco_full2010  wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store e
 
regress TO_LAND zRisk_Pref wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store f

regress TO_LAND zTime_Pref wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store g

regress TO_LAND zdevelopmentflow_landresources  wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store h

regress TO_LAND zadjusted_savings wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store i

regress TO_LAND zAgriculturevalueaddedconsta wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)
 
est store j

regress TO_LAND zyr_sch2010 wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store k

regress TO_LAND va_share_a wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store l

regress TO_LAND znpk_fert  wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store m

regress TO_LAND zAvailability wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store n

regress TO_LAND zAffordability wb_ssa wb_mena wb_eca wb_sas wb_eap wb_nam wb_lac, vce(robust)

est store o

coefplot  a b c d e f g h i j k l m n o  , keep( zgdp zgdp2  zeconomic_growth zeconomic_growth2 zpopulation zpopulation_growth zc_kun_wiqreco_full2010 zRisk_Pref zTime_Pref zdevelopmentflow_landresources zadjusted_savings zyr_sch2010 va_share_a znpk_fert zAvailability zAffordability) xline(0) levels(  95  ) ciopts(recast(. rcap)) legend(off) 


