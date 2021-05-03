//Set Directories
global main "D:\" //set main directory for replication

//Build Data
run "$main\code\clean_popdata.do" //cleans population data
run "$main\code\clean_stbudget.do" //cleans state expenditure data
run "$main\code\clean_policy.do" //cleans policy timing data
run "$main\code\clean_corrupt.do" //cleans corruption data
run "$main\code\build_npdes_2.do" //cleans NPDES compliance data

//Combine Data
run "$main\code\combine_data.do" //combines data for analysis
