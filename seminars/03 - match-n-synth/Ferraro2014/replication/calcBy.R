#function that emulates bysort from stata: Updated 02/26/2011

################################################################################################################
#	                                             ARGUMENTS                                                       #
#	                                                                                                             #
#	       x = name of variable (as string) for which calculations will be made                                  #
#	    data = data frame in which the variables of interest lie                                                 #
#	      by = name of variable (as string) delineating the groups over which the calculations will be made      #
#	     FUN = name of function (as string) for the calculation                                                  #
#	    sort = should the data be returned sorted according to the by variable?                                  #
#	 collapse = should the duplicate observations (according to the by variable) by dropped?                     #
#	                                                                                                             #
#  The function is designed (similar to bysort in Stata) to make calculations according to groups within the   #
#	 data.                                                                                                       #
#  If you store the function in an object with the same name as the original data a new column will be added   #
#	 to the data with the function name appended to the original variable name.                                  #
#	                                                                                                             #
#	 if you store the function in a new object name, a new data From will be created with the new column.        #
#                                                                                                              #
################################################################################################################




################################################################################################################                                                                                               
                                                                                                               
calcBy <- function(x="varname.string", data=NULL, calc.data=NULL, by="by.varname.string", FUN="sum", sort=FALSE, collapse=FALSE,...){
	
	#first call the aggregate function
	
	if(is.null(calc.data)){
				agg.data <- aggregate(x=data[x], by=list(list = data[,by]), FUN=FUN, ...)  #note that the comma in "data[,by]" b/c object is list
	}
			
	if(!is.null(calc.data)){#then use the calc.data to calculate FUN for the groups
				agg.data <- aggregate(x=calc.data[x], by=list(list = calc.data[,by]), FUN=FUN, ...)  #note that the comma in "data[,by]" b/c object is list
	}

	#change names for merge
	names(agg.data) <- c(by, paste(x, FUN, sep="."))
	
	#merge data back into main dataset
	data <- merge(data, agg.data, by=by, all.x=T)
	
	#remove the duplicate observations
	if(collapse){
    data$dup <- duplicated(data[by])
    data <- subset(data, dup ==FALSE)
    data$dup <- NULL
	}#end if
	
	#sort data
	if(sort){
		data <- data[order(data[by]),]
	}#end if
	
	return(data)
}#end function
	
	