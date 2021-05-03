#Function: countif

#####################################################################################
# 																																									#
# Function evaluates a logical expression and returns the number of elements that   #
# meet and do not meet the condition. Convenient for complex expressions. Function  #
# also will return a data frame with the elements of the original data that meet    #
# the specified condition.                                                          #
#                                                                                   #
# Arguments:                                                                        #
#			exp  = Logical expression to be evaluated. Can be specified as string (most   #
#						 convenient) or in the form data$column.                                #
#                                                                                   #
#			data = Data on which exp will be evaluated. If data=NULL then the most        #
#						 recent attached (see attach()) data will be used to evaluate exp.      #
#                                                                                   #
#			store= Logical, indicate whether or not elements of data that meet condition  #
#						 exp should be stored in a new object.	                                #
#                                                                                   #
#####################################################################################

countif <- function(exp=expression, data=NULL, store=FALSE){
	
	#if no data are specified then use the attached data
	if(is.null(data)){
		cat("Data was not specified, so using data that was attached last", "\n")
		s <- search()
		data <- get(s[2])
	}#end if(is.null(data))
	
	#evaluate depending on the type of input for "exp"
	if(typeof(exp) == "character"){
		temp <- subset(data, eval(parse(text=exp)))
	} else {
		storage.mode(exp) <- "logical"
		temp <- subset(data, exp)
	}#end if/else(typeof)
	
	#print results
	cat("Count  True = ", nrow(temp), "\n")
	cat("Count False = ", nrow(data) - nrow(temp), "\n")
	
	#if called, store data that meets the condition "exp" for return
	if(store){
		cond.data <- as.data.frame(temp)
		return(cond.data)
	}#end if(store)
}
