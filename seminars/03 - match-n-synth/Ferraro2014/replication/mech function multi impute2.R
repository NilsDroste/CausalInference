#Function to estimate mechanism effects

mech <- function(model=NULL, data, match=NULL, Y="outcome", Tr="treatment", covs="covariates", mech="mechanism", covs.bias=NULL, unique="id variable", se=1,
								 balance.out=TRUE, match.sum=TRUE, summary.stats=TRUE, betas=FALSE, BiasAdjust=TRUE, impute.mech=FALSE, ...){
	
	#add column of ones to data
	data$ones <- 1
	match2 <- 1
	#If match = NULL then Match will be called
	if(is.null(match)){
			#indicator for later use
			match2 <- NULL
			#covariates for matching
			covs.m <- data[covs]
			y      <- as.matrix(data[Y])
			treat  <- as.matrix(data[Tr])
	
			#handle the potential for bias-adjustment variables that differ from the matching covariates
			if(is.null(covs.bias)){
					covs.b <- covs.m
			} else {
					covs.b <- data[covs.bias]
			}#end If(is.null(cov.bias))
			
			#run matching algorithm
			match <- Match(Y=y, Tr=treat, X=covs.m, Z = covs.b, BiasAdjust=BiasAdjust, ...)
	
	}#end if(is.null(match))
		
	#Take the Match object and create the matched set
	bal <- balance(data, match, Y=Y, treatin=Tr, names=covs, matchout=TRUE, impute=TRUE)
	#store balance measures
	bal.out <- bal$bal
	#store matched set
	matched1 <- bal$match.set

	#Treated units
	matched1t <- subset(matched1, matched1[Tr] == 1)
	#Control units
	matched1c <- subset(matched1, matched1[Tr] == 0)
	
	#if the subset command fails
	if(dim(matched1t)[1] == 0){
		matched1t <- matched1[matched1[Tr] == 1,]
		matched1c <- matched1[matched1[Tr] == 0,]
	}
	
	#combine covariates and mechanism for regression imputations
	names.model <- c(covs, mech)
	
	##########################################################################################################################
	#store betas and calculate NATT and MATT
	
	#If a model is not spcified then use the matching covariates to estimated the influence of the mechanism for treated units
	
	#if there is only one mechanism
	if(length(mech) == 1){
	
			if(is.null(model)){

					cov.t <- as.matrix(matched1t[names.model])
					yt <- as.matrix(matched1t[Y])
					b1 <- as.matrix(lm(yt ~ cov.t)$coefficients)
					#standard errors
					se1 <- as.matrix(summary(lm(yt ~ cov.t))$coefficients[,c(2,4)])
					lm <- lm(yt ~ cov.t)

			} else {#is a model is given

					#estimate coefficients
					b1 <- as.matrix(lm(model, data=matched1t)$coefficients)
					#standar errors
					se1 <- as.matrix(summary(lm(model, data=matched1t))$coefficients[,c(2,4)])
					lm <- lm(model, data=matched1t)
							
					#extract names from betas for use in imputation
					c <- as.data.frame(t(b1))
					#drop intercept from names
					names.model <- names(c)[-1]
	
			}#end if else(model)

			#save the original treated unit mechanism values
			matched1t[paste(mech, "orig", sep=".")] <- matched1t[mech]
					 
			#store the mechanism outcomes of the matched controls to their respective treated units, overwriting the originals
			#so that the imputations can be made from the treated betas
			matched1t[mech] <- matched1c[mech]
			
			#if biasadjustment is used to impute counterfactual mechanism values
			if(impute.mech){
					
					#calculate coefficients from control regression of mechanism on covariates
					cov.c <- as.matrix(matched1c[covs])
					mc <- as.matrix(matched1c[mech])
					b.m <- as.matrix(lm(mc ~ cov.c)$coefficients)
					cov.t.m <- as.matrix(cbind(matched1t["ones"], matched1t[covs]))
					matched1t[mech] <- cov.t.m %*% b.m
			
			}#end if(imput.mech)
			
			#store matrix of mechanism values
			mech.mat <- as.matrix(matched1t[mech])

									
			#create matrix of treated covariates and matched control mechanism outcomes
			cov.t.m <- as.matrix(cbind(matched1t["ones"], matched1t[names.model]))
				
			#Now use the coefficients estimated above to estimate Key Counterfactual E[Y(1,S(0)]|X
			kc <- cov.t.m %*% b1 
					
			#if biasAdjust=TRUE then the nate should be calculated according to the imputed counterfactuals Yhat
			if(BiasAdjust){
					nate <- mean(kc) - mean(matched1t$yhat)

					#store the individual nate for calculation of mate variance
					nate.i <- kc - matched1t$yhat
					
					#calculate individual mate based on nate.i
					est <- matrix(match$est, nrow(kc), 1)
					mate.i <- matched1t[Y] - kc
					#if the mate.i is calculated as a non-numeric list, need to convert
					if(!is.numeric(mate.i)){
						mate.i <- as.numeric(unlist(mate.i))
					}

##########################################################################################################################
#test for dif in mate.i using dif techs
#					mate.i <- matched1t[Y] - kc
##########################################################################################################################					
					
			} else {#if not, should use the actual outcomes of matched controls as the counterfactuals
					#similar to code from the BasAdjust section
					nate.i <- kc - matched1c[Y]
					est <- matrix(match$est, nrow(kc), 1)
					mate.i <- matched1t[Y] - kc
					if(!is.numeric(matched1c[Y])){
						nate.i <- as.numeric(unlist(nate.i))
						mate.i <- as.numeric(unlist(mate.i))
						} 
				}#end else				

			nate <- mean(nate.i)
			#need this calculation when not using bias adjustment (works with bias adjustment too)
			mate <- match$est - nate	
			
##########################################################################################################################
#mate <- mean(mate.i)
##########################################################################################################################
		#standard errors for the mate and nate		
		if(se==0){
				#call function to calculate standard (not heteroskedasticity robust) ses (condidtion variance = (Yti - NATEi - MATT)^2)
				se.nh <- se.badj(data=data, matched=matched1, treat=Tr, y=Y, names=covs, unique=unique, yhatbadj.in=nate.i, ate.in=mate, out=F)$se
		}else{
				#call function to calculate heteroscdacticity robust ses
				se.h.m <- cond.gen.m(data=matched1, data.orig=data, names=covs, div=2, y=Y, treat=Tr, unique=unique, old=FALSE, est=mate, var=1, mate.i=mate.i, nate.i.in=kc)$se.m
				se.h.n <- cond.gen.m(data=matched1, data.orig=data, names=covs, div=2, y=Y, treat=Tr, unique=unique, old=FALSE, est=mate, var=1, mate.i=mate.i, nate.i.in=kc)$se.n
		}#end if se

	}#end if(length(mech) == 1) 
			

	##########################################################################################################################
	#if there is more than one mechanism
	if(length(mech) > 1){

			if(is.null(model)){

					cov.t <- as.matrix(matched1t[names.model])
					yt <- as.matrix(matched1t[Y])
					b1 <- as.matrix(lm(yt ~ cov.t)$coefficients)
					#standard errors
					se1 <- as.matrix(summary(lm(yt ~ cov.t))$coefficients[,c(2,4)])
					lm <- lm(yt ~ cov.t)

			} else {#is a model is given

					#estimate coefficients
					b1 <- as.matrix(lm(model, data=matched1t)$coefficients)
					lm <- lm(model, data=matched1t)
					#standar errors
					se1 <- as.matrix(summary(lm(model, data=matched1t))$coefficients[,c(2,4)])
							
					#extract names from betas for use in imputation
					c <- as.data.frame(t(b1))
					#drop intercept from names
					names.model <- names(c)[-1]
	
			}#end if else(model)
			
			#save the original treated unit mechanism values and the matched control mechanism values
			for(i in 1:length(mech)){
	
					#for each of the mechanisms in mech
					matched1t[paste(mech[i], "orig", sep=".")] <- matched1t[mech[i]]
				 	
				 	#store the mechanism outcomes of the matched controls to their respective treated units, overwriting the originals
					#so that the imputations can be made from the treated betas
					
					if(impute.mech == FALSE){

							matched1t[mech[i]] <- matched1c[mech[i]]
					
					}#end if
					
					#if biasadjustment is used to impute counterfactual mechanism values
					if(impute.mech){

							#calculate coefficients from control regression of mechanism on covariates
							cov.c <- as.matrix(matched1c[covs])
							mc <- as.matrix(matched1c[mech[i]])
							b.m <- as.matrix(lm(mc ~ cov.c)$coefficients)
							cov.t.m <- as.matrix(cbind(matched1t["ones"], matched1t[covs]))
							matched1t[mech[i]] <- cov.t.m %*% b.m
					
					}#end if(imput.mech
					
			}#end for
			
			#stor a matrix of counterfactual mechanism values. Note that if mech.impute=T then these are imputed using bias adjustment 
			#if mech.impute=F then these are the counterfactual values based on observed control mechanisms 
			mech.mat <- matched1t[mech]
			
			#create vector to hold counterfactuals
			kc <- vector()
			#create matrix to hold individual counterfactuals ADD A COLUMN TO ACCOMODATE THE TOTAL MECHANISM EFFECT
			kc.i <- matrix(0, nrow(matched1t), length(mech)+1)

			#first estimate the total net treatment effects
			#create matrix of treated covariates and matched control mechanism outcomes
			cov.t.m <- as.matrix(cbind(matched1t["ones"], matched1t[names.model]))
			
			#Now use the coefficients estimated above to estimate E[Y(1,S(0)]|X, i.e., if none of the mechanisms were active
			kc[1] <- mean(cov.t.m %*% b1)
			#store individual counterfactuals
			kc.i[,1] <- cov.t.m %*% b1

			#estimate the counterfactual of interest moving stepwise through the mechanisms
			for(i in 1:length(mech)){

					#set the complimentary mechanisms to their original values
					matched1t[mech[-i]] <- matched1t[paste(mech[-i], "orig", sep=".")]
					
					#create matrix of treated covariates and matched control mechanism outcomes
					cov.t.m <- as.matrix(cbind(matched1t["ones"], matched1t[names.model]))
		
					#estimate counterfactual of interest which is E[Y(1...)] setting the mechanism of interest to that of the matched control
				 	kc[i + 1] <- mean(cov.t.m %*% b1)
				 	#estimate and store individual counterfactuals of interest
				 	kc.i[,i + 1] <- cov.t.m %*% b1

				 	#reset the mechanisms to that of the controls
				 	matched1t[mech] <- mech.mat[mech] 
				 	
			}#end for

			#if biasAdjust=TRUE then the nate should be calculated according to the imputed counterfactuals Yhat
			if(BiasAdjust){
					#store the vector of NATEs
					nate <- kc - mean(matched1t$yhat)
			} else {#if not, should use the actual outcomes of matched controls as the counterfactuals
					#store the vector of NATEs
					nate <- kc - mean(matched1c[Y])
			}#end else				
			
			#estmate MATE
			mate <- match$est - nate
			
			#generate matrix for individual nate calculations
			nate.i <- matrix(0, nrow(kc.i), ncol(kc.i))
			#generate vector for standard errors
			se.h.m <- vector()
			se.h.n <- vector()

			if(se == 0){
					#calculate standard standard errors for each mechanim
		   		for(i in 1:(length(mech)+1)){
		   				nate.i[,i] <- kc.i[,i] - matched1t$yhat 
							nate.in <- as.data.frame(nate.i[,i])
							mate.in <- mate[i]
							kc.i.in <- kc.i[,i] 
		   				#call function to calculate ses
		   				se.nh[i] <- se.badj(data=data, matched=matched1, treat=Tr, y=Y, names=covs, unique=unique, yhatbadj.in=kc.i.in, ate.in=mate.in, out=F)$se
					}#end for
			}else{
					#calculate robust se s for each MECHANISM
					for(i in 1:(length(mech)+1)){
		   				nate.i[,i] <- kc.i[,i] - matched1t$yhat
		   				nate.i.in <- as.data.frame(nate.i[,i]) 
							mate.in <- mate[i]
							mate.i.in <- (match$est) - as.data.frame(nate.i[,i])
							kc.i.in <- kc.i[,i] 
		   				#call function to calculate ses
							se.h.m[i] <- cond.gen.m(data=matched1, data.orig=data, names=covs, div=2, y=Y, treat=Tr, unique=unique, old=FALSE, est=mate.in, var=1, mate.i=mate.i.in, nate.i.in=kc.i.in)$se.m
							se.h.n[i] <- cond.gen.m(data=matched1, data.orig=data, names=covs, div=2, y=Y, treat=Tr, unique=unique, old=FALSE, est=mate.in, var=1, mate.i=mate.i.in, nate.i.in=kc.i.in)$se.n
					}#end for
			}#end if se

	}#end if
	
	#store SEs for output
  if(se==0){
			se <- se.nh
			se.type <- "Standard"
	}else{
			se.m <- se.h.m
			se.n <- se.h.n
			se.type <- "Robust"
	}
	
	#display summary stats for mechanisms if called for
	if(summary.stats){
			mech.mat <- as.matrix(matched1t[mech])
			cat("             Description on Mechanism by Treatment Arm", "\n")
			cat("Treated Units", "\n")
			print(describe(matched1t[paste(mech, "orig", sep=".")]))
			cat("\n", "Control Units", "\n")
			print(describe(mech.mat))
			print(t.test(matched1t[paste(mech, "orig", sep=".")], mech.mat))
			cat("Effect of treatment in Mechanism = ", t.test(matched1t[paste(mech, "orig", sep=".")], mech.mat)$est[1] - t.test(matched1t[paste(mech, "orig", sep=".")], mech.mat)$est[2], "\n")
	}#end if(summary.stats)

	#display results if called for
	if(is.null(match2)){
			
			if(match.sum){
					cat("\n", "ESTIMATES FROM MATCH", "\n")
					summary(match)
			}#end if(match.sum)
			
			#display balance measures if called for
			if(balance.out){
					cat("                            Balance Results From Matching", "\n")
					print(bal.out)
					cat("\n")
			}#end if(balance.out)

	}#end if(is.null(match))
	
	#print betas from treatment-treatment regression
	if(betas){
			cat("\n")
			cat("Coefficients from post-match treatment group", "\n")
			cat("regression of specified covariates on outcome", "\n")
			b <- data.frame(b1, se1)
			names(b)[1] <- "Coefficients"
			names(b)[2] <- "SEs"
			names(b)[3] <- "p-value"
			print(b)
			cat("\n")
	}#end if(betas)	
	
	if(length(mech) == 1){
			cat("      Results          ", "\n")
			cat("ATT..........", match$est, "\n")
			cat("NATT.........", nate, "\n")
			cat("NATT.SE......", se.n, "\n")
			cat("MATT.........", mate, "\n")
			cat("MATT.SE......", se.m, "\n")
	} else {
			cat("      Results          ", "\n")
			cat("ATT...............", match$est, "\n")
			cat("Total NATT........", nate[1], "\n")
			cat("SE Total NATT.....", se.n[1], "\n")
			cat("Total MATT........", mate[1], "\n")
			cat("SE Total MATT.....", se.m[1], "\n")
			cat("\n")
			
			#create following periods for each mech name
			NATT.out <- vector()
			MATT.out <- vector()
			SE.m.out <- vector()
			SE.n.out <- vector()
	 
			for(i in 1:length(mech)){
					j= 20 - nchar(mech[i])
					NATT.out[i] <- paste("NATT", noquote(mech[i]), sep=".")
					SE.n.out[i]   <- paste("SE.N", noquote(mech[i]), sep=".")
					MATT.out[i] <- paste("MATT", noquote(mech[i]), sep=".")
					SE.m.out[i]   <- paste("SE.M", noquote(mech[i]), sep=".")
					
					while(j > 1){
							NATT.out[i] <- paste(NATT.out[i], ".", sep="")
							MATT.out[i] <- paste(MATT.out[i], ".", sep="")
							SE.m.out[i] <- paste(SE.m.out[i], ".", sep="")
							SE.n.out[i] <- paste(SE.n.out[i], ".", sep="")
							j = j - 1
					}#end while
			}#end for	
			
			for(i in 1:length(mech)){
					cat(NATT.out[i], nate[i+1], "\n")
					cat(SE.n.out[i], se.n[i+1], "\n")
					cat(MATT.out[i], mate[i+1], "\n")
					cat(SE.m.out[i], se.m[i+1], "\n")
					cat("\n")
			}#end for
	}#end else
	
	#Results
	out <- list(se.m=se.m, se.n=se.n, se.type=se.type, nate=nate, mate=mate, coeffs=b1, lm=lm, treated=matched1t, control=matched1c, mechs=mech.mat, matt=mate.i, natt=nate.i)
	return(out)
	
}

##########################################################################################################################
#function to calculate robust standard errors
cond.gen.m <- function(data, data.orig=NULL, names, div=2, y, treat, unique, old=FALSE, est=NULL, var=0, mate.i, nate.i.in) {
   #column of ones in original data for weights
   data$ones <- 1
   
   #duplicates from control group
   data$dups <- duplicated(data[unique])
   matched2 <- subset(data, data$dups == FALSE)
   Tr <- as.matrix(matched2[treat])
	 Y <- as.matrix(matched2[y])
	 X <- as.matrix(matched2[names])
	 id <- as.matrix(matched2[unique])
		 
	 Nx <- nrow(X)
   Kx <- ncol(X)	
	 
	 #many of the calculations call for a weighting vecto...all my weights are one
	 weight <- matrix(1, Nx, 1)

	 N <- nrow(X)
  
   #weight matrix for mahalanobis distance NOTE THAT THIS IS SLIGHTLY DIFFERENT FROM THE MATCHING FORM B/C THE WEIGHTING
   #ONLY BASED ON THE OBSERVATIONS IN THE SAMPLE (HERE) WHEREAS IT IS BASED ON THE ENTIRE DATA SAMPLE IN THE MATCH FUNCTION

	if(det(cov(X)) != 0) {
		icov <- solve(cov(X))
		}
	if(det(cov(X)) == 0) {
		icov <- ginv(cov(X))
		}

		Mu.X  <- matrix(0, Kx, 1)
		Sig.X <- matrix(0, Kx, 1)
		X.s <- as.matrix(X) 
		eps <- vector()
		X.n <- as.matrix(X)

		
		#Transform the sample data
		for (k in 1:Kx){
				Mu.X[k,1] <- sum(X.s[,k]*weight[k])/sum(weight)
				eps <- X.s[,k]-Mu.X[k,1]
				Sig.X[k,1] <- sqrt(sum(X.s[,k]*X.s[,k]*weight[k])/sum(weight)-Mu.X[k,1]^2)
				Sig.X[k,1] <- Sig.X[k,1]*sqrt(N/(N-1))
				X.n[,k]=eps/Sig.X[k,1]
		} #end of k loop

		#if there is no original data specified then the weighting matrix will be derived from the sample
		if(is.null(data.orig)){
				weight.mat <-solve(t(X.n)%*%X.n/N)   
		 		ww <- chol(weight.mat)
		}else{

				X.o <- as.matrix(data.orig[names])
				N.o <- nrow(data.orig)
		    data.orig$ones <- 1
				weight.o <- data.orig$ones
				X.no <- as.matrix(data.orig[names])
				
				
				#Transform the original data to calculate the weight matrix
				for (k in 1:Kx){
						Mu.X[k,1] <- sum(X.o[,k]*weight.o[k])/sum(weight.o)
						eps <- X.o[,k]-Mu.X[k,1]
						Sig.X[k,1] <- sqrt(sum(X.o[,k]*X.o[,k]*weight.o[k])/sum(weight.o)-Mu.X[k,1]^2)
						Sig.X[k,1] <- Sig.X[k,1]*sqrt(N.o/(N.o-1))
						X.no[,k]=eps/Sig.X[k,1]
				} #end of k loop
				
				weight.mat <-solve(t(X.no)%*%X.no/N.o)   
				ww <- chol(weight.mat)
		}#end if(is.null(data.orig))

 
   #dim variables for loop
   Dist.m <- matrix(0, Nx, Nx)
   sigma <- vector()
   Ydif <- vector()
   M <- vector()
   MM <- vector()
   MMM <- vector()
   sig.id <- vector()
   for (i in 1:N) {
   
            # treatment indicator observation to be matched
            TREATi <- Tr[i]
            # covariate value for observation to be matched
            xx <- X.n[i,]
            
            if(old){
            		xx <- X[i,]
            }
            
            # potential matches are all observations with the same treatment value
            #this assigns a logical T,F to all observations based on whether or not they share the same original treatment with i
            POTMAT <- (Tr==TREATi)
            #changes i to 0 and chages all TRUE=1 and FALSE=0 so that i now has opposite Tr compared to original group
            POTMAT[i] <- 0
            weightPOT <- as.matrix(POTMAT)
            
            #this calculates the covariate distance for all j!=i (for i this is all 0's)
             DX <- (X.n - matrix(1, N,1) %*% xx) %*% t(ww)
						if(old){
		            DX <- (((X - matrix(1, nrow(X), 1) %*% xx) %*% icov))
						}		
							
            if (Kx>1)
              {
                #create vector of mahalanobs distances
                foo <- apply(t(DX*DX), 2, sum)
                Dist <- as.matrix(foo)
								if(old){
                		Dist <- as.matrix(rowSums(DX * (X - matrix(1, nrow(X), 1) %*% xx)))
                }
              } else {
                Dist <- DX*DX
              }

						Dist.m[,i] <- as.matrix(Dist)

            # distance to observation to be matched

            # Distance vector only for potential matches
            DistPot <- Dist[POTMAT==1,1]
            # sorted distance of potential matches
            S <- sort(DistPot)
           
            #calculate the minimum distance, based on covariates, between to observations of the same original treatment
            Distmin=S[1]
            # returns logical vector all FALSE except for the observation that is the closest Match
            ACTMAT <- (POTMAT==1) & (Dist== Distmin)
            #stores that outcomes from the unit being matched and the matched unit Y[ACTMAT] chooses observation where ACTMAT==TRUE
            Yactmat <- as.matrix(c(Y[i], Y[ACTMAT]))

            #take the difference between Yi and its closest match
            Ydif[i] <- Yactmat[1] - Yactmat[2]
            sigma[i] <- (Ydif[i]^2)/div 
						sig.id[i] <- id[i] 						 
         }# end of i loop
		sigma.id <- data.frame(sig.id, sigma)

		#calculate and store weights (the number of times each control was used as a match) for the control units
		data2 <- calcBy(x="ones", data=data, by=unique, FUN="sum", sort=FALSE, collapse=TRUE)
		
		data2 <- data.frame(data2$ones.sum, data2[treat], data2[y], data2[unique])

		  
		#store the weights
		km <- merge(data2, sigma.id, by.x=unique, by.y="sig.id")


		#square the weights
		n <- nrow(km)
		kms <- vector()
		
	
		#this creates the denomonator based on number of treated and control observations
		div0 <- 0
		div1 <- 0
		
		for(i in 1:n){
			if(km[i, 3] == 1) {
				kms[i] <- km[i, 2]^2 / (nrow(data)/2)^2
			} else{
				kms[i] <- km[i, 2]^2 / (nrow(data)/2)^2
			}
		}
	
		#equation (11) from AI 2004
		kms2 <- (kms * km$sigma)
		var <- sum(kms2)
		se <- sqrt(var)
	cat("var.samp", var, "\n")		
		#if population variance is to be estimated
		if(var != 0){
				
				#Just the control weights for the pop.var
				Kcountc <- ifelse(km[,3] == 0, km[,2], 0)
				var.pop=sum((km$sigma*(Kcountc*Kcountc-Kcountc))/(nrow(data)/2)^2)
						
				#calculate portion of the population variance that accounts for the estimated treatment effects
				#note for the mechanism estimation i use the deviation to the estimated individual mate
				dvar.pop <- sum((mate.i-array(est, length(mate.i)))*(mate.i-array(est, length(mate.i))))/(nrow(data)/2)^2
#print(mate.i-array(est, length(mate.i)))
#print(mean(mate.i))
#print(mate.i)
#print(mate.i-(est))

				datat <- subset(data, data[treat] == 1)
				datac <- subset(data, data[treat] == 0)
				#sometimes the subset() function fails
#				if(dim(datat)[1]==0){
					datat <- data[data[treat] == 1,]
					datac <- data[data[treat] == 0,]
#				}
				#calculate the nate for variance
				nate.i <- nate.i.in - datat$yhat
				if(!is.numeric(nate.i)){
					nate.i <- as.numeric(unlist(nate.i))
				}

				#for the matt
				dvar.pop.m <- sum((datat[y] - nate.i.in - matrix(est, nrow(datat), 1))^2)/(nrow(data)/2)^2
				var.pop.m <- var.pop + dvar.pop.m
				se.m <- sqrt(var.pop.m)

				#FOR ESTIMATING THE VARIANCE OF THE NATT
				dvar.pop.n <- sum((nate.i - mean(nate.i))^2)/(nrow(data)/2)^2
  			var.pop.n <- var.pop + dvar.pop.n
				se.n <- sqrt(var.pop.n)
		}#end if 
		out <- list(se.m=se.m, se.n=se.n, kms2= kms2, del=weight.mat, ww=ww, sigs=sigma.id$sigma)#, km=km)
		
		
		return(out)
 } #end function  
 
 ##########################################################################################################################  
#function to calculate the standard errors
se.badj <- function(data, matched, treat, y, names, unique, yhatbadj.in=NULL, ate.in=NULL, out=TRUE) { 
		#get observation number from matched data set for creating control weights later 
		uni <- as.data.frame(matched[unique])
		
		#assign treatment data frame and order ny observation number so that it will allign with the covariate data
		tr <- as.data.frame(matched[treat])
		
		#add ones to the data
		matched$ones <- 1
		matchedt <- subset(matched, matched[treat] == 1)
		matchedc <- subset(matched, matched[treat] == 0)
		
		#if the subset command fails
		if(dim(matchedt)[1] == 0){
			matchedt <- matched[matched[treat] == 1,]
			matchedc <- matched[matched[treat] == 0,]

		}		
		
		#subset the Z and Y data by treatment
		Zt <- matchedt[c("ones", names)]
		Zc <- matchedc[c("ones", names)]

		ytt <- matchedt[y]
		ycc <- matchedc[y]
		
							 
		#convert outcome and covariate (with column of ones) data frames to matrices
		xt <- as.matrix(Zt)
		xc <- as.matrix(Zc)
		
		yt <- as.matrix(ytt)
		yc <- as.matrix(ycc)
		
		#transpose of xc
		txc <- t(xc)

		#estimate coefficients for regression of controls units
		b.c <- ginv(txc %*% xc) %*% txc %*% yc

		#calc residuals
		ec <- yc - xc %*% b.c
		
		#calculate the fits for control covariates using control coefficients
		yhatCC <- xc %*% b.c

		#calculate fits for treated covariates using control coefficients, note that this should work as the counterfactual
		yhatTC <- xt %*% b.c 

		#calcualate the AI bia-adjusted counterfactual
		yhatbadj <- yc + yhatTC - yhatCC
		
		#calculate variances
		#generate column of average treatment effect
		te <- seq(1: nrow(xt))
		te <- data.frame(te)
		te$te <- mean(yt - yhatbadj) #note that the variance depends on the treatment effect estimate
		
		#generate column of ones for the matrix computation later
		te$ones <- 1
		ate <- as.matrix(te$te)
		ones <- as.matrix(te$ones)
		
		#these are from equation (16)(second) from AI 2004
		#genrate dif from TEi and ATE for treated obs
		vct <- (yt - yhatbadj - ate)^2
		
		if(!is.null(yhatbadj.in)){
				#use inputs from function
				vct <- (yt - yhatbadj.in - matrix(ate.in, nrow(yt), 1))^2
		}#end if(!is.null(yhatbadj)
		
		#generate sigma for all obs
		sigmas <- (sum(vct)/(nrow(xt)*2))
		
	 	names(uni)[1] <- "uniq"
  	uni$ones <- 1
  	
   	dups <- summaryBy(ones ~ uniq, data=uni, 
		  								FUN = function(x) { c(m = sum(x)) } )
		#merge the count data (how many times a control was used)
		#note that by default merge does not keep observations that are not contained in both objects so the
		#resultin object (matchnodups) will have the same number of observations as dups from above
		#matchnodups <- merge(matchall, dups, by="codseg73", sort=FALSE)
		
		#store the 'weights' for the control observations (this is the number of times that a given control is used as a match)
		km <- dups$ones.m
		
		#equation (11) from AI 2004
		kms2 <- ((km)^2 * sigmas)
		var <- sum(kms2) / (nrow(xt)^2)
		
		#Bias-adjusted ATT
		ATT <- mean(yt - yhatbadj)
		#Abadie Imbens SE
		SE <- sqrt(var)
		#Tstat
		T <- mean(yt-yhatbadj) / sqrt(var)
		if(out){
				cat("ATT........",ATT,"\n")
				cat("SE.........",SE,"\n")
				cat("Tstat......",T,"\n")
		}		
		out <- list(att=ATT, se=SE, t=T, yhatt=yhatTC)
		return(out)
}
