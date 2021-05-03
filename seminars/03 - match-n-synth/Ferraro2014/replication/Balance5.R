#__________________________________________________________________________
# Merlin M. Hanauer 8/23/2009, Updated 4/2014
# balance function returns pre- and post-match balance information, in     
# tabular and graphical form (optional), and the full post-match data set 
# of matched pairs (optional), after Sekhon's Match function has been run                                      
# input:    
#
#balance(dataAA, matchZ, Y=NULL, treatin="String", names=NULL, matchout=FALSE, 
#										fromMatch=TRUE, dataM=dataAA, impute=FALSE, prec=3, ltx=FALSE, ltx.title=NULL,
#										d.plot=FALSE, bw=1, plot.all=FALSE, plot.covs=NULL, leg=TRUE)
#
#                                                               
# 	dataAA-		the original dataset or covariate matrix, used to establish  
#							pre-match balance. NOTE that if you want simple balance      
#							measures based on the covariate matrix you used in Match,    
#							then dataAA should use the same matrix as used in Match,     
#							without using names. If the matched set is requested in this 
#							specification, then the covariates and treatment and Y's will 
#							be returned. By setting dataAA equal to your original dataset
#							, specifying column names and setting matchout=TRUE, the     
#							matched dataset will include all the variables from the       
#							original dataset.                                            
# 	matchZ-		the object returned by Match                                 
#		treatin- 	the treatment indicator for dataset in which Match was       
#							performed, should be a string                                
#		names- 		a vector of names (optional). this should be used if you     
#							would like to show balance information for covariates        
#							that were not included as covariates in Match                
#		matchout- if set to TRUE, balance will return the post match set of    
#							matched pairs from dataAA                                    
#		fromMatch-indicates if matchZ is coming from Match, if just comparing  
#							2 established data sets, set to FALSE                        
#		dataM- 		this should only be used if you have an expanded data set    
#							that includes all observations from an original              
#					 		matching specification (in addition to the original data     
#							set) **used for very specific case, should be=NULL           
#   impute-   default is FALSE, tells the function to impute outcomes for  
#							treated units according to the bias-adjustment matching      
#							estimator                                                    
#   prec-     number of decimal places in the balance table
#		ltx-      logical indicating if a .TEX file for the table should be 
#							created
#		ltx.title-title and file name for latex file
#		d.plot-   logical indicating if empirical density plots for pre- 
#							and post-matching balance should be provided
#		bw-       bandwidth for empirical density plots
#		plot.covs-vector of covariate names (string) for plotting, if empty
#							all covariates will be plotted
#		leg-      logical, should legend be added to plots?
# the names vector should be designated prior to calling balance, i.e.,    
#			names <- cbind("name_1", "name_2,..., "name_N")                      
# where name_n is equal to the column name of interest from the original   
# data set                                                                 	
#__________________________________________________________________________

#Example:
#
# original dataset is foo; foo$treatment in treatment indicator; foo$Y is outcome variable  
#
# covariates for matching
# cov.names <- c("cov1", cov2", "cov3", "cov4")
# covs <- foo[cov.names]
#
#
# mahalanobis covariate matching 
# mal1 <- MatchZ(Y=foo$Y , Tr= foo$treatment, X= covs,  Weight=2, BiasAdjust=TRUE, Var.calc=1)
#
# calling balance simply using the cov matrix
# balance(cov, match1, treat)
# (note that this is similar, and shorthand for assigning all arguments in the balance function:
# balance(dataAA=cov, matchZ=match1, treat=treat1)
#
# calling balance using names and original dataset (allows for balance measures on covariates
# not used in matching analysis:
# 
# names <- c("clpovindex.73", "for.pct.60", "luc.pct.123", "luc.pct.4", "luc.pct.567", "dist.mcty", "rv1973", "pct.agwork.73"
#						, "pct.charcwood.73")
#
# balance(costa1, match1, treat1, names)
# this is shorthand for:
# balance(dataAA=costa1, matchZ=match1, treat=treat1, names=names)
#
# if you choose to return the matched set the output from balance is returned in a list() and therefore should be
# stored in an object (otherwise the balance measures and the matched set will be listed in the command window)
#
# balance.out <- balance(costa1, match1, treat1, names, matchout=TRUE)
#
# to list the balance measures:
# balance.out$bal
#
# to store the matched set as its own object:
# matches <- as.data.frame(balance.out$match.set)



balance <- function(dataAA, matchZ, Y=NULL, treatin="String", names=NULL, matchout=FALSE, 
										fromMatch=TRUE, dataM=dataAA, impute=FALSE, prec=3, ltx=FALSE, ltx.title=NULL,
										d.plot=FALSE, bw=1, plot.all=FALSE, plot.covs=NULL, plot.rows=2, leg=TRUE) {
		#create observation numbers for each data set so that a matched set can be created
		dataAA$obs <- seq(nrow(dataAA))
		dataM$obs <- seq(nrow(dataM))
		
		#condition if names are not used, only balance on match covariates will be returned
		if (is.null(names)) {		
								
				#assign unmatched data 
				dataA <- as.data.frame(c(dataAA, treatin))
					
				#assign treatment column
				treat <- dataAA[treatin] 
				
				#create matched data set
				matchset <- as.data.frame(matchZ$mdata$X) 
				tr <- as.data.frame(matchZ$mdata$Tr)
				nvars <- ncol(matchset)
				
				#create subsets of matched and unmatched data so as to calc sd
				matcht <- subset(matchset, tr == 1)
				matchc <- subset(matchset, tr == 0)
				
				tdataA <- subset(dataA, treat == 1)
				cdataA <- subset(dataA, treat == 0)
				
				#if the subset command fails
				if(dim(tdataA)[1] == 0){
					tdataA <- dataA[dataA[treatin] == 1,]
					cdataA <- dataA[dataA[treatin] == 0,]
				}

				
				#this portion is just to create a matched set
				#get observation number from matched data set for creating control weights later 
				ti <- as.data.frame(matchZ$index.treated) 
				names(ti)[1] <- "obs"
				tc <- as.data.frame(matchZ$index.control) 
				names(tc)[1] <- "obs"
				obsa <- rbind(ti, tc)
				obsa <- as.data.frame(obsa)
				
				matchall <- merge(obsa, dataA, by="obs", all.x="TRUE", sort=FALSE)
		
      #if names are used then balance measures will be returned for all variables in "names"		
		}	
		if(fromMatch==TRUE) {  #note that the end if bracket and the else must be on the same line
				
				#get observation number from matched data set for creating control weights later 
				ti <- as.data.frame(matchZ$index.treated)
				#add a sequence that will identify which control observations were matched to which treated observations after the merge
				ti$match.id <- seq(nrow(ti)) 
				names(ti)[1] <- "obs"
				
				tc <- as.data.frame(matchZ$index.control) 
				#add a sequence that will identify which control observations were matched to which treated observations after the merge
				tc$match.id <- seq(nrow(tc)) 
				names(tc)[1] <- "obs"

				obsa <- as.data.frame(rbind(ti, tc))
			
				#assign treatment data frame and order NY observation number so that it will align with the covariate data
				tr <- as.data.frame(matchZ$mdata$Tr)
						
				#this will drop all observations from the original data set that are not a part of the matched data set
				matchall <- merge(obsa, dataM, by="obs", all.x=TRUE, sort=FALSE)
				
				#sort according to match.id so that the stacked data correspond to actual matches (i.e., treated observation 1 is matched with control observation 1 etc)
				matchall <- matchall[order(-matchall[treatin], matchall$match.id),]
						
				#create matchset
				matchset <- matchall[names]
				
				#matchsets by treatment
				matchsetat <- subset(matchall, tr == 1)
				matchsetac <- subset(matchall, tr == 0)
				matcht <- matchsetat[names]
				matchc <- matchsetac[names]
					
				#create unmatched set
				dataA <- dataAA[c(names, treatin)]
				
				#assign treatment column
				treat <- dataAA[treatin] 
				
				#create unmatched set by treatment
				tdataA <- subset(dataA, treat == 1)
				cdataA <- subset(dataA, treat == 0)
				
					#if the subset command fails
				if(dim(tdataA)[1] == 0){
					tdataA <- dataA[dataA[treatin] == 1,]
					cdataA <- dataA[dataA[treatin] == 0,]
				}

				
				nvars <- ncol(dataA)
		} else if(fromMatch==FALSE) {			
										
				#instead of creating a data set from the output of Match, this will use the user specified data set as a Comparison
				#(matchZ) to the original data set (dataAA)
				matchall <- matchZ
		
				#create matchset
				matchset <- matchall[c(names, treatin)]
				
				#define treatment for "matchset"
				tr <- matchall[treatin]
				
				#matchsets by treatment
				matchsetat <- subset(matchall, tr == 1)
				matchsetac <- subset(matchall, tr == 0)
				matcht <- matchsetat[names]
				matchc <- matchsetac[names]
					
				#create unmatched set
				dataA <- dataAA[names]
				
				#assign treatment column
				treat <- dataAA[treatin] 
				
				#create unmatched set by treatment
				tdataA <- subset(dataA, treat == 1)
				cdataA <- subset(dataA, treat == 0)
			
				#if the subset command fails
				if(dim(tdataA)[1] == 0){
					tdataA <- dataA[dataA[treatin] == 1,]
					cdataA <- dataA[dataA[treatin] == 0,]
				}

				nvars <- ncol(dataA)
		} #end if
			
		#now add imputed values, if specified, to the dataframe
		
		if(impute == TRUE) {
			#control outcomes
			yc <- matchsetac[Y]
			
						
			#estimate coefficients for regression of controls units
			xc <- as.matrix(cbind(as.matrix(matchc), matrix(1, nrow(matchc), 1)))
			
			#b.c <-ginv(t(xc) %*% xc) %*% t(xc) %*% as.matrix(yc)
			#print(b.c)
			b.c <- lm(as.matrix(yc)~as.matrix(matchc))$coefficients
			
			yhat <- as.data.frame(as.matrix(cbind(matrix(1, nrow(matchall), 1),matchall[names])) %*% b.c)

			names(yhat)[1] <- "yhat"
			matchall <- data.frame( matchall, yhat)
			}
			
			
			
			
			#dim all the variable to be used in loop
			balu <- list()
			balm <- list()
			muT <- 1
			muC <- 1
			mdifu <- 1
			eqqu <- 1
			mmT <- 1
			mmC <- 1
			mdifm <- 1
			eqqm <- 1
			pctimp <- 1
			meanT <- vector()
			meanC <- vector()
			difmean <- vector()
			eqqdif <- vector()
			pctimpO <- vector()
			normdifu <- vector()
			normdifm <- vector()
			normdif <- vector()
			pctimprove <- vector()
			names2 <- vector()
			mu <- vector()
			
			count = 0
			#loop over each variable to calculate and compile stats
			nvars <- ncol(matchset)
			cat("##############################################################", "\n", nvars)
			for (i in 1:nvars) {
					
					#calls Sekhon's univariate balance function for unmatched variables
					balu <- balanceUV(dataA[, i][treat ==1], dataA[, i][treat != 1])
					
					#means unmatched// note store with [[]] call with []
					muT[[i]] <- balu$mean.Tr
					muC[[i]] <- balu$mean.Co
					
					#mean dif unmatched
					mdifu[[i]] <- muT[[i]] - muC[[i]]
					#raw mean Esq from balanceUV
					eqqu[[i]] <- balu$qqsummary.raw$meandiff
					#normalized difference for unmatched, as suggested by Imbens
					normdifu[i] <- abs(mdifu[i] / sqrt(sd(tdataA[, i])^2 + sd(cdataA[, i])^2)) 

		
					#calls Sekhon's univariate balance function for animated variables
					balm <- balanceUV(matchset[, i][tr ==1], matchset[, i][tr != 1])
					
					#means matched set
					mmT[[i]] <- balm$mean.Tr
					mmC[[i]] <- balm$mean.Co
					
					#mean dif matched
					mdifm[[i]] <- mmT[[i]] - mmC[[i]]
					#Esq for matched
					eqqm[[i]] <- balm$qqsummary.raw$meandiff
					#normdif for matched
					normdifm[i] <- abs(mdifm[i] / sqrt(sd(matcht[, i])^2 + sd(matchc[, i])^2))
					
					#pct meandif imp
					pctimp[[i]] <- (abs(mdifu[[i]]) - abs(mdifm[[i]])) / abs(mdifu[[i]])
					
					#this combines "stacks" all stats for each loop
					meanT <- round(rbind(meanT, muT[i], mmT[i]), prec)
					meanC <- round(rbind(meanC, muC[i], mmC[i]), prec)
					difmean <- round(rbind(difmean, mdifu[i], mdifm[i]), prec)
					normdif <- round(rbind(normdif, normdifu[i], normdifm[i]), prec)
					eqqdif <- round(rbind(eqqdif, eqqu[i], eqqm[i]), prec)
					pctimprove <- round(rbind(pctimprove, 0, pctimp[i]), prec)
					
					if (is.null(names)) {
							names2 <- rbind(names2, names(dataA)[i], names(dataA)[i]) 
					} else {
							names2 <- rbind(names2, names[i], names[i]) 
					}
					
					mu <- rbind(mu, "Unmatched", "Matched")
					
					#Option to plot pre and post match densities for each covariate
					if(d.plot == TRUE){
					
					#determine which covariates should be plotted
					if(is.null(plot.covs)){
						cov.plot <- seq(1, length(names), 1)
					} else{
						cov.plot <- plot.covs
					}

					#Either plot all densities in same figure
						if(plot.all == TRUE){
							if(count == 0){
								par(mfrow = c(ceiling((length(cov.plot)/plot.rows)), plot.rows))
							}#end if(count)
						#or plot all densities in separate windows
						} else {
								#only create new plot window if necessary
								if((count + 1) %in% cov.plot){
									x11() #create new graph
							}
						}#end if/else

						#density for the treated group, pre-match
						d.t.p <- density(dataA[, i][treat ==1], bw=sd(dataA[, i][treat ==1]* bw))

						#density for the control group, pre-match
						d.c.p <- density(dataA[, i][treat != 1], bw=sd(dataA[, i][treat !=1]* bw))

						#density for treated group, post-match
						d.t.m <- density(matcht[, i], bw=sd(matcht[, i]*bw))

						#density for control group, post-match
						d.c.m <- density(matchc[, i], bw=sd(matchc[, i]*bw))

						#only plot the covariate if called on
						if((count + 1) %in% cov.plot){
							plot(d.c.p, lwd = 2, ylim=c(0, max(d.t.p$y, d.c.p$y, d.t.m$y, d.c.m$y)), main=paste("Pre- and Post-Match Densities,", names[i], sep=" "))
							lines(d.c.m, col="red", lwd=4)
							lines(d.t.m, col="blue", lwd=2)
							lines(d.t.p, col="blue", lwd=2, lty="dashed")        
							segments(x0=mean(dataA[, i][treat ==1]), x1=mean(dataA[, i][treat ==1]), y0=0, y1=max(d.t.p$y, d.c.p$y, d.t.m$y, d.c.m$y), col="blue", lty="dashed")
							segments(x0=mean(dataA[, i][treat !=1]), x1=mean(dataA[, i][treat !=1]), y0=0, y1=max(d.t.p$y, d.c.p$y, d.t.m$y, d.c.m$y), col="black", lty="dashed")
							segments(x0=mean(matchc[, i]), x1=mean(matchc[, i]), y0=0, y1=max(d.t.p$y, d.c.p$y, d.t.m$y, d.c.m$y), col="red", lty="dashed")
							#add legend if called
							if(leg){
								legend("topright", c("All Untreated", "Matched Untreated", "Treated", "Matched Treated" ), col=c("black", "red", "blue", "blue"), lwd=2, lty=c(1,1,1,2), bg="white")# cex=2)
							}#end(if(leg)
						}#end if(count)
					}
			count = count +1		
			}
			
			#set output
			#condition, if the user specifies the return of a matched set
			if (matchout) {
					match.set <- as.data.frame(matchall)
					bal <- data.frame(names2, mu, meanT, meanC, difmean, normdif, eqqdif, pctimprove)
					out <- list(bal=bal, match.set=match.set)
			}	else {
					out <- data.frame(names2, mu, meanT, meanC, difmean, normdif, eqqdif, pctimprove)
			}	
			#option to write balance table to LaTeX
			if(ltx){
				if(is.null(ltx.title)){
					latex(bal)
				}else{
					latex(bal, title=ltx.title)
				}#end if/else
			}#end if (ltx)	
			return(out)
	}
	