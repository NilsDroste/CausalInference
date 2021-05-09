# Author: Werner Krause
# Contact: WZB Social Science Center Berlin, Reichpietschufer 50, 10785 Berlin
# Email: werner.krause@wzb.eu

# Date: 2017-12-22

# Abou-Chadi, Tarik and Krause, Werner: The Causal Effect of Radical Right 
# Success on Mainstream Parties’ Policy Positions. A Regression 
# Discontinuity Approach. BJPS. 

# > sessionInfo()
# R version 3.4.3 (2017-11-30)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS High Sierra 10.13.1
# 
# attached base packages:
#   [1] stats graphics  grDevices utils datasets methods base     
# other attached packages:
#   [1] rdd_0.57 magrittr_1.5 dplyr_0.7.4 memisc_0.99.14.9 
#   [5] sandwich_2.4-0 lmtest_0.9-35 ggplot2_2.2.1

## Cluster Function ####

# Functions for clustered standard errors as provided by Ian Gow: 
# http://www.people.hbs.edu/igow/GOT/Code/cluster2.R.html, 2017-12-22.

coeftest.cluster <- function( data , fm , cluster1 = NULL , cluster2 = NULL , ret = 'test' ) {
  options( warn = -1 )

  # Return White (1980) standard errors if no cluster
  # variable is provided
  if( is.null( cluster1 )) {
    if( ret == 'cov' ) {
      return( sandwich::vcovHC( fm , type = 'HC0' ))
    } else {
      return( coeftest( fm , vcov = vcovHC( fm , type = 'HC0' )))
    }
  }
  
  # Calculation shared by covariance estimates
  est.fun <- sandwich::estfun( fm )
  # est.fun <- sweep(fm$model,MARGIN=2,fm$residuals,`*`)
  
  # Need to identify observations used in the regression (i.e.,
  # non-missing) values, as the cluster vectors come from the full
  # data set and may not be in the regression model.
  inc.obs <- !is.na( est.fun[ , 1 ])
  est.fun <- est.fun[ inc.obs , ]
  
  # Shared data for degrees-of-freedom corrections
  N  <- dim( fm$model )[ 1 ]
  NROW <- NROW( est.fun )
  K  <- fm$rank
  
  # Calculate the sandwich covariance estimate
  cov <- function( cluster ) {
    cluster <- factor( cluster , exclude = NULL )
    
    # Calculate the "meat" of the sandwich estimators
    u <- apply( est.fun , 2 , function( x ) tapply( x , cluster , sum ))
    meat <- crossprod( u ) / N
    
    # Calculations for degrees-of-freedom corrections, followed
    # by calculation of the variance-covariance estimate.
    # NOTE: NROW/N is a kluge to address the fact that sandwich
    # uses the wrong number of rows (includes rows omitted from
    # the regression).
    M <- length( levels( cluster ))
    dfc <- M / ( M-1 ) * ( N-1 ) / ( N-K )
    
    #print (sandwich(fm, meat=meat))
    return( dfc * NROW / N * sandwich::sandwich( fm , meat = meat ))
  }
  
  # Calculate the covariance matrix estimate for the first cluster.
  cluster1 <- data[ inc.obs , cluster1 ]
  cov1  <- cov( cluster1 )
  # print(cov1)
  
  if ( is.null( cluster2 )) {
    # If only one cluster supplied, return single cluster
    # results
    if ( ret == 'cov' ) {
      return( cov1 )
    } else {
      return( lmtest::coeftest( fm , cov1 ))
    }
  } else {
    # Otherwise do the calculations for the second cluster
    # and the "intersection" cluster.
    cluster2 <- data[ inc.obs , cluster2]
    cluster12 <- paste( cluster1 , cluster2 , sep = '' )
    
    # Calculate the covariance matrices for cluster2, the "intersection"
    # cluster, then then put all the pieces together.
    cov2   <- cov( cluster2 )
    cov12  <- cov( cluster12 )
    covMCL <- ( cov1 + cov2 - cov12 )
    
    # Return the output of coeftest using two-way cluster-robust
    # standard errors.
    # print(ret)
    if ( ret == 'cov' ) {
      return( covMCL )
    } else {
      return( lmtest::coeftest( fm , covMCL ))
    }
  }
  options( warn = 0 )
}

summary.cluster <- function( obj , data , cluster1 , cluster2 = NULL , alpha = 0.05 ) {
  # Following based on suggestion from
  # https://stat.ethz.ch/pipermail/r-help/2011-January/264777.html
  # provided by Achim Zeileis.
  options( warn = -1 )
  # Get original summary
  s <- memisc::getSummary( obj , alpha = alpha )
  
  ## replace Wald tests of coefficients
  s$coef[ , 1 : 4 , 1 ] <- coeftest.cluster( data , obj , cluster1 , cluster2 )
  
  ## replace confidence intervals
  crit <- qt( alpha / 2 , obj$df.residual )
  s$coef[ , 5 , 1 ] <- s$coef[ , 1 , 1 ] + crit * s$coef[ , 2 , 1 ]
  s$coef[ , 6 , 1 ] <- s$coef[ , 1 , 1 ] - crit * s$coef[ , 2 , 1 ]
  
  # Note that some components of s$sumsstat will be inconsistent with
  # the clustered calculations
  
  return( s )
  options( warn = 0 )
}


## RD Functions ####

jump.plot <- function( data , force.var , yvar , seat.identifier , polynomial ){
  data <- data[ , c( force.var , yvar  , seat.identifier )]
  data <- na.omit( data )
  library( ggplot2 )
  p <- ggplot( ) +
    geom_point( data = data 
                , aes_string( x = force.var, y = yvar , shape = seat.identifier ) 
                , size = 2 ) +
    geom_smooth( data = subset( data , data[ , force.var] < 0 )
                 , aes_string ( x = force.var , y = yvar )
                 , method = 'lm' , formula = y ~ poly( x , polynomial , raw = TRUE )
                 , linetype = 1 , color = 'black' , size = 1 ) +
    geom_smooth( data = subset( data , data[ , force.var] >= 0 )
                 , aes_string ( x = force.var , y = yvar )
                 , method = 'lm' , formula = y ~ poly( x , polynomial , raw = TRUE )
                 , linetype = 1 , color = 'black' , size = 1 ) +
    scale_x_continuous( name = 'Vote Share of Radical Right Parties'
                        , limits = c( -5 , 10 )
                        , breaks = seq( -5 , 10 , 2.5 )) +
    scale_y_continuous( name = 'Cultural Protectionism' 
                        , limits = c( -8 , 8 )
                        , breaks = seq( -8 , 8 , 4 )) +
    scale_shape_manual( values = c( 1 , 19 ) 
                        , labels = c( 'RRP w/o seats     ' , 'RRP w seat(s)     ')) +
    geom_vline( xintercept = 0 , linetype = 2 , size = .6 ) +
    theme( legend.position = 'bottom' , legend.title = element_blank())
  return( p )
  detach( package:ggplot2 )
}

rd.core <- function( data , force.var , yvar , seat.identifier , fixed.effects 
                     , clust1 , clust2 , polynomial , bws ){
  i <- polynomial
  data <- as.data.frame( data )
  data <- data[ , c( yvar , force.var , fixed.effects , seat.identifier , clust1 , clust2 )]
  
  if( i <= 2 & is.null( bws )){
    h <- rdd::IKbandwidth( X = data[ , force.var ] , Y = data[ , yvar ]
                           , cutpoint = 0 , kernel = 'triangular' )
    data$w <- rdd::kernelwts( X = data[,force.var] , center = 0 
                              , bw = h,  kernel = 'triangular' )
  }
  if( i <= 2 & !is.null( bws )){
    h <- bws
    data$w <- rdd::kernelwts( X = data[ , force.var ] , center = 0
                              , bw = h , kernel = "triangular" )
  }
  
  if( !is.null( clust1 )){ data[ , clust1 ] <- as.factor( data[ , clust1 ])}
  if( !is.null( clust2 )){ data[ , clust2 ] <- as.factor( data[ , clust2 ])}
  data <- na.omit( data )
  
  data$above[ data[ , force.var] >= 0 & !is.na( data[ , force.var ])] <- 1
  data$above[ data[ , force.var] < 0 & !is.na( data[ , force.var ])] <- 0
  data$force_above <- data[ , force.var] * data$above
  
  formula = as.formula( paste( yvar, "~" , seat.identifier , " + poly (" , force.var , "," , i , " , raw = TRUE ) +
                              poly( force_above , " , i , " , raw = TRUE ) + as.factor( ", fixed.effects , " ) |
                              above + poly( " , force.var , "," , i , " , raw = TRUE )+
                              poly( force_above , " , i , " , raw = TRUE ) + as.factor( " , fixed.effects , " )" ))
  if( i <= 2 ){                  
    ivreg <- AER::ivreg( formula = formula
                         , weights = w
                         , data = subset( data , w > 0 ))
    data2 <- subset( data , w > 0 ) 
  }
  if( i > 2 ){ 
    ivreg <- AER::ivreg( formula = formula , data = data )
    data2 <- data
  }
  ivreg.out <- summary( ivreg )
  
  data2[ , clust1 ] <- as.factor( as.character( data2[ , clust1 ]))
  data2[ , clust2 ] <- as.factor( as.character( data2[ , clust2 ]))
  
  coeftest.cluster( data2 , ivreg , cluster1 = clust1 , cluster2 = clust2 )
  coef <- summary.cluster( ivreg , data2 , cluster1 = clust1 , cluster2 = clust2 , alpha = 0.05 )
  return( coef )
  
  
}

rd.base <- function( data , force.var , yvar , seat.identifier , fixed.effects 
                     , clust1 , clust2 , polynomials , bws ){
  data <- as.data.frame( data )
  data <- data[ , c( force.var , yvar , seat.identifier , fixed.effects , clust1 , clust2 )]
  data <- na.omit( data )
  
  for ( i in polynomials ){
    coef <- rd.core( data = data 
                     , force.var = force.var 
                     , yvar = yvar
                     , seat.identifier = seat.identifier 
                     , fixed.effects = fixed.effects 
                     , clust1 = clust1 
                     , clust2 = clust2 
                     , polynomial = i 
                     , bws = NULL )
    coef <- as.data.frame( t ( coef$coef[ 2 ,  , 1 ] ))
    if( i > 2 ){
      coef$IK_BW <- 'global'
      coef$Estimation <- 'Parametric'
      Nleft <- as.character( nrow( subset( data , data[ , force.var ] < 0 )))
      Nright <- as.character( nrow( subset( data , data[ , force.var ] >= 0 )))
    }
    if( i <= 2 & !is.null( bws )){
      coef$IK_BW <- bws
      coef$IK_BW <- sprintf( '%.3f' , round( coef$IK_BW , 3 ))
      coef$Estimation <- 'Non-Parametric'
      coef$Nleft <- as.character( nrow( subset( data.cut , data.cut[ , force.var ] >= h * -1 &  data.cut[ , force.var ] < 0 )))
      coef$Nright <- as.character( nrow( subset( data.cut , data.cut[ , force.var ] <= h &  data.cut[ , force.var ] >= 0 )))
    }
    if( i <= 2  & is.null( bws )){
      h <- rdd::IKbandwidth( X = data[ , force.var ] , Y = data[ , yvar ]
                             , cutpoint = 0 , kernel = 'triangular' )
      coef$IK_BW <- h
      coef$IK_BW <- sprintf( '%.3f' , round( coef$IK_BW , 3 ))
      coef$Estimation <- 'Non-Parametric'
      Nleft <- as.character( nrow( subset( data , data[ , force.var ] >= h * -1 &  data[ , force.var ] < 0 )))
      Nright <- as.character( nrow( subset( data , data[ , force.var ] <= h &  data[ , force.var ] >= 0 )))
    }
    
    coef$stars[ coef[ , 4 ] > .1 ] <- ''
    coef$stars[ coef[ , 4 ] <= .1 ] <- '*'
    coef$stars[ coef[ , 4 ] <= .05 ] <- '**'
    coef$stars[ coef[ , 4 ] <= .01 ] <- '***'
    coef$est <- sprintf( '%.3f' , round( coef$est , 3 ))
    coef$est <- paste( coef$est , coef$stars , sep = '' )
    coef$Poly <- i
    coef$Poly <- as.character( coef$Poly )
    coef$Nleft <- Nleft
    coef$Nright <- Nright
    coef[ , 'stars' ] <- NULL
    
    if( exists( 'return.ds' )){
      return.ds <- rbind( return.ds , coef )
    }
    if ( !exists( 'return.ds' )){
      return.ds <- coef
    }
  }
  return.ds <- return.ds[ , c( 1:2 , 4 , 7:11 ) ]
  
  colnames( return.ds ) <- c( 'LATE' , 'St. Err.' , 'p-value', 'Bandwith'
                              , 'Approach' , 'Polynomial' , 'N left of c' 
                              , 'N right of c' ) 
  return( return.ds )
}

rd.placebo <- function( data , force.var , yvar , seat.identifier , fixed.effects 
                        , clust1 , clust2 , polynomials , cut.ps , bws ){
  for( i in polynomials ){
    for( z in cut.ps){
      data.cut <- as.data.frame( data )
      data.cut <- data.cut[ , c( force.var , yvar , seat.identifier , fixed.effects , clust1 , clust2 )]
      data.cut <- na.omit( data.cut )
      data.cut[ , force.var ] <- data.cut[ , force.var ] - z
      data.cut[ , seat.identifier ] <- ifelse( data.cut[ , force.var ] >= 0 , 1 , 0 )
      data.cut[ , seat.identifier ] <- as.factor( as.character( data.cut[ , seat.identifier ]))
      
      if( i <= 2 & is.null( bws ) ){
        h <- rdd::IKbandwidth( X = data.cut[ , force.var ] , Y = data.cut[ , yvar ]
                               , cutpoint = 0 , kernel = 'triangular' )
        data.cut$w <- rdd::kernelwts( X = data.cut[ , force.var] , center = 0
                                      , bw = h, kernel = 'triangular' )
      }
      if( i <= 2 & !is.null( bws ) ){
        h <- bws
        data.cut$w <- rdd::kernelwts( X = data.cut[ , force.var ] , center = 0
                                      , bw = h , kernel = 'triangular' )
      }
      
      coef <- rd.core( data = data.cut
                       , force.var = force.var 
                       , yvar = yvar
                       , seat.identifier = seat.identifier 
                       , fixed.effects = fixed.effects 
                       , clust1 = clust1 
                       , clust2 = clust2 
                       , polynomial = i 
                       , bws = NULL )
      
      coef <- as.data.frame( t ( coef$coef[ 2 ,  , 1 ] ))
      
      coef$stars[coef[ , 4 , 1 ] > .1] <- ''
      coef$stars[coef[ , 4 , 1 ] <= .1] <- '*'
      coef$stars[coef[ , 4 , 1 ] <= .05] <- '**'
      coef$stars[coef[ , 4 , 1 ] <= .01] <- '***'
      coef$est <- sprintf( '%.3f' , round( coef$est , 3 ))
      coef$est <- paste( coef$est , coef$stars , sep = '' )
      
      if( i <= 2 & is.null( bws )){
        data <- as.data.frame( data )
        h <- rdd::IKbandwidth( X = data.cut[ , force.var ] , Y = data.cut[ , yvar ]
                               , cutpoint = 0 , kernel = 'triangular' )
        coef$IK_BW <- h
        coef$IK_BW <- sprintf( '%.3f' , round( coef$IK_BW , 3 ))
        coef$Estimation <- 'Non-Parametric'
        coef$Nleft <- as.character( nrow( subset( data.cut , data.cut[ , force.var ] >= h * -1 &  data.cut[ , force.var ] < 0 )))
        coef$Nright <- as.character( nrow( subset( data.cut , data.cut[ , force.var ] <= h &  data.cut[ , force.var ] >= 0 )))
      }
      if( i <= 2 & !is.null( bws )){
        coef$IK_BW <- bws
        coef$IK_BW <- sprintf( '%.3f' , round( coef$IK_BW , 3 ))
        coef$Estimation <- 'Non-Parametric'
        coef$Nleft <- as.character( nrow( subset( data.cut , data.cut[ , force.var ] >= h * -1 &  data.cut[ , force.var ] < 0 )))
        coef$Nright <- as.character( nrow( subset( data.cut , data.cut[ , force.var ] <= h &  data.cut[ , force.var ] >= 0 )))
      }
      if( i > 2 ){
        coef$IK_BW <- 'global'
        coef$Estimation <- 'Parametric'
        coef$Nleft <- as.character( nrow( subset( data.cut , data.cut[ , force.var ] < 0 )))
        coef$Nright <- as.character( nrow( subset( data.cut , data.cut[ , force.var ] >= 0 )))
      }
      
      coef$cutpoint <- z
      coef$poly <- i
      coef[ , 'stars' ] <- NULL
      
      if( exists( 'return.ds' )){
        return.ds <- rbind( return.ds , coef )
      }
      if (!exists( 'return.ds' )){
        return.ds <- coef
      }
    }
  }
  
  return.ds$cutpoint <- as.character( sprintf( '%.1f' , round( return.ds$cutpoint , 1 )))
  
  return.ds <- select( return.ds , 1:2 , 7 , 11 , 12 , 8 , 9 , 10 )
  
  colnames( return.ds ) <- c( 'LATE' , 'St. Err.' , 'Bandwidth' , 'Cut-off Point' 
                              , 'Polynomial (Degree)' , 'Approach' 
                              , 'N left of c' , 'N right of c' ) 
  return( return.ds )
  
}

rd.sens <- function( data , force.var , yvar , seat.identifier , fixed.effects 
                     , clust1 , clust2 , polynomials , bws ){
  for( i in polynomials ){
    for( w in bws ){
      coef <- rd.core( data = data 
                       , force.var = force.var 
                       , yvar = yvar
                       , seat.identifier = seat.identifier 
                       , fixed.effects = fixed.effects 
                       , clust1 = clust1 
                       , clust2 = clust2 
                       , polynomial = i 
                       , bws = w )
      coef <- as.data.frame( t ( coef$coef[ 2 ,  , 1 ] ))
      coef$bw <- w
      coef$poly <- i
      if( !exists( "return.ds" )){
        return.ds <- coef
      }
      if( exists( "return.ds" )){
        return.ds <- rbind( return.ds , coef )
      }
    }
  }
  return( return.ds )
}