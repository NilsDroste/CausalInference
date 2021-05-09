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

# > Variable description
# iso2c: iso2 character country code
# edate: election date
# party: CMP-code mainstream party
# partyname: name of mainstream party
# parfam: party family of mainstream party (CMP-coding)
# thrs: electoral threshold
# thrs_l: electoral threshold (lagged)
# er.v.c: radical right party vote share (centerd on electoral threshold)
# er.v.c_l: radical right party vote share (centerd on electoral threshold, lagged)
# er.in: radical right parliamentary presence (binary indicator)
# er.in_l: radical right parliamentary presence (binary indicator, lagged)
# rile.logit: rile score (according to Lowe et al. 2011)
# per607: multiculturalism positive (CMP coding)
# per608: multiculturalism negative (CMP coding)
# multic.logit_fd: cultural protectionism score (Lowe et al. 2011, first difference)
# per608_fd: per608 score (first difference)
# multic.ratio_fd: cultural protectionism score (Kim and Fording 2003, first difference) 
# af.bipolar_fd: cultural protectionism score (Alonso and da Fonseca 2012, first difference) 
# meguid.bipolar_fd: cultural protectionism score (Meguid 2008, first difference) 
# env.logit_fd: environment protection score (Lowe et al. 2011, first difference) 


rm( list = ls( ))
cat( '\014' )
library( dplyr )
library( magrittr )
source( 'rrp_rdd_functions.R' )
load( 'rrp_rdd.Rdata' )

# Figure 1: Mainstream party position change on cultural protectionism

p1 <- jump.plot( data = subset( ds , er.v.c_l <= 10 ) 
                 , force.var = 'er.v.c_l' 
                 , yvar = 'multic.logit_fd' 
                 , seat.identifier = 'er.in_l' 
                 , polynomial = 3 
                 ) 
p1

# Table 2: Mainstream party position change on cultural protectionism
rd.multic <- rd.base( data = ds
                      , force.var = 'er.v.c_l' 
                      , yvar = 'multic.logit_fd'
                      , seat.identifier = 'er.in_l' 
                      , fixed.effects = 'iso2c'
                      , clust1 = 'party' 
                      , clust2 = 'edate'
                      , polynomials = c( 1 , 2 , 3 , 4 ) 
                      , bws = NULL
                      )
rd.multic


# Table 3:  Mainstream left party position change on cultural protectionism

ds %<>%
  group_by( iso2c ) %>%
  mutate( country.mean.rile.logit = mean( rile.logit , na.rm = TRUE )) %>%
  ungroup( ) %>%
  group_by( party ) %>%
  mutate( mean.rile.logit = mean ( rile.logit , na.rm = TRUE )) %>%
  ungroup( )

rd.ml <- rd.base( data = subset( ds , ( mean.rile.logit < country.mean.rile.logit ))
                  , force.var = 'er.v.c_l' 
                  , yvar = 'multic.logit_fd'
                  , seat.identifier = 'er.in_l' 
                  , fixed.effects = 'iso2c'
                  , clust1 = 'party' 
                  , clust2 = 'edate'
                  , polynomials = c( 1 , 2 , 3 , 4 ) 
                  , bws = NULL
                  )
rd.ml

# Table 4:  Mainstream right party position change on cultural protectionism

rd.mr <- rd.base( data = subset( ds , ( mean.rile.logit > country.mean.rile.logit ))
                  , force.var = 'er.v.c_l' 
                  , yvar = 'multic.logit_fd'
                  , seat.identifier = 'er.in_l' 
                  , fixed.effects = 'iso2c'
                  , clust1 = 'party' 
                  , clust2 = 'edate'
                  , polynomials = c( 1 , 2 , 3 , 4 )
                  , bws = NULL
                  )
rd.mr

# Figure 2: LATE under varying bandwidths

rd.sens.data <- rd.sens( data = ds
                         , force.var = 'er.v.c_l'
                         , yvar = 'multic.logit_fd'
                         , seat.identifier = 'er.in_l'
                         , fixed.effects = 'iso2c'
                         , clust1 = 'party'
                         , clust2 = 'edate'
                         , polynomials = c( 1 , 2 )
                         , bws = seq( 1.5 , 10 , .25 )
                         )
rd.sens.data

# Figure 3: Mainstream party position change on cultural protectionism, countries with legally fixed threshold

c.list <- c( 'AT' , 'BG' , 'CZ' , 'DE' , 'EE' , 'GR' , 'LV' , 'NL' , 'PL' , 'RO' , 'SE' , 'SI')

ds %>% 
  filter( iso2c %in% c.list ) %>%  
  filter( !( iso2c == 'AT' & edate < '1994-01-01' )) %>%
  filter( !( iso2c == 'GR' & edate < '1993-01-01' )) %>%
  filter( !( iso2c == 'SI' & edate < '2000-01-01' )) -> ds.fixed


p3 <- jump.plot( data = subset( ds.fixed , er.v.c_l <= 10 ) 
                 , force.var = 'er.v.c_l' 
                 , yvar = 'multic.logit_fd' 
                 , seat.identifier = 'er.in_l' 
                 , polynomial = 3 
                 ) 
p3 

# Table 5: Mainstream party position change on cultural protectionism, countries with legally fixed threshold

rd.fixed <- rd.base( data = ds.fixed
                     , force.var = 'er.v.c_l' 
                     , yvar = 'multic.logit_fd'
                     , seat.identifier = 'er.in_l' 
                     , fixed.effects = 'iso2c'
                     , clust1 = 'party' 
                     , clust2 = 'edate'
                     , polynomials = c( 1 , 2 , 3 , 4 ) 
                     , bws = NULL
)
rd.fixed

# Table A2: Robustness - Mainstream party position change on cultural protectionism, DV: per608

rd.608 <- rd.base( data = ds
                   , force.var = 'er.v.c_l' 
                   , yvar = 'per608_fd'
                   , seat.identifier = 'er.in_l' 
                   , fixed.effects = 'iso2c'
                   , clust1 = 'party' 
                   , clust2 = 'edate'
                   , polynomials = c( 1 , 2 , 3 , 4 )
                   , bws = NULL
)
rd.608

# Table A3: Robustness - Mainstream party position change on cultural protectionism, DV: Kim and Fording (2003)

rd.kf <- rd.base( data = ds
                  , force.var = 'er.v.c_l' 
                  , yvar = 'multic.ratio_fd'
                  , seat.identifier = 'er.in_l' 
                  , fixed.effects = 'iso2c'
                  , clust1 = 'party' 
                  , clust2 = 'edate'
                  , polynomials = c( 1 , 2 , 3 , 4 )
                  , bws = NULL
                  )
rd.kf

# Table A4: Robustness - Mainstream party position change on cultural protectionism, DV: Alonso and da Fonseca (2012)

rd.af <- rd.base( data = ds
                  , force.var = 'er.v.c_l' 
                  , yvar = 'af.bipolar_fd'
                  , seat.identifier = 'er.in_l' 
                  , fixed.effects = 'iso2c'
                  , clust1 = 'party' 
                  , clust2 = 'edate'
                  , polynomials = c( 1 , 2 , 3 , 4 )
                  , bws = NULL
)
rd.af

# Table A5: Robustness - Mainstream party position change on cultural protectionism, DV: Meguid (2008)

rd.m <- rd.base( data = ds
                 , force.var = 'er.v.c_l' 
                 , yvar = 'meguid.bipolar_fd'
                 , seat.identifier = 'er.in_l' 
                 , fixed.effects = 'iso2c'
                 , clust1 = 'party' 
                 , clust2 = 'edate'
                 , polynomials = c( 1 , 2 , 3 , 4 )
                 , bws = NULL
                 )
rd.m

# Table A6: Placebo Test


ds %>% filter( er.v.c_l < 0 ) -> left
left.median <- median( left$er.v.c_l )
ds %>% filter( er.v.c_l >= 0 ) -> right
right.median <- median( right$er.v.c_l )

rd.bw <- rd.placebo( data = ds 
                     , force.var = 'er.v.c_l' 
                     , yvar = 'multic.logit_fd'
                     , seat.identifier = 'er.in_l' 
                     , fixed.effects = 'iso2c'
                     , clust1 = 'party' 
                     , clust2 = 'edate'
                     , polynomials = c( 1 , 2 , 3 , 4 )
                     , cut.ps = c( left.median , 0 , right.median )
                     , bws = NULL
                     )
rd.bw

# Table A7: Robustness - Mainstream party position change on environmental protection

rd.env <- rd.base( data = ds
                   , force.var = 'er.v.c_l' 
                   , yvar = 'env.logit_fd'
                   , seat.identifier = 'er.in_l' 
                   , fixed.effects = 'iso2c'
                   , clust1 = 'party' 
                   , clust2 = 'edate'
                   , polynomials = c( 1 , 2 , 3 , 4 )
                   , bws = NULL
                   )
rd.env

# Table A8: Robustness - Mainstream party position change on cultural protectionism, forcing variable: RRP vote share at election t

rd.notlagged <- rd.base( data = ds
                         , force.var = 'er.v.c' 
                         , yvar = 'multic.logit_fd'
                         , seat.identifier = 'er.in' 
                         , fixed.effects = 'iso2c'
                         , clust1 = 'party' 
                         , clust2 = 'edate'
                         , polynomials = c( 1 , 2 , 3 , 4 )
                         , bws = NULL
                         )
rd.notlagged

# Table A9: Jackknife analyses

ds %>% select( iso2c ) %>% unique( ) %>% as.list( ) %>% unlist( ) -> c.list

for( i in c.list ){
  print( i )
  rd.jk <- rd.base( data = subset( ds , iso2c != i )
                    , force.var = 'er.v.c_l' 
                    , yvar = 'multic.logit_fd'
                    , seat.identifier = 'er.in_l' 
                    , fixed.effects = 'iso2c'
                    , clust1 = 'party' 
                    , clust2 = 'edate'
                    , polynomials = c( 1 , 2 , 3 , 4 )
                    , bws = NULL
  )
  print( rd.jk )
}


# Detach Packages

detach( package:dplyr )
detach( package:magrittr )

