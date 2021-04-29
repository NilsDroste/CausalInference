library(tidyverse)
library(gganimate)
library(ggthemes)

# setting a seed for the random numer generation 
set.seed(132456)

# generating the data
df <- data.frame(C = as.integer((1:200>100))) %>%
  mutate(T = .5+2*C + rnorm(200)) %>%
  mutate(Y = -.5*T + 4*C + 1 + rnorm(200)) %>% 
  group_by(C) %>%
  mutate(mean_T=mean(T),mean_Y=mean(Y)) %>%
  ungroup()

# writing out the data to read it in during class
write_csv(df, "./data/data.csv")

#plot to check the distribution
ggplot(df,aes(y=Y,x=T,color=as.factor(C)))+geom_point()+
  guides(color=guide_legend(title="C"))+
  scale_color_colorblind()

#Calculate correlations
before_cor <- paste("1. Start with raw data. Correlation between T and Y: ",round(cor(df$T,df$Y),3),sep='')
after_cor <- paste("6. Analyze what's left! Correlation between T and Y controlling for W: ",round(cor(df$T-df$mean_T,df$Y-df$mean_Y),3),sep='')


#Add step 2 in which T is demeaned, and 3 in which both T and Y are, and 4 which just changes label
dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(mean_T=NA,mean_Y=NA,time=before_cor),
  #Step 2: Add T-lines
  df %>% mutate(mean_Y=NA,time='2. Figure out what differences in T are explained by C'),
  #Step 3: T de-meaned 
  df %>% mutate(T = T - mean_T,mean_T=0,mean_Y=NA,time="3. Remove differences in X explained by C"),
  #Step 4: Remove T lines, add Y
  df %>% mutate(T = T - mean_T,mean_T=NA,time="4. Figure out what differences in Y are explained by C"),
  #Step 5: Y de-meaned
  df %>% mutate(T = T - mean_T,Y = Y - mean_Y,mean_T=NA,mean_Y=0,time="5. Remove differences in Y explained by C"),
  #Step 6: Raw demeaned data only
  df %>% mutate(T = T - mean_T,Y = Y - mean_Y,mean_T=NA,mean_Y=NA,time=after_cor))

p <- ggplot(dffull,aes(y=Y,x=T,color=as.factor(C)))+geom_point()+
  geom_vline(aes(xintercept=mean_T,color=as.factor(C)))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(C)))+
  guides(color=guide_legend(title="C"))+
  scale_color_colorblind()+
  labs(title = 'The Relationship between Y and T, Controlling for a Binary Variable C \n{next_state}')+
  transition_states(time,transition_length=c(12,32,12,32,12,12),state_length=c(160,100,75,100,75,160),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()

animate(p,nframes=200)


