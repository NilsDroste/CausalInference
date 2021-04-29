library(tidyverse)
library(gganimate)
library(ggthemes)
library(here)

df_test <- correlation::simulate_simpson(
  n = 100,
  r = 0.5,
  groups = 4
) %>% rename(X = V1, Y= V2, Person = Group) %>% 
  group_by(Person) %>%
  mutate(mean_X=mean(X),mean_Y=mean(Y)) %>%
  ungroup()

# df <- data.frame(Person = rep(1:4,50)) %>%
#   mutate(X = .5+.5*(Person-2.5) + rnorm(200)) %>%
#   mutate(Y = -.5*X + (Person-2.5) + 1 + rnorm(200),time="1") %>%
#   group_by(Person) %>%
#   mutate(mean_X=mean(X),mean_Y=mean(Y)) %>%
#   ungroup()

#Calculate correlations
before_cor <- paste("1. Start with raw data. Correlation between X and Y: ",round(cor(df$X,df$Y),3),sep='')
after_cor <- paste("6. Analyze what's left! Within-Individual Correlation Between X and Y: ",round(cor(df$X-df$mean_X,df$Y-df$mean_Y),3),sep='')

#Add step 2 in which X is demeaned, and 3 in which both X and Y are, and 4 which just changes label
dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(mean_X=NA,mean_Y=NA,time=before_cor),
  #Step 2: Add x-lines
  df %>% mutate(mean_Y=NA,time='2. Figure out any between-Individual differences in X'),
  #Step 3: X de-meaned 
  df %>% mutate(X = X - mean_X,mean_X=0,mean_Y=NA,time="3. Remove all between-Individual differences in X"),
  #Step 4: Remove X lines, add Y
  df %>% mutate(X = X - mean_X,mean_X=NA,time="4. Figure out any between-Individual differences in Y"),
  #Step 5: Y de-meaned
  df %>% mutate(X = X - mean_X,Y = Y - mean_Y,mean_X=NA,mean_Y=0,time="5. Remove all between-Individual differences in Y"),
  #Step 6: Raw demeaned data only
  df %>% mutate(X = X - mean_X,Y = Y - mean_Y,mean_X=NA,mean_Y=NA,time=after_cor))

p <- ggplot(dffull,aes(y=Y,x=X,color=as.factor(Person)))+geom_point()+
  geom_vline(aes(xintercept=mean_X,color=as.factor(Person)))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(Person)))+
  guides(color=guide_legend(title="Individual"))+
  scale_color_colorblind()+
  labs(title = 'The Relationship between Y and X, with Individual Fixed Effects \n{next_state}')+
  transition_states(time,transition_length=c(12,32,12,32,12,12),state_length=c(160,100,75,100,75,160),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()

anim <- animate(p,nframes=200, renderer = file_renderer("C:/Users/Nils/Box/teaching/CausalInf/lectures/02 - panel/R/figs", prefix = "FixedEffects", overwrite = TRUE))
# anim_save("FixedEffects.gif", anim, path="C:/Users/Nils/Box/teaching/CausalInf/lectures/02 - panel/R/figs")


fm_person <- lm(formula=Y~X+as.factor(Person), data=df)
fm_all <- lm(formula=Y~X, data=df)
df <- df %>% mutate(preds_person = predict(fm_person), preds_all = predict(fm_all))
df %>% ggplot(data = df, mapping = aes(y  = Y, x = X, color = as.factor(Person))) + geom_point() +  geom_line(mapping=aes(y=preds_person)) + guides(color = guide_legend(title = "Individual")) + scale_color_colorblind() + geom_line(aes(y=preds_all), color="red") 
ggsave("C:/Users/Nils/Box/teaching/CausalInf/lectures/02 - panel/R/figs/FEstatic.png")
