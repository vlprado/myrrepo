#Load Packages.
library(lubridate)
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(psych)
library(ggplot2)
library(readxl)


#//Set YOUR working directory
setwd("C:/Users/blopez/Documents/ElderlyLearning/visualMetrics/check/flanker")


#######################################################.
################# Flanker Task #########################.

ds0 <- read_csv("flanker_compress_long.csv")

### CREATE stim VARIABLE AND SELECT UNIQUE sriid-stim-rt_congr.

### Table 1. Compare  RT differences across groups (1-congr/0-incongr) with ANOVAs
ds1 <- ds0 %>% 
 mutate(stim = substr(stim_resp,1,1)) %>%
 mutate(stiml = as.numeric(grepl('^C',stim_resp))) %>%
 select(sriid, stim, stiml, rt_congr) %>%
 distinct(sriid, stim, stiml, rt_congr)
 
# run the linear regression and summarize results
reg <- lm(rt_congr ~ stiml, data=ds1)
summary(reg)


### Table 2. Regression RT with Age.
ds1 <- ds0 %>% 
 distinct(sriid, age, rt)

# run the linear regression and summarize results
reg <- lm(rt ~ age, data=ds1)
summary(reg)

# create a plot chart with regression line.
ggplot(data=ds1, aes(x=age, y=rt))+
 geom_point(shape=1)+
 geom_smooth(method=lm)


### Table 3. Compare  ACC differences across groups (0-incongr/1-congr) with ANOVAs
ds1 <- ds0 %>% 
 mutate(stim = substr(stim_resp,1,1)) %>%
 mutate(stiml = as.numeric(grepl('^C',stim_resp))) %>%
 select(sriid, stim, stiml, pctC_congr) %>%
 distinct(sriid, stim, stiml, pctC_congr)

# run the linear regression and summarize results
reg <- lm(pctC_congr ~ stiml, data=ds1)
summary(reg)


##### **Table 4. Regression ACC vs Age.
ds1 <- ds0 %>% 
 distinct(sriid, age, pctCorrect)

# run the linear regression and summarize results
reg <- lm(pctCorrect ~ age, data=ds1)
summary(reg)

# create a plot chart with regression line.
ggplot(data=ds1, aes(x=age, y=pctCorrect))+
 geom_point(shape=1)+
 geom_smooth(method=lm)


##### Table 5. Compare RT differences across groups (IL, IR, CL, CR) with ANOVAs
ds1 %>% group_by(stim_resp) %>% summarise(mean(rt_cirl), mean( pctC_cirl))
ds1 <- ds0 %>% 
 mutate(ir = as.numeric(grepl('IR',stim_resp))) %>%
 mutate(cl = as.numeric(grepl('CL',stim_resp))) %>%
 mutate(cr = as.numeric(grepl('CR',stim_resp))) %>%
 select(sriid, stim_resp, ir:cr, rt_cirl, pctC_cirl) %>%
 distinct(sriid, stim_resp, .keep_all=TRUE)

# run the linear regression and summarize results
reg <- lm(rt_cirl ~ ir + cl + cr, data=ds1)
summary(reg)


##### Table 6. Compare ACC differences across groups (IL, IR, CL, CR) with ANOVAs
##### ds1 <- USE DATA SET ds1 FROM TABLE 5.
# run the linear regression and summarize results
reg <- lm(pctC_cirl ~ ir + cl + cr, data=ds1)
summary(reg)





