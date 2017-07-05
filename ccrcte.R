## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

knitr::opts_knit$set(root.dir = '')


library(openxlsx)
library(dtplyr)
library(dplyr)
library(data.table)
library(splitstackshape)
library(plyr)
library(lubridate)
library(lazyeval)
library(RODBC)
library(data.table)
library(reshape2)
library(knitr)
library(ggplot2)
library(RODBCext)
library(scales)
library(xtable)
library(reshape2)
library(pander)
library(tidyr)

## NOTE: remove NA's from tables when showing top schools for outcomes specified


## Individual school level
# full template (with grad rate - cri)

# Arrange schools with highest grad rates, 2015-16
# Arrange schools with highest gr_cri percentage change from 2013-14 to 2015-16

## CTE-designated summary level
## Means

# Are CTE grad rates increasing? 
# Are CTE cri increasing?
# Are CTE gr_cri increasing? 

# Other measures
# How are all other measures trending? 

## Comparison to all high schools

# Median for all schools for all measures
# Median for CTE-designated schools for all measures

# How do CTE grad rates and cri and gr_cri compare to all high schools, and the percentage change between all high schools



## ----readFiles, include=FALSE--------------------------------------------
ccr1314 <- read.xlsx("2013_2014_HS_SQR_Results_2015_03_02.xlsx", sheet="College and Career Readiness", startRow = 2)
sa1314 <- read.xlsx("2013_2014_HS_SQR_Results_2015_03_02.xlsx", sheet="Student Achievement", startRow = 2)

sa1314_1 <- sa1314[c(1,5,17)]
colnames(sa1314_1) <- paste("yr1314", colnames(sa1314_1), sep="_")
names(sa1314_1)[1] <- "DBN"

ccr1314_1 <- ccr1314[c(1,5,17,27,39,49)]
colnames(ccr1314_1) <- paste("yr1314", colnames(ccr1314_1), sep="_")
names(ccr1314_1)[1] <- "DBN"

sa1415 <- read.xlsx("2014_2015_HS_SQR_Results_2016_04_08.xlsx", sheet="Student Achievement", startRow = 2)
targ1415 <- read.xlsx("2014_2015_HS_SQR_Results_2016_04_08.xlsx", sheet="Targets", startRow = 2)

sa1415_1 <- sa1415[c(1,90,97,132)]
colnames(sa1415_1) <- paste("yr1415", colnames(sa1415_1), sep="_")
names(sa1415_1)[1] <- "DBN"

targ1415_1 <-targ1415[c(1,63,71,75,79)]
colnames(targ1415_1) <- paste("yr1415", colnames(targ1415_1), sep="_")
names(targ1415_1)[1] <- "DBN"

sa1516 <- read.xlsx("2015_2016_HS_SQR_Results_2017_01_05.xlsx", sheet="Student Achievement", startRow = 2)
cag1516 <- read.xlsx("2015_2016_HS_SQR_Results_2017_01_05.xlsx", sheet="Closing the Achievement Gap", startRow = 2)

sa1516_1 <- sa1516[c(1,56,63,167,153,146,160)]
colnames(sa1516_1) <- paste("yr1516", colnames(sa1516_1), sep="_")
names(sa1516_1)[1] <- "DBN"

cag1516_1 <- cag1516[c(1,62)]
colnames(cag1516_1) <- paste("yr1516", colnames(cag1516_1), sep="_")
names(cag1516_1)[1] <- "DBN"

t <- read.xlsx("2013-14 SQR example.xlsx", sheet="TEMPLATE", startRow=2)


## ----mergeDFs, include=FALSE---------------------------------------------

t1 <- t[c(1,2)]
str(t1)
rm(t2)
t2 <- t1 %>%
        left_join(sa1314_1, by="DBN") %>%
        left_join(ccr1314_1, by="DBN") %>%
        left_join(sa1415_1, by="DBN") %>%
        left_join(targ1415_1, by="DBN") %>%
        left_join(sa1516_1, by="DBN") %>%
        left_join(cag1516_1, by="DBN")
  

str(t2)



## ----filepath, include=FALSE---------------------------------------------

fn = paste("t2", format(Sys.time(), "%d-%m-%Y"),".csv", sep="-")

write.csv(t2, fn)

fp = paste(getwd(), fn, sep="/")

fp

#rm(list = ls())


## ----colNames, size="tiny", echo=FALSE-----------------------------------

names(t2)


## ----addColumnsToTemplate, include=FALSE---------------------------------

t2$yr1314_gr.cri4 <- t2$yr1314_Four.Year.Graduation.Rate - t2$`yr1314_4-Year.College.Readiness.Index`
t2$yr1314_gr.cri6 <- t2$yr1314_Six.Year.Graduation.Rate - t2$`yr1314_6-Year.College.Readiness.Index.with.Persistence`



t2$yr1415_gr.cri4 <- t2$`yr1415_Metric.Value.-.Graduation.Rate,.4.year` - t2$`yr1415_Metric.Value.-.Four-Year.College.Readiness.Index`
t2$yr1415_gr.cri6 <- t2$`yr1415_Metric.Value.-.Graduation.Rate,.6.year`- t2$`yr1415_Metric.Value.-.College.Readiness.Rate.including.persistence`


t2$yr1516_gr.cri4 <- t2$`yr1516_Metric.Value.-.Graduation.Rate,.4.year` - t2$`yr1516_Metric.Value.-.Four-Year.College.Readiness.Index`
t2$yr1516_gr.cri6 <- t2$`yr1516_Metric.Value.-.Graduation.Rate,.6-Year`- t2$`yr1516_Metric.Value.-.College.Persistence,.6-Year`


t3 <- t2 %>%
        mutate(perc_change4=((t2$yr1516_gr.cri4 - t2$yr1314_gr.cri4) /  t2$yr1314_gr.cri4)) %>%
        mutate(change4=(t2$yr1516_gr.cri4 - t2$yr1314_gr.cri4)) %>%
        mutate(perc_change6= ((t2$yr1516_gr.cri6 - t2$yr1314_gr.cri6) /  t2$yr1314_gr.cri6)) %>%
        mutate(change6 = (t2$yr1516_gr.cri6 - t2$yr1314_gr.cri6)) %>%
        mutate(gr_change4 = (t2$`yr1516_Metric.Value.-.Graduation.Rate,.4.year` - t2$yr1314_Four.Year.Graduation.Rate)) %>%
        mutate(gr_perc_change4 =((t2$`yr1516_Metric.Value.-.Graduation.Rate,.4.year` - t2$yr1314_Four.Year.Graduation.Rate) /  t2$yr1314_Four.Year.Graduation.Rate)) %>%
        mutate(gr_change6 =(t2$`yr1516_Metric.Value.-.Graduation.Rate,.6-Year` - t2$yr1314_Six.Year.Graduation.Rate)) %>%
        mutate(gr_perc_change6 = ((t2$`yr1516_Metric.Value.-.Graduation.Rate,.6-Year` - t2$yr1314_Six.Year.Graduation.Rate) /  t2$yr1314_Six.Year.Graduation.Rate)) %>%
        mutate(cri_change6 = ((t2$`yr1516_Metric.Value.-.College.Persistence,.6-Year` - t2$`yr1314_6-Year.College.Readiness.Index.with.Persistence`))) %>%
        mutate(cri_perc_change6 =((t2$`yr1516_Metric.Value.-.College.Persistence,.6-Year` - t2$`yr1314_6-Year.College.Readiness.Index.with.Persistence`) / t2$`yr1314_6-Year.College.Readiness.Index.with.Persistence`))

names(t3)

names(t3)[3] <- "yr1314_4yr_gradRate"
names(t3)[4] <- "yr1314_6yr_gradRate"
names(t3)[5] <- "yr1314_4yr_CRI"
names(t3)[6] <- "yr1314_6yr_CRI"
names(t3)[7] <- "yr1314_postSecnd_Enr_Rate_6MO"
names(t3)[8] <- "yr1314_postSecnd_Enr_Rate_18MO"
names(t3)[9] <- "yr1314_College_Career_Prep_CourseIndex"
names(t3)[10] <- "yr1415_4yr_gradRate"
names(t3)[11] <- "yr1415_6yr_gradRate"
names(t3)[12] <- "yr1415_6yr_CRI"
names(t3)[13] <- "yr1415_4yr_CRI"
names(t3)[14] <- "yr1415_postSecnd_Enr_Rate_6MO"
names(t3)[15] <- "yr1415_postSecnd_Enr_Rate_18MO"
names(t3)[16] <- "yr1415_College_Career_Prep_CourseIndex"
names(t3)[17] <- "yr1516_4yr_gradRate"
names(t3)[18] <- "yr1516_6yr_gradRate"
names(t3)[19] <- "yr1516_4yr_CRI"
names(t3)[20] <- "yr1516_postSecnd_Enr_Rate_6MO"
names(t3)[21] <- "yr1516_postSecnd_Enr_Rate_18MO"
names(t3)[22] <- "yr1516_College_Career_Prep_CourseIndex"
names(t3)[23] <- "yr1516_6yr_CRI"

names(t3)

fn_t3 = paste("t3", format(Sys.time(), "%d-%m-%Y"),".csv", sep="-")

write.csv(t3, fn_t3)

fp_t3 = paste(getwd(), fn_t3, sep="/")

fp_t3




## ----meanVals, include=FALSE---------------------------------------------

t3_m <- melt(t3)



t3_mean <- t3_m %>%
            group_by(variable) %>%
            summarise_each(funs(mean(., na.rm=TRUE)), -DBN, -School.Name)

t3_mean1_gradRate <- t3_mean %>%
              spread(key=variable, value=value) %>%
              select(yr1314_4yr_gradRate, yr1415_4yr_gradRate,yr1516_4yr_gradRate) %>%
              mutate(PC_1314_1415=((yr1415_4yr_gradRate - yr1314_4yr_gradRate)/yr1314_4yr_gradRate) * 100) %>%
              mutate(PC_1415_1516=((yr1516_4yr_gradRate - yr1415_4yr_gradRate)/yr1415_4yr_gradRate)* 100) %>%
              mutate(PC_1516_1314=((yr1516_4yr_gradRate - yr1314_4yr_gradRate)/yr1314_4yr_gradRate)* 100) %>%
              mutate_if(is.numeric, funs(round(.,2)))

t3_mean1_CRI <- t3_mean %>%
              spread(key=variable, value=value) %>%
              select(yr1314_4yr_CRI, yr1415_4yr_CRI, yr1516_4yr_CRI ) %>%
              mutate(PC_1314_1415=((yr1415_4yr_CRI - yr1314_4yr_CRI)/yr1314_4yr_CRI) * 100) %>%
              mutate(PC_1415_1516=((yr1516_4yr_CRI - yr1415_4yr_CRI)/yr1415_4yr_CRI)* 100) %>%
              mutate(PC_1516_1314=((yr1516_4yr_CRI - yr1314_4yr_CRI)/yr1314_4yr_CRI)* 100) %>%
              mutate_if(is.numeric, funs(round(.,2)))

t3_mean1_gap_4_yr<- t3_mean %>%
              spread(key=variable, value=value) %>%
              select(yr1314_gr.cri4, yr1415_gr.cri4, yr1516_gr.cri4 ) %>%
              mutate(PC_1314_1415=((yr1415_gr.cri4 - yr1314_gr.cri4)/yr1314_gr.cri4) * 100) %>%
              mutate(PC_1415_1516=((yr1516_gr.cri4 - yr1415_gr.cri4)/yr1415_gr.cri4)* 100) %>%
              mutate(PC_1516_1314=((yr1516_gr.cri4 - yr1314_gr.cri4)/yr1314_gr.cri4)* 100) %>%
              mutate_if(is.numeric, funs(round(.,2)))


## ----presentMeanValuesGR, size="tiny", echo=FALSE------------------------

kable(t3_mean1_gradRate)


## ----make_mgr1_graph, include=FALSE--------------------------------------
mgr <- t3_mean1_gradRate %>%
        select(yr1314_4yr_gradRate,yr1415_4yr_gradRate, yr1516_4yr_gradRate)

mgr1 <- melt(mgr)


mgr1_graph <- ggplot(data=mgr1, aes(x=variable, y=value, group=1, color=variable)) +
    geom_point(shape=1) + geom_line() + theme(legend.position="none")
    


## ----mgr1_graph, echo=FALSE----------------------------------------------

mgr1_graph


## ----presentMeanValuesGap, size="tiny",echo=FALSE------------------------

kable(t3_mean1_gap_4_yr)


## ----makegapgraph, include=FALSE-----------------------------------------

mgap <- t3_mean1_gap_4_yr %>%
        select(yr1314_gr.cri4,yr1415_gr.cri4, yr1516_gr.cri4)

mgap1 <- melt(mgap)



mgap1_graph <- ggplot(data=mgap1, aes(x=variable, y=value, group=1, color=variable)) +
    geom_point(shape=1) + geom_line() + theme(legend.position="none")


## ----gapgraph, size="tiny",echo=FALSE------------------------------------

mgap1_graph


## ----makecrigraph, include=FALSE-----------------------------------------
t3_mean1_CRI

mcri <- t3_mean1_CRI %>%
        select(yr1314_4yr_CRI,yr1415_4yr_CRI, yr1516_4yr_CRI)

mcri1 <- melt(mcri)

mcri1

mcri1_graph <- ggplot(data=mcri1, aes(x=variable, y=value, group=1, color=variable)) +
    geom_point(shape=1) + geom_line() + theme(legend.position="none")




## ----crigraph, echo=FALSE------------------------------------------------

mcri1_graph 


## ----presentMeanValuesCRI, echo=FALSE------------------------------------

kable(t3_mean1_CRI)



## ----mostImprovementGapCalc, include=FALSE-------------------------------

mst_imp_gap <- t3 %>%
                  select(DBN, School.Name, yr1314_gr.cri4,yr1415_gr.cri4,yr1516_gr.cri4, perc_change4 ) %>%
                  arrange(perc_change4) %>%
                  mutate(perc_change4=(perc_change4* 100)) %>%
                  mutate_if(is.numeric, funs(round(.,2))) %>%
                  slice(1:5)




## ----mostImprovementTable, echo=FALSE------------------------------------

kable(mst_imp_gap)



## ----leastImprovementGapCalc, include=FALSE------------------------------


str(t3)
lst_imp_gap <- t3 %>%
                  select(DBN, School.Name, yr1314_gr.cri4,yr1415_gr.cri4,yr1516_gr.cri4, perc_change4 ) %>%
                  arrange(desc(perc_change4)) %>%
                  mutate(perc_change4=(perc_change4* 100)) %>%
                  mutate_if(is.numeric, funs(round(.,2))) %>%
                  slice(1:5)


## ----leastImprovementTable, echo=FALSE-----------------------------------

kable(lst_imp_gap)



## ----top10_4yr_grad_rates, echo=FALSE------------------------------------
top_10_gradRates <- t3 %>%
                  select(DBN, School.Name, yr1314_4yr_gradRate,yr1415_4yr_gradRate,yr1516_4yr_gradRate ) %>%
                  arrange(desc(yr1516_4yr_gradRate)) %>%
                  mutate_if(is.numeric, funs(round(.,2))) %>%
                  slice(1:5)


kable(top_10_gradRates)


## ----top10_4yr_CRI, echo=FALSE-------------------------------------------


top_10_CRI <- t3 %>%
                  select(DBN, School.Name, yr1314_4yr_CRI,yr1415_4yr_CRI,yr1516_4yr_CRI ) %>%
                  arrange(desc(yr1516_4yr_CRI)) %>%
                  mutate_if(is.numeric, funs(round(.,2))) %>%
                  slice(1:5)


kable(top_10_CRI)


## ----cityComparison, include=FALSE---------------------------------------
b2 <- sa1314_1 %>%
      left_join(ccr1314_1, by="DBN") %>%
      left_join(sa1415_1, by="DBN") %>%
      left_join(targ1415_1, by="DBN") %>%
      left_join(sa1516_1, by="DBN") %>%
      left_join(cag1516_1, by="DBN")


b2$yr1314_gr.cri4 <- b2$yr1314_Four.Year.Graduation.Rate - b2$`yr1314_4-Year.College.Readiness.Index`
b2$yr1314_gr.cri6 <- b2$yr1314_Six.Year.Graduation.Rate - b2$`yr1314_6-Year.College.Readiness.Index.with.Persistence`



b2$yr1415_gr.cri4 <- b2$`yr1415_Metric.Value.-.Graduation.Rate,.4.year` - b2$`yr1415_Metric.Value.-.Four-Year.College.Readiness.Index`
b2$yr1415_gr.cri6 <- b2$`yr1415_Metric.Value.-.Graduation.Rate,.6.year`- b2$`yr1415_Metric.Value.-.College.Readiness.Rate.including.persistence`


b2$yr1516_gr.cri4 <- b2$`yr1516_Metric.Value.-.Graduation.Rate,.4.year` - b2$`yr1516_Metric.Value.-.Four-Year.College.Readiness.Index`
b2$yr1516_gr.cri6 <- b2$`yr1516_Metric.Value.-.Graduation.Rate,.6-Year`- b2$`yr1516_Metric.Value.-.College.Persistence,.6-Year`


b3 <- b2

b3 <- b2 %>%
        mutate(perc_change4=((b2$yr1516_gr.cri4 - b2$yr1314_gr.cri4) /  b2$yr1314_gr.cri4)) %>%
        mutate(change4=(b2$yr1516_gr.cri4 - b2$yr1314_gr.cri4)) %>%
        mutate(perc_change6= ((b2$yr1516_gr.cri6 - b2$yr1314_gr.cri6) /  b2$yr1314_gr.cri6)) %>%
        mutate(change6 = (b2$yr1516_gr.cri6 - b2$yr1314_gr.cri6)) %>%
        mutate(gr_change4 = (b2$`yr1516_Metric.Value.-.Graduation.Rate,.4.year` - b2$yr1314_Four.Year.Graduation.Rate)) %>%
        mutate(gr_perc_change4 =((b2$`yr1516_Metric.Value.-.Graduation.Rate,.4.year` - b2$yr1314_Four.Year.Graduation.Rate) /  b2$yr1314_Four.Year.Graduation.Rate)) %>%
        mutate(gr_change6 =(b2$`yr1516_Metric.Value.-.Graduation.Rate,.6-Year` - b2$yr1314_Six.Year.Graduation.Rate)) %>%
        mutate(gr_perc_change6 = ((b2$`yr1516_Metric.Value.-.Graduation.Rate,.6-Year` - b2$yr1314_Six.Year.Graduation.Rate) /  b2$yr1314_Six.Year.Graduation.Rate)) %>%
        mutate(cri_change6 = ((b2$`yr1516_Metric.Value.-.College.Persistence,.6-Year` - b2$`yr1314_6-Year.College.Readiness.Index.with.Persistence`))) %>%
        mutate(cri_perc_change6 =((b2$`yr1516_Metric.Value.-.College.Persistence,.6-Year` - b2$`yr1314_6-Year.College.Readiness.Index.with.Persistence`) / b2$`yr1314_6-Year.College.Readiness.Index.with.Persistence`))



names(b3)[2] <- "yr1314_4yr_gradRate"
names(b3)[3] <- "yr1314_6yr_gradRate"
names(b3)[4] <- "yr1314_4yr_CRI"
names(b3)[5] <- "yr1314_6yr_CRI"
names(b3)[6] <- "yr1314_postSecnd_Enr_Rate_6MO"
names(b3)[7] <- "yr1314_postSecnd_Enr_Rate_18MO"
names(b3)[8] <- "yr1314_College_Career_Prep_CourseIndex"
names(b3)[9] <- "yr1415_4yr_gradRate"
names(b3)[10] <- "yr1415_6yr_gradRate"
names(b3)[11] <- "yr1415_6yr_CRI"
names(b3)[12] <- "yr1415_4yr_CRI"
names(b3)[13] <- "yr1415_postSecnd_Enr_Rate_6MO"
names(b3)[14] <- "yr1415_postSecnd_Enr_Rate_18MO"
names(b3)[15] <- "yr1415_College_Career_Prep_CourseIndex"
names(b3)[16] <- "yr1516_4yr_gradRate"
names(b3)[17] <- "yr1516_6yr_gradRate"
names(b3)[18] <- "yr1516_4yr_CRI"
names(b3)[19] <- "yr1516_postSecnd_Enr_Rate_6MO"
names(b3)[20] <- "yr1516_postSecnd_Enr_Rate_18MO"
names(b3)[21] <- "yr1516_College_Career_Prep_CourseIndex"
names(b3)[22] <- "yr1516_6yr_CRI"


m_b <- melt(b3)

str(m_b)

bval_1 <- m_b %>%
    group_by(variable) %>%
  summarise_each(funs(mean(., na.rm=TRUE)), -DBN)        

bval_1
str(bval_1)


str(t3)




city_cte <- bval_1 %>%
              left_join(t3_mean, by="variable") %>%
              mutate(city_mean=value.x) %>%
              mutate(cte_mean=value.y) %>%
              select(-value.x, -value.y) %>%
              mutate(city_cte_diff=(city_mean-cte_mean)) %>%
              mutate_if(is.numeric, funs(round(.,2)))
city_cte




## ----city_cte, echo=FALSE------------------------------------------------

kable(city_cte)


## ----city_cte_gap, include=FALSE-----------------------------------------


city_cte_cri <- city_cte %>%
                  filter(variable == 'yr1516_gr.cri4')

## ----city_cte_graph_gap, include=FALSE-----------------------------------

city_cte_gap_1 <- city_cte %>%
                    select(-city_cte_diff) %>%
                  filter((variable == 'yr1314_gr.cri4') | (variable == 'yr1516_gr.cri4') | (variable == 'yr1415_gr.cri4'))
             


city_cte_gap_2 <- melt(city_cte_gap_1)

names(city_cte_gap_2)[2] <- "category"


city_cte_gap_2_graph <- ggplot(data=city_cte_gap_2, aes(x=variable, y=value, group=category, color=category)) +
    geom_point(shape=1) + geom_line() 


## ----graph_cityctegap, echo=FALSE----------------------------------------

kable(city_cte_gap_1)

city_cte_gap_2_graph


## ----city_cte_gr, include=FALSE------------------------------------------

city_cte_gr <- city_cte %>%
                  filter(variable == 'yr1516_4yr_gradRate')

city_cte_gr$yr1516_4yr_gradRate




## ----city_cte_graph_gr, include=FALSE------------------------------------

city_cte_gr_1 <- city_cte %>%
                filter(variable == c('yr1314_4yr_gradRate','yr1415_4yr_gradRate','yr1516_4yr_gradRate')) %>%
                select(-city_cte_diff)



city_cte_gr_2 <- melt(city_cte_gr_1)

names(city_cte_gr_2)[2] <- "category"

city_cte_gr_2_graph <- ggplot(data=city_cte_gr_2, aes(x=variable, y=value, group=category, color=category)) +
    geom_point(shape=1) + geom_line() 


## ----cityctegraphgr, echo=FALSE------------------------------------------


city_cte_gr_2_graph

kable(city_cte_gr_1)



## ----app_mst_imp,  echo=FALSE--------------------------------------------

app_mst_imp_gap <- t3 %>%
                  select(DBN, School.Name, yr1314_gr.cri4,yr1415_gr.cri4,yr1516_gr.cri4, perc_change4 ) %>%
                  arrange(perc_change4) %>%
                  mutate(perc_change4=(perc_change4* 100)) %>%
                  mutate_if(is.numeric, funs(round(.,2)))

kable(app_mst_imp_gap)


## ----app_5_4yr_grad_rates, echo=FALSE------------------------------------
app_gradRates <- t3 %>%
                  select(DBN, School.Name, yr1314_4yr_gradRate,yr1415_4yr_gradRate,yr1516_4yr_gradRate ) %>%
                  arrange(desc(yr1516_4yr_gradRate)) %>%
                  mutate_if(is.numeric, funs(round(.,2))) 


kable(app_gradRates)


## ----app_4yr_CRI, echo=FALSE---------------------------------------------

app_CRI <- t3 %>%
                  select(DBN, School.Name, yr1314_4yr_CRI,yr1415_4yr_CRI,yr1516_4yr_CRI ) %>%
                  arrange(desc(yr1516_4yr_CRI)) %>%
                  mutate_if(is.numeric, funs(round(.,2))) 


kable(app_CRI)


