#Libraries and data imports####
library("ipumsr")
library(readxl)
library(ggplot2)
library(stats)
library(Rlab)
library(ineq)
library(weights)
library(xtable)
library(githubinstall)
library(overlapping)
library(usmap)
library(sjlabelled)
library(reticulate)
library(reticulate)
library(quantreg)
library(tidyverse)
library(lubridate)
library(yaml)
library(rprojroot)
library("RColorBrewer")
library(spatstat)
library(acid)

setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/COVID/RoSE/Data/IPUMS")

ddi <- read_ipums_ddi("cps_00012.xml")
data <- read_ipums_micro(ddi)

data_ASEC_2020<-subset(data, ASECFLAG==1 & MONTH==3)
data_ASEC_2020$MARCH_EMPL_STAT<-data_ASEC_2020$EMPSTAT
data_april_2020<-subset(data, MONTH==4)
data_april_2020$APRIL_EMPL_STAT<-data_april_2020$EMPSTAT

#create datasets######


#creaet intersection of ASEC and April dataset:
intersection<-merge(data_ASEC_2020, data_april_2020[,c(which(colnames(data_april_2020)=="CPSIDP"),which(colnames(data_april_2020)=="APRIL_EMPL_STAT"))], by="CPSIDP")

#comparing ASEC vs intersection:
x<-list(data_ASEC_2020$INCWAGE[data_ASEC_2020$INCWAGE<99999 & data_ASEC_2020$INCWAGE>0]*data_ASEC_2020$ASECWT[data_ASEC_2020$INCWAGE<99999 & data_ASEC_2020$INCWAGE>0],
        intersection$INCWAGE[intersection$INCWAGE<99999 & intersection$INCWAGE>0]*intersection$ASECWT[intersection$INCWAGE<99999 & intersection$INCWAGE>0])

overlap(x, plot=FALSE)

#plot overlapping densities:
plot(density(data_ASEC_2020$INCWAGE[data_ASEC_2020$INCWAGE<99999 & data_ASEC_2020$INCWAGE>0]*data_ASEC_2020$ASECWT[data_ASEC_2020$INCWAGE<99999 & data_ASEC_2020$INCWAGE>0]), col="blue",bty='n', xlim=c(0,400000000),
     xlab="",
     main="")
lines(density(intersection$INCWAGE[intersection$INCWAGE<99999 & intersection$INCWAGE>0]*intersection$ASECWT[intersection$INCWAGE<99999 & intersection$INCWAGE>0]), col="red")


#remove those NILF & with zero wage income:
intersection<-subset(intersection, MARCH_EMPL_STAT>=10 & MARCH_EMPL_STAT <=22 & APRIL_EMPL_STAT>=10 & APRIL_EMPL_STAT <=22)
intersection<-subset(intersection, INCWAGE>0)


data_ASEC_2020<-subset(data_ASEC_2020, MARCH_EMPL_STAT>=10 & MARCH_EMPL_STAT <=22 & EMPSTAT>=10 & EMPSTAT <=22)
data_ASEC_2020<-subset(data_ASEC_2020, INCWAGE>0)


#comparing ASEC vs intersection:
x<-list(data_ASEC_2020$INCWAGE[data_ASEC_2020$INCWAGE<99999 & data_ASEC_2020$INCWAGE>0]*data_ASEC_2020$ASECWT[data_ASEC_2020$INCWAGE<99999 & data_ASEC_2020$INCWAGE>0],
        intersection$INCWAGE[intersection$INCWAGE<99999 & intersection$INCWAGE>0]*intersection$ASECWT[intersection$INCWAGE<99999 & intersection$INCWAGE>0])

overlap(x, plot=TRUE)

#reformate employment status measurement (1=employed, 2=unemployed):
intersection$MARCH_EMPL_STAT<-round(intersection$MARCH_EMPL_STAT/10)
intersection$APRIL_EMPL_STAT<-round(intersection$APRIL_EMPL_STAT/10)



#Create table of industry unemployment rates:


naics_description <- read_excel("naics_description.xlsx")
x<-merge(ind_unempl, naics_description, by="Group.1")
print(xtable(x[-1,c(1,18,13,12,4,7)], type="latex"), include.rownames = FALSE )

#EXERCISE 1#####

#import state-level unemployment policies:
unemployment_benefits <- read.csv("unemployment_benefits.csv")
unemployment_benefits$fips <- fips(unemployment_benefits$State)
unemployment_benefits$fips <- as.numeric(fips(unemployment_benefits$State))
intersection<-merge(intersection, unemployment_benefits[,c(4,7,8)], by.x="STATEFIP", by.y="fips")

#rescale and apply weights:
#can just multiply by weights

#USE GITHUB Calculator:
setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/COVID/RoSE/Data/IPUMS/ui_calculator-master")
ui_calculator = import("ui_calculator")
palette <- RColorBrewer::brewer.pal(6, "Blues")
setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/COVID/RoSE/Data/IPUMS")

x<-data.frame(as.numeric(fips(state.abb)),state.abb)
intersection<-merge(intersection,x,by.x="STATEFIP", by.y = "as.numeric.fips.state.abb..")
intersection$state<-intersection$state.abb

wages<-intersection %>%
  transmute(state,
            CPSID,
            fips = STATEFIP,
            weight = ASECWT,
            wage = INCWAGE,
            employment_status = EMPSTAT,
            weeks_worked = WKSWORK1) %>%
  mutate(weekly_earnings = wage / weeks_worked,
         q1_earnings = weeks_worked - 39,
         q2_earnings = weeks_worked - 26,
         q3_earnings = weeks_worked - 13,
         q4_earnings = weeks_worked) %>%
  mutate_at(vars(matches("q[1-4]_earnings")),
            ~ case_when(.x > 13 ~ 13 * weekly_earnings,
                        .x < 0 ~ 0,
                        TRUE ~ .x * weekly_earnings)) 

wages <- wages %>%
  mutate(benefits_amount =
           ui_calculator$calc_weekly_state_quarterly(q1_earnings,
                                                     q2_earnings,
                                                     q3_earnings,
                                                     q4_earnings,
                                                     state,
                                                     weeks_worked) %>% map_dbl(1))

intersection<-merge(intersection, wages, by="CPSID")

intersection$april_no_cares_income<-0
intersection$april_no_cares_income[intersection$APRIL_EMPL_STAT==2]<-intersection$benefits_amount[intersection$APRIL_EMPL_STAT==2]
intersection$april_no_cares_income[intersection$APRIL_EMPL_STAT==1]<-intersection$weekly_earnings[intersection$MARCH_EMPL_STAT==1]

intersection$april_cares_income<-0
intersection$april_cares_income[intersection$APRIL_EMPL_STAT==2]<-intersection$benefits_amount[intersection$APRIL_EMPL_STAT==2]+600
intersection$april_cares_income[intersection$APRIL_EMPL_STAT==1]<-intersection$weekly_earnings[intersection$MARCH_EMPL_STAT==1]

intersection$march_income<-0
intersection$march_income[intersection$MARCH_EMPL_STAT==2]<-intersection$benefits_amount[intersection$MARCH_EMPL_STAT==2]
intersection$march_income[intersection$MARCH_EMPL_STAT==1]<-intersection$weekly_earnings[intersection$MARCH_EMPL_STAT==1]

plot(density(intersection$april_cares_income), xlim=c(0,6000), col="blue", type='l',bty='n',
           xlab="",
           main="")
lines(density(intersection$march_income[!is.na(intersection$march_income)]), xlim=c(0,6000), col="black")
lines(density(intersection$april_no_cares_income), xlim=c(0,6000), col="red")

#~0.59 is consistent with pre-tax wage income..
#Gini(intersection$april_cares_income*intersection$ASECWT)
#Gini(intersection$april_no_cares_income*intersection$ASECWT)
#Gini(intersection$march_income*intersection$ASECWT)

#WEIGHTED GINI AND MEDIAN:
weighted.gini(intersection$april_cares_income, intersection$ASECWT)
weighted.gini(intersection$april_no_cares_income, intersection$ASECWT)
weighted.gini(intersection$march_income, intersection$ASECWT)

weighted.median(intersection$april_cares_income, intersection$ASECWT)
weighted.median(intersection$april_no_cares_income, intersection$ASECWT)
weighted.median(intersection$march_income, intersection$ASECWT)

weighted.mean(intersection$april_cares_income, intersection$ASECWT)
weighted.mean(intersection$april_no_cares_income, intersection$ASECWT)
weighted.mean(intersection$march_income, intersection$ASECWT)


# #plot distributions based on employment status in April:
intersection$APRIL_EMPL_STAT<-abs(intersection$APRIL_EMPL_STAT-2)

plot(density(intersection$march_income[intersection$MARCH_EMPL_STAT==1 & intersection$APRIL_EMPL_STAT==1]), xlim=c(0,6000),ylim=c(0,0.0015), col="blue",bty='n',
     xlab="",
     main="")
lines(density(intersection$march_income[intersection$MARCH_EMPL_STAT==1 & intersection$APRIL_EMPL_STAT==0]), col="red")

ggplot(subset(intersection, MARCH_EMPL_STAT==1), aes(x=march_income, group=as.factor(c(APRIL_EMPL_STAT)), fill=as.factor(APRIL_EMPL_STAT)))+
  geom_density(alpha=0.3)+
  xlab("$")+
  ylab("Density")+
  xlim(0,6000)+
  ylim(0,0.0015)+
  theme_bw()+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  scale_fill_discrete(breaks=c(0,1),labels=c("Lost employment","Kept employment"))+
  theme(  legend.position = c(0.85,0.5),
          legend.title = element_blank(),
          #axis.title.y = element_blank(),
          #axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          #axis.text.x = element_blank()
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()
  )


weighted.median(intersection$march_income[intersection$APRIL_EMPL_STAT==1], intersection$ASECWT[intersection$APRIL_EMPL_STAT==1])
weighted.median(intersection$march_income[intersection$APRIL_EMPL_STAT==0], intersection$ASECWT[intersection$APRIL_EMPL_STAT==1])



#EXERCISE 2#####

ddi <- read_ipums_ddi("cps_00012.xml")
data2 <- read_ipums_micro(ddi)

data_july_2020 <- subset(data2, YEAR == 2020 & MONTH==7)

#COMPUTE JULY UNEMPLOYMENT RATES####

#remove those NILF & with zero wage income:
data_july_2020<-subset(data_july_2020, EMPSTAT>=10 & EMPSTAT <=22)

#reformate employment status measurement (1=employed, 0=unemployed):
data_july_2020$EMPSTAT<-round(data_july_2020$EMPSTAT/10)
data_july_2020$EMPSTAT <- abs(data_july_2020$EMPSTAT-2)

#apply crosswalk (see https://usa.ipums.org/usa/volii/indtoindnaics18.shtml) 
crosswalk <- read_excel("croswalk.xlsx")
crosswalk$IND <- as.numeric(crosswalk$IND)
data_july_2020<-merge(data_july_2020, crosswalk, by="IND")
data_july_2020$NAICS<- substr(data_july_2020$NAICS, 1,3)


#Bin split or multiple classification to nearest 3-digit NAICS (see https://usa.ipums.org/usa/volii/indtoindnaics18.shtml) 
data_july_2020$NAICS[data_july_2020$NAICS=="22S"]<-"221" 
data_july_2020$NAICS[data_july_2020$NAICS=="23"]<-"236"
data_july_2020$NAICS[data_july_2020$NAICS=="31M"]<-"315"
data_july_2020$NAICS[data_july_2020$NAICS=="33M"]<-"332"
data_july_2020$NAICS[data_july_2020$NAICS=="3MS"]<-"339"
data_july_2020$NAICS[data_july_2020$NAICS=="42S"]<-"423"
data_july_2020$NAICS[data_july_2020$NAICS=="4MS"]<-"453"
data_july_2020$NAICS[data_july_2020$NAICS=="52M"]<-"523"
data_july_2020$NAICS[data_july_2020$NAICS=="53M"]<-"532"
data_july_2020$NAICS[data_july_2020$NAICS=="55"]<-"551"
data_july_2020$NAICS[data_july_2020$NAICS=="92M"]<-"999"


data_july_2020$JULY_COUNT<-1

data_july_2020$WEIGHTED_JULY_COUNT<-data_july_2020$WTFINL*data_july_2020$JULY_COUNT
data_july_2020$WEIGHTED_JULY_EMPL_STAT<-data_july_2020$WTFINL*data_july_2020$EMPSTAT


#create industry-level dataset for July:
agg <- aggregate(data_july_2020[,c(which(colnames(data_july_2020)=="WEIGHTED_JULY_EMPL_STAT"),which(colnames(data_july_2020)=="WEIGHTED_JULY_COUNT"))], by=list(data_july_2020$NAICS), sum)
agg$july_unempl_rate<-1-agg$WEIGHTED_JULY_EMPL_STAT/agg$WEIGHTED_JULY_COUNT


#COMPUTE NOVEMBER UNEMPLOYMENT RATES#####
data_november_2020 <- subset(data2, YEAR == 2020 & MONTH==11)

#remove those NILF & with zero wage income:
data_november_2020<-subset(data_november_2020, EMPSTAT>=10 & EMPSTAT <=22)

#reformate employment status measurement (1=employed, 0=unemployed):
data_november_2020$EMPSTAT<-round(data_november_2020$EMPSTAT/10)
data_november_2020$EMPSTAT <- abs(data_november_2020$EMPSTAT-2)

#apply crosswalk (see https://usa.ipums.org/usa/volii/indtoindnaics18.shtml)
crosswalk <- read_excel("croswalk.xlsx")
crosswalk$IND <- as.numeric(crosswalk$IND)
data_november_2020<-merge(data_november_2020, crosswalk, by="IND")
data_november_2020$NAICS<- substr(data_november_2020$NAICS, 1,3)


#Bin split or multiple classification to nearest 3-digit NAICS (see https://usa.ipums.org/usa/volii/indtoindnaics18.shtml)
data_november_2020$NAICS[data_november_2020$NAICS=="22S"]<-"221"
data_november_2020$NAICS[data_november_2020$NAICS=="23"]<-"236"
data_november_2020$NAICS[data_november_2020$NAICS=="31M"]<-"315"
data_november_2020$NAICS[data_november_2020$NAICS=="33M"]<-"332"
data_november_2020$NAICS[data_november_2020$NAICS=="3MS"]<-"339"
data_november_2020$NAICS[data_november_2020$NAICS=="42S"]<-"423"
data_november_2020$NAICS[data_november_2020$NAICS=="4MS"]<-"453"
data_november_2020$NAICS[data_november_2020$NAICS=="52M"]<-"523"
data_november_2020$NAICS[data_november_2020$NAICS=="53M"]<-"532"
data_november_2020$NAICS[data_november_2020$NAICS=="55"]<-"551"
data_november_2020$NAICS[data_november_2020$NAICS=="92M"]<-"999"


data_november_2020$november_COUNT<-1

data_november_2020$WEIGHTED_november_COUNT<-data_november_2020$WTFINL*data_november_2020$november_COUNT
data_november_2020$WEIGHTED_november_EMPL_STAT<-data_november_2020$WTFINL*data_november_2020$EMPSTAT


#create industry-level dataset for november:
agg2 <- aggregate(data_november_2020[,c(which(colnames(data_november_2020)=="WEIGHTED_november_EMPL_STAT"),which(colnames(data_november_2020)=="WEIGHTED_november_COUNT"))], by=list(data_november_2020$NAICS), sum)
agg2$november_unempl_rate<-1-agg2$WEIGHTED_november_EMPL_STAT/agg2$WEIGHTED_november_COUNT










#merge july and november:
agg<-merge(agg, agg2, by="Group.1")

#ADD UNEMPLOYMENT RATES TO INTERSECTION PANEL:

#reformate employment status measurement (1=employed, 0=unemployed):
intersection$EMPSTAT <- abs(intersection$EMPSTAT-2)
intersection$APRIL_EMPL_STAT <- abs(intersection$APRIL_EMPL_STAT-2)



intersection<-merge(intersection, crosswalk, by="IND")
intersection$NAICS<- substr(intersection$NAICS, 1,3)


#Bin split or multiple classification to nearest 3-digit NAICS (see https://usa.ipums.org/usa/volii/indtoindnaics18.shtml) 
intersection$NAICS[intersection$NAICS=="22S"]<-"221"
intersection$NAICS[intersection$NAICS=="23"]<-"236"
intersection$NAICS[intersection$NAICS=="31M"]<-"315"
intersection$NAICS[intersection$NAICS=="33M"]<-"332"
intersection$NAICS[intersection$NAICS=="3MS"]<-"339"
intersection$NAICS[intersection$NAICS=="42S"]<-"423"
intersection$NAICS[intersection$NAICS=="4MS"]<-"453"
intersection$NAICS[intersection$NAICS=="52M"]<-"523"
intersection$NAICS[intersection$NAICS=="53M"]<-"532"
intersection$NAICS[intersection$NAICS=="55"]<-"551"
intersection$NAICS[intersection$NAICS=="92M"]<-"999"


intersection$APRIL_COUNT<-1
intersection$MARCH_COUNT<-1

intersection$MARCH_EMPL_STAT<-abs(intersection$MARCH_EMPL_STAT-2)
intersection$APRIL_EMPL_STAT<-abs(intersection$APRIL_EMPL_STAT-2)

intersection$WEIGHTED_MARCH_COUNT<-intersection$ASECWT*intersection$MARCH_COUNT
intersection$WEIGHTED_APRIL_COUNT<-intersection$ASECWT*intersection$APRIL_COUNT
intersection$WEIGHTED_MARCH_EMPL_STAT<-intersection$ASECWT*intersection$MARCH_EMPL_STAT
intersection$WEIGHTED_APRIL_EMPL_STAT<-intersection$ASECWT*intersection$APRIL_EMPL_STAT


agg_intersection <- aggregate(intersection[,c(which(colnames(intersection)=="WEIGHTED_APRIL_EMPL_STAT"),which(colnames(intersection)=="WEIGHTED_APRIL_COUNT"),which(colnames(intersection)=="WEIGHTED_MARCH_EMPL_STAT"),which(colnames(intersection)=="WEIGHTED_MARCH_COUNT"))], by=list(intersection$NAICS), sum)
agg_intersection$april_unempl_rate<-1-agg_intersection$WEIGHTED_APRIL_EMPL_STAT/agg_intersection$WEIGHTED_APRIL_COUNT
agg_intersection$march_unempl_rate<-1-agg_intersection$WEIGHTED_MARCH_EMPL_STAT/agg_intersection$WEIGHTED_MARCH_COUNT


agg_inc <- aggregate(intersection[,c(which(colnames(intersection)=="INCWAGE"))], by=list(intersection$NAICS), mean)


#create industry-level unemployment dataset:
ind_unempl<-merge(agg, agg_intersection, by="Group.1")
ind_unempl$avg_inc <- agg_inc[,2]
ind_unempl$NAICS<-as.factor(ind_unempl$Group.1)

ind_unempl$delta_aa_march_april_unempl <- ind_unempl$april_unempl_rate-ind_unempl$march_unempl_rate
ind_unempl$delta_april_july_unempl <- ind_unempl$july_unempl_rate-ind_unempl$april_unempl_rate

dat_long<-ind_unempl %>%
  dplyr::select(starts_with("delta") | starts_with("NAICS")|starts_with("avg_inc")) %>%
  gather("stat", "value",-c(NAICS,avg_inc))


#Plot unemployment rate by industry
ggplot(subset(dat_long, NAICS!=0 & NAICS != 114),
       aes(x = reorder(NAICS, avg_inc), y = value, fill=stat)) +
  geom_col()+
  xlab("NAICS industry")+
  ylab(expression(paste(Delta," Unemployment rate")))+
  scale_fill_manual(labels=c("March-April", "April-July"), values=c("red", "blue"))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.82),
    legend.title = element_blank(),
    panel.grid.major.y = element_blank() ,
    legend.text = element_blank(),
       # axis.title.y = element_blank(),
      #  axis.title.x = element_blank(),
        axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))



#Plot state-level unemployment change
agg_state<-aggregate(intersection[,c(which(colnames(intersection)=="WEIGHTED_APRIL_EMPL_STAT"),which(colnames(intersection)=="WEIGHTED_APRIL_COUNT"),which(colnames(intersection)=="WEIGHTED_MARCH_EMPL_STAT"),which(colnames(intersection)=="WEIGHTED_MARCH_COUNT"))], by=list(intersection$STATEFIP), FUN=sum )
agg_state$march_unempl_rate<-1-agg_state$WEIGHTED_MARCH_EMPL_STAT/agg_state$WEIGHTED_MARCH_COUNT  
agg_state$APRIL_unempl_rate<-1-agg_state$WEIGHTED_APRIL_EMPL_STAT/agg_state$WEIGHTED_APRIL_COUNT  
agg_state$delta_unempl_rate<-agg_state$APRIL_unempl_rate-agg_state$march_unempl_rate

agg_state$state<-fips_info(as.numeric(agg_state$Group.1))[,1]

ggplot(agg_state, aes(x=as.factor(state), y=delta_unempl_rate))+
  geom_col()

rm(agg, agg_intersection,data_july_2020,crosswalk,data2,data_april_2020, data_march_2020, data_ASEC_2020,ddi)





#CALCULATE DISTRIBUTION####

#step 1:perform simulation of recovery (ONLY RUN THIS ONCE)
intersection<-merge(intersection, ind_unempl[,c(which(colnames(ind_unempl)=="NAICS"),which(colnames(ind_unempl)=="july_unempl_rate"),which(colnames(ind_unempl)=="july_unempl_rate"),which(colnames(ind_unempl)=="april_unemp_rate"))], by="NAICS")

#set probability of EMPLOYMENT IN july here:
intersection$p<-0
intersection$p[intersection$APRIL_EMPL_STAT==0]<-0
intersection$p[intersection$APRIL_EMPL_STAT==1]<-1

intersection$p[intersection$APRIL_EMPL_STAT==0 & intersection$july_unempl_rate<intersection$april_unempl_rate]<-(intersection$april_unempl_rate[intersection$APRIL_EMPL_STAT==0 & intersection$july_unempl_rate<intersection$april_unempl_rate]-intersection$july_unempl_rate[intersection$APRIL_EMPL_STAT==0 & intersection$july_unempl_rate<intersection$april_unempl_rate])/intersection$april_unempl_rate[intersection$APRIL_EMPL_STAT==0 & intersection$july_unempl_rate<intersection$april_unempl_rate]
intersection$p[intersection$APRIL_EMPL_STAT==1 & intersection$july_unempl_rate>intersection$april_unempl_rate]<-(1-intersection$july_unempl_rate[intersection$APRIL_EMPL_STAT==1 & intersection$july_unempl_rate>intersection$april_unempl_rate])/(1 - intersection$april_unempl_rate[intersection$APRIL_EMPL_STAT==1 & intersection$july_unempl_rate>intersection$april_unempl_rate])


#calculate expected income:
intersection$expected_income_july_cares<- intersection$p*intersection$weekly_earnings + (1-intersection$p)*(intersection$benefits_amount+600)
intersection$expected_income_july_no_cares<- intersection$p*intersection$weekly_earnings + (1-intersection$p)*(intersection$benefits_amount)

#plot:
plot(density(intersection$expected_income_july_cares), xlim=c(0,6000), ylim=c(0,0.0015), col="blue",bty='n',
     xlab="",
     main="")
lines(density(intersection$expected_income_july_no_cares),col="black")
lines(density(intersection$expected_income_july_cares),col="black")

#compute Gini and median:
weighted.median(intersection$expected_income_july_cares, intersection$ASECWT)
weighted.median(intersection$expected_income_july_no_cares, intersection$ASECWT)

weighted.gini(intersection$expected_income_july_cares, intersection$ASECWT)
weighted.gini(intersection$expected_income_july_no_cares, intersection$ASECWT)


