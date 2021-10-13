

library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(tidyverse)
library(here)

## FQ code needs to be amended so that variable rr_fqr_pcnt is prioritised and used over all other variables,where available. this implies adapting 
## Philippes code (which was calculating % and sd from the numerator and denominator data). must calculate prev of FQ among RR as well


setwd("C:/Users/tosaso/OneDrive - World Health Organization/WHO_WORK/R_DATA_WHO/GLOBAL_TB_REPORT/GLOBAL_REPORT/GR2021/FQ_GR_2021")


# Create connection string
connection_string <- "driver={SQL Server}; server=ssdb231.who.int; database=TMEData; uid=TMEDATA_reader; pwd=Tmere@d3r"
# Connect to the database
require(RODBC)
ch <- odbcDriverConnect(connection_string)
# Load view into a dataframe
fq <- sqlFetch(ch, "view_DRS_for_estimation_sldst")
fix(fq)

#write.csv(fq, "C:\\Users\\tosaso\\OneDrive - World Health Organization\\Desktop\\Working On\\fq.csv", row.names=FALSE) 

nrow(fq)


## recode drs_record_type so that all levels are either survey or surveillance

fq$drs_record_type <- recode(fq$drs_record_type, "pre-2010 TME data collection" = "Surveillance")

##because the view is filtered for the past 15 years of data since 2020 GR, there is no need to filter for past 15 years here
##the filter in this view also ensures that all records have data for either mdr_dst_rlt OR rr_dst_rlt OR r_dst_rlt_fq


#get records with FQ data for both numerator AND denominator

fq2<- fq %>%
  filter(((mdr_dst_rlt>0) & !is.na(mdr_dr_fq)) | ((rr_dst_rlt>0) & !is.na(rr_dr_fq)) | ((rr_dst_rlt_fq>0) & (!is.na(rr_fqr)|!is.na(rr_fqr_pcnt))))%>% 
  droplevels()


nrow(fq2)

#for each country select the latest year with data


fq3<-fq2%>%select(-year,year) #year becomes the last column

fq4<-fq3 %>% group_by(country) %>% top_n(1,year)#for each country, this will rank by the last column (value) and return the top n=1 rows.

nrow(fq4)


fq6<-fq4 %>% select(country, iso3, year,drs_record_type, mdr_dst_rlt, mdr_dr_fq, rr_dst_rlt, rr_dr_fq, rr_dst_rlt_fq, rr_fqr_pcnt, rr_fqr, rr_fqr_pcnt_lo, rr_fqr_pcnt_hi)

#for records with rr_fqr_pcnt data, which is to be prioritised for use, calculate the sd, following the methods of Philippe (see end of script)

fq6<-fq6 %>% 
  mutate(my_sd = case_when(!is.na(rr_fqr_pcnt_lo) & !is.na(rr_fqr_pcnt_hi) & !is.na(rr_fqr_pcnt) ~ ((rr_fqr_pcnt_hi/100)-(rr_fqr_pcnt_lo/100)) /3.92))

#check that there are no duplicates in fq6

n_count<-fq6 %>% group_by(iso3) %>% tally() %>%
  rename(data_points=n)

fq6b<-merge(fq6,n_count,all.x=TRUE)

dup<-fq6b %>%
  filter(data_points > 1)%>% 
  droplevels()

fix(dup) # shows countries with >1 data point for the latest year of data

#if, for a given country, there are 2 sources of data for the latest year, we must only keep one. In principle, surveillance data overwrites survey data, but this must be
#manually checked each time the code is ran, based on context and based on discussions around the accuracy of the estimates obtained from each source.
#in the case of data for the 2021 Global TB report, 2 data points were available for Ethiopia: 2018 survey data plus 2018 surveillance data.
#first a sensitivity analysis was conducted to check how use of either source affects the global proportion of resistance to FQs. In this instance, using either surveillance or survey data
# had no influence on the global estimate (the final global percentage of resistance to FQs plus the 95% CIs were identical)
# surveillance data for ETH was prioritized for use in line with usual practice. This choice must however be considered on a case-by-case basis each year

#prioritize use of surveillance data in cases where both survey and surveillance data are available for the latest year of data:

fq6c<-fq6b %>%
  filter(data_points == 1 | (data_points > 1 & drs_record_type == "Surveillance"))%>% 
  droplevels()%>% 
  select(-c(data_points, drs_record_type))

##prioritise use of rr_dst_rlt_fq & rr_fqr over rr_dst_rlt & rr_dr_fq, and the later over use of mdr_dst_rlt & mdr_dr_fq (if data is available) to come up with a single 
##numerator and denominator to estimate FQ resistance

fq7<-fq6c %>% 
  mutate(fq_denominator = case_when(!is.na(rr_dst_rlt_fq) & !is.na(rr_fqr) ~ rr_dst_rlt_fq,
                                    !is.na(rr_dst_rlt) & !is.na(rr_dr_fq) ~ rr_dst_rlt, 
                                    TRUE ~ mdr_dst_rlt))

fq8<-fq7 %>% 
  mutate(fq_numerator = case_when(!is.na(rr_dst_rlt_fq) & !is.na(rr_fqr) ~ rr_fqr, 
                                  !is.na(rr_dst_rlt) & !is.na(rr_dr_fq) ~ rr_dr_fq, 
                                  TRUE ~ mdr_dr_fq))


#All data should now be weighted by e_inc_rr_num (most recent year)

# Create connection string
connection_string <- "driver={SQL Server}; server=ssdb231.who.int; database=TMEData; uid=TMEDATA_reader; pwd=Tmere@d3r"
# Connect to the database
require(RODBC)
ch <- odbcDriverConnect(connection_string)
# Load view into a dataframe
wt <- sqlFetch(ch, "view_TME_estimates_drtb")
fix(wt)
names(wt)

wt1<-wt %>% select(iso3, year, e_inc_rr_num)

max_year<-max(wt1$year)#get the latest year of data

wt2<- wt1 %>%
  filter(year==max_year) %>% 
  droplevels()

#check that there are no duplicates in wt2

wt3<-wt2 %>% select(iso3)
wt3<-unique(wt3)
nrow(wt2)
nrow(wt3) #no duplicates if nrows in wt2 = nrows in wt3


fq9<-merge(fq8,wt2,by="iso3",all=FALSE)

names(fq9)[names(fq9) == "year.x"] <- "year"

fq10<-fq9 %>% select(country,iso3, year, fq_denominator, fq_numerator, e_inc_rr_num, rr_fqr_pcnt, my_sd)%>% 
  mutate(rr_fqr_pcnt = case_when(!is.na(rr_fqr_pcnt) ~ rr_fqr_pcnt/100))


#####WRITE FINAL DATASET USED FOR THE ESTIMATION IN THE RIGHT FOLDER#################

write.csv(fq10, "C:\\Users\\tosaso\\OneDrive - World Health Organization\\WHO_WORK\\R_DATA_WHO\\GLOBAL_TB_REPORT\\GLOBAL_REPORT\\GR2021\\FQ_GR_2021\\fq_all_countries_with_data_GR2021.csv", row.names=FALSE) 
save(fq10, file = here(paste("fq_all_countries_with_data_GR2021_",Sys.Date(), ".rda")))

##############ESTIMATE GLOBAL WEIGHED PROPORTION



# returns Beta shape and scale params using the method of moments
get.beta <- function(ev, sd){
  # ev = expected value
  # sd = standard deviation
  stopifnot(ev>0 & ev<1)
  stopifnot(sd>0)
  
  S = (ev * (1 - ev) / sd^2) - 1
  if (S < 0) stop('Not distributed Beta: sd^2 >= ev*(1-ev)')
  
  a = S * ev
  b = S * (1 - ev)
  return(c(a = a, b = b))
}

# generate low and high bounds assuming Beta distribution
lohi <- function(ev, se){
  par <- get.beta(ev, se)
  lo <- qbeta(0.025, par[1], par[2])
  hi <- qbeta(0.975, par[1], par[2])
  return(c(lo=lo, hi=hi))
}



# calculate FQ global proportion 



fqw<-as.data.table(fq10)

setnames(fqw, c('country', 'iso3','year', 'm', 'x', 'w', 'my_p', 'my_sd'))
setkey(fqw, iso3)
out <- fqw[m>0,{tmp = binom.test(x, m);
list(fqw=tmp$estimate,
     fqw.sd=diff(tmp$conf.int)/3.92)}, by=.(iso3)]
fqw <- fqw[out]

##to prioritise rr_fqr_pcnt we run the following:

fqw2<-fqw %>%
  mutate(fqw.sd = case_when(!is.na(my_sd)~ my_sd,
                         TRUE ~ fqw.sd)) %>%
  mutate(fqw = case_when(!is.na(my_p)~ my_p,
                            TRUE ~ fqw))


(out <- fqw2[, .(weighted.mean(fqw, w), sqrt(sum(fqw.sd^2*w^2)/sum(w^2)))])
(lohi(out$V1, out$V2))

res1<-round((out <- fqw2[, .(weighted.mean(fqw, w), sqrt(sum(fqw.sd^2*w^2)/sum(w^2)))])*100,1)
res2<-as.data.frame(round((lohi(out$V1, out$V2))*100,1))

res3<-cbind (res1[,1],t(res2))

###NUMBER OF COUNTRIES IN FINAL COMPUTATION IS CALCULATED BY DOING nrow(fqw) AFTER RUNNING THE WHOLE CODE!!
###THIS EXCLUDES COUNTRIES WHERE DENOMINATOR IS ZERO. COUNT MUST BE CHECKED AGAINST ORIGINAL CSV CREATED FOR THIS (fq_2020_report_all_countries_with_data)

n_fqw<-as.data.frame(nrow(fqw))
  

## calculate how many of the countries in fqw are High TB burden or high MDR-TB burden:


#get the list of high TB or MDR TB burden countries

# Create connection string
connection_string <- "driver={SQL Server}; server=ssdb231.who.int; database=TMEData; uid=TMEDATA_reader; pwd=Tmere@d3r"
# Connect to the database
require(RODBC)
ch <- odbcDriverConnect(connection_string)
# Load view into a dataframe
HTB <- sqlFetch(ch, "view_country_group_membership")
fix(HTB)

table(HTB$group_description)

HTB1<- HTB %>%
  filter(group_description=="On 30 high MDR-TB burden countries list" | group_description=="On 30 high TB burden countries list") %>% 
  droplevels()

nrow(HTB1)##contains duplicate records
HTB2<-HTB1 %>% select(country, iso3)
HTB3<-unique(HTB2)
nrow(HTB3)##list of countries; up until 2020 there were 40 countries in the combined lists. Now there are 43.

countHB<-merge(HTB3,fqw,by="iso3",all=FALSE)


n_HB<-nrow(countHB)


res4<-cbind(res3,n_fqw,n_HB)%>%
rename(perc_FQ = V1)%>%
  rename(lo_CI = lo)%>%
  rename(hi_CI = hi)%>%
  rename(Count_countries = "nrow(fqw)")%>%
  rename(Count_High_TB_MDR_Burden_countries = "n_HB")

save(res4, file = here(paste("Global_Prop_FQ_Resistance_",Sys.Date(), ".rda")))
