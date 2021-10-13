# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch3-2-txt.rmd
# and ch3-2-figs.rmd
# Hazim Timimi, August 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load data packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(stringr)
library(dplyr)
library(tidyr)
library(here)


# Load the conf dataframe from Philippes gtb.rda file and drop all the others ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
load(here('report/data/gtb.rda'))
rm(list=setdiff(ls(), "conf"))


# Set the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2021


# Set datestamps of CSV files to use ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
csv_datestamp <- '2021-09-13'
csv_estimate_datestamp <- '2021-08-16'


# Load TB data (CSV,not rda because the latter are datatables ...) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Function to read in timestamped CSV file and to undo Philippe's conversion of underscores to dots
get_timestamped_csv <- function(csv_filename_stub, timestamp) {
  df <- read.csv(here(paste0('csv/', csv_filename_stub, '_', timestamp,'.csv')))
  names(df) <- gsub('[.]', '_', names(df))
  return(df)
}

notification <- get_timestamped_csv('tb', csv_datestamp)
data_collection <- get_timestamped_csv('datacoll', csv_datestamp)
strategy <- get_timestamped_csv('sty', csv_datestamp)
country_group_membership <- get_timestamped_csv('grpmbr', csv_datestamp)
TBHIV_for_aggregates <- get_timestamped_csv('agg', csv_datestamp)
dr_surveillance <- get_timestamped_csv('dr', csv_datestamp)

# Tweak dr_surveillance: remove records that aren't explicitly covering a whole country
dr_surveillance <- filter(dr_surveillance, all_areas_covered==1 | is.na(all_areas_covered))


# Fix lists of the three sets of 30 high burden countries (used to filter records for some figures)
hbc30 <- country_group_membership %>%
  filter(group_type == "g_hb_tb" & group_name == 1) %>%
  select(iso3)

hbmdr30 <- country_group_membership %>%
  filter(group_type == "g_hb_mdr" & group_name == 1) %>%
  select(iso3)

hbtbhiv30 <- country_group_membership %>%
  filter(group_type == "g_hb_tbhiv" & group_name == 1) %>%
  select(iso3)


# Get global and regional estimates directly from the files that get imported into the database
est_country  <- read.csv(here(paste0('csv/db/db_est_country_', csv_estimate_datestamp, '.csv')))
est_regional <- read.csv(here(paste0('csv/db/db_est_regional_est_', csv_estimate_datestamp, '.csv')))
est_global <- read.csv(here(paste0('csv/db/db_est_global_est_', csv_estimate_datestamp, '.csv')))


# Get Pete's aggregate incidence estimates by agre group and sex
load(here('disaggregation/dboutput/db_estimates_group.Rdata'))

# Create a set of WHO region short names to use in figures and tables
who_region_shortnames <- get_timestamped_csv('grp', csv_datestamp) %>%
  filter(group_type=="g_whoregion") %>%
  select(g_whoregion = group_name, entity= group_description) %>%
  # Remove the WHO bit at the beginning of regional names
  mutate(entity = gsub("(WHO )|(WHO/PAHO )", "", entity)) %>%
  # Change the order based on the bizarre method chosen by WHO Press ...
  mutate(entity = factor(entity,
                         levels = c("African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))




# Add simple function to convert NA to zero
NZ <- function(x) {
  ifelse(is.na(x),
         0,
         x)
}



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: bac confirmation ----
# Determine numerators and denominators for bacteriological confirmation and
# then use this dataframe for figures 3.2.1, 3.2.2 and 3.3.3
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


bacconf_data <- notification %>%
  filter(year >= 2000) %>%
  select(iso3,
         country,
         year,
         g_whoregion,
         # old variables pre-2013
         new_sp,
         new_sn,
         new_su,
         # new variables
         new_labconf, new_clindx,
         ret_rel_labconf, ret_rel_clindx) %>%

  #calculate % of pulmonary cases with bac confirmation
  rowwise() %>%
  # a bit tricky for years before 2013, so do for new only by smear only
  mutate(bacconf_pct_numerator = ifelse(year < 2013 & g_whoregion != 'EUR',
                                        # old variables, do for new only outside EUR
                                        new_sp,
                                        # new variables
                                        sum(c_across(contains("labconf")), na.rm = TRUE)),
         bacconf_pct_denominator = ifelse(year < 2013 & g_whoregion != 'EUR',
                                          # old variables, do for new only outside EUR
                                          sum(c_across(new_sp:new_su), na.rm = TRUE),
                                          # new variables
                                          sum(c_across(new_labconf:ret_rel_clindx), na.rm = TRUE))) %>%

  # Adjust calculation for EUR pre-2013 (applies to years 2002 - 2012)
  mutate(bacconf_pct_numerator = ifelse(between(year, 2002, 2012) & g_whoregion == 'EUR',
                                        # old variables, but using new_labconf
                                        new_labconf,
                                        # otherwise keep calculation from previous step
                                        bacconf_pct_numerator),
         bacconf_pct_denominator = ifelse(between(year, 2002, 2012) & g_whoregion == 'EUR',
                                          # old variables
                                          sum(c_across(new_sp:new_su), na.rm = TRUE),
                                          # otherwise keep calculation from previous step
                                          bacconf_pct_denominator)) %>%

  # Finally deal with EUR 2000 and 2001 numerator
  mutate(bacconf_pct_numerator = ifelse(between(year, 2000, 2001) & g_whoregion == 'EUR',
                                        # old variables
                                        new_sp,
                                        # otherwise keep calculation from previous step
                                        bacconf_pct_numerator),
         bacconf_pct_denominator = ifelse(between(year, 2000, 2001) & g_whoregion == 'EUR',
                                          # old variables
                                          sum(c_across(new_sp:new_su), na.rm = TRUE),
                                          # otherwise keep calculation from previous step
                                          bacconf_pct_denominator)) %>%

  ungroup() %>%

  # reduce to needed variables
  select(country,
         iso3,
         year,
         g_whoregion,
         bacconf_pct_numerator,
         bacconf_pct_denominator)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.1 ----
# (Panel plot of TB cases with bacteriological confirmation by WHO region and globally since 2000)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Calculate aggregates
bacconf_data_regional <- bacconf_data %>%
  group_by(year, g_whoregion) %>%
  summarise(across(bacconf_pct_numerator:bacconf_pct_denominator, sum, na.rm = TRUE)) %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion) %>%
  ungroup()

bacconf_data_global <- bacconf_data %>%
  group_by(year) %>%
  summarise(across(bacconf_pct_numerator:bacconf_pct_denominator, sum, na.rm = TRUE)) %>%
  mutate(entity = 'Global')

# Add global to the regional aggregates
f3.2.1_data <- rbind(bacconf_data_regional, bacconf_data_global) %>%

  # Calculate the percentages
  mutate(bacconf_pct = bacconf_pct_numerator * 100 / bacconf_pct_denominator) %>%

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))



# summary dataset for quoting numbers in the text on bac confirmation in pulmonary TB
f3.2.1_txt <- filter(notification, year == (report_year - 1)) %>%
  select(c_newinc,
         new_labconf, new_clindx,
         ret_rel_labconf, ret_rel_clindx) %>%
  summarise(across(c_newinc:ret_rel_clindx, sum, na.rm=TRUE)) %>%
  mutate(pulm = new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx) %>%
  mutate(pulm_pct = pulm * 100/ c_newinc) %>%
  select(c_newinc, pulm, pulm_pct)

# Calculate global % bac conf for the last two years and percent change and add to the summary
f3.2.1_txt <- filter(f3.2.1_data, entity == "Global" & year >= report_year-2) %>%
  select(year, bacconf_pct) %>%
  pivot_wider(names_from = year,
              names_prefix = "bc_pct_",
              values_from = bacconf_pct) %>%
  cbind(f3.2.1_txt)

# Add regional max and min values for 2020
f3.2.1_txt <- filter(f3.2.1_data, year==report_year-1 & entity %in% c("Region of the Americas", "Western Pacific Region")) %>%
  select(entity, bacconf_pct) %>%
  pivot_wider(names_from = entity,
              names_prefix = "bc_pct_",
              values_from = bacconf_pct) %>%
  # handle spaces in column names
  select(bc_pct_AMR = `bc_pct_Region of the Americas`,
         bc_pct_WPR = `bc_pct_Western Pacific Region`) %>%
  cbind(f3.2.1_txt)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.2 ----
# (Panel plot of TB cases with bacteriological confirmation for 30 countries since 2000)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.2.2_data <- bacconf_data %>%
  inner_join(hbc30, by = "iso3") %>%

  # Calculate the percentages
  mutate(bacconf_pct = ifelse(bacconf_pct_denominator > 0,
                              bacconf_pct_numerator * 100 / bacconf_pct_denominator,
                              NA)) %>%

  # get rid of extra variables
  select(country,
         year,
         bacconf_pct)

# summary datasets for quoting numbers in the text
f3.2.2_txt_MOZ <- filter(f3.2.2_data, year==report_year-1 & country=="Mozambique")
f3.2.2_txt_list <- filter(f3.2.2_data, year==report_year-1 & bacconf_pct > 75) %>%
  select(country)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.3 ----
# (World map showing percent of TB cases with bacteriological confirmation)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.2.3_data <- filter(bacconf_data, year>=report_year-2) %>%

  # Calculate the percentages
  mutate(bacconf_pct = ifelse(bacconf_pct_denominator > 0,
                              bacconf_pct_numerator * 100 / bacconf_pct_denominator,
                              NA)) %>%

  # Assign the categories for the map
  mutate(var = cut(bacconf_pct,
                   c(0, 50, 65, 80, Inf),
                   c('0-49', '50-64', '65-79', '\u226580'),
                   right=FALSE))

# Find the countries with empty data for latest year and see if there are data for the previous year
bacconf_prev_year_data <- f3.2.3_data %>%
  filter(year == report_year - 1 & is.na(bacconf_pct)) %>%
  select(iso3) %>%
  inner_join(filter(f3.2.3_data, year == report_year - 2), by = "iso3") %>%
  filter(!is.na(bacconf_pct))

# Now combine into one data frame, with previous data used if latest year's data are not available
f3.2.3_data <- f3.2.3_data %>%
  filter(year == report_year - 1) %>%
  anti_join(bacconf_prev_year_data, by= "iso3") %>%
  rbind(bacconf_prev_year_data)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.4 ----
# (World map showing percent of TB cases tested with rapid diagnostics)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Just uses the conf dataframe
# create summary for the text
f3.2.4_txt <- filter(conf, year==report_year-1 & g.income=="HIC") %>%
  mutate(median_HIC = conf.med * 100) %>%
  select(median_HIC)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.5 ----
# (World map showing percent of TB cases tested with rapid diagnostics)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.2.5_data <- notification %>%
  filter(year  >= report_year - 2) %>%
  select(iso3,
         country,
         year,
         c_newinc,
         rdx_data_available,
         newinc_rdx,
         rdxsurvey_newinc,
         rdxsurvey_newinc_rdx) %>%

  # calculate the percentage for each country depending on data availability
  mutate(wrd_pcnt_nu = ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                              newinc_rdx,
                              ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                     rdxsurvey_newinc_rdx,
                                     NA)),
         wrd_pcnt_de = ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                              c_newinc,
                              ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                     rdxsurvey_newinc,
                                     NA)),
         wrd_pcnt =ifelse(rdx_data_available == 60 & NZ(c_newinc) > 0,
                          newinc_rdx * 100 / c_newinc,
                          ifelse(rdx_data_available == 61 & NZ(rdxsurvey_newinc) > 0,
                                 rdxsurvey_newinc_rdx * 100 / rdxsurvey_newinc,
                                 NA))) %>%

  # Assign the categories for the map
  mutate(var = cut(wrd_pcnt,
                   c(0, 25, 50, 75, 90, Inf),
                   c('0-24', '25-49', '50-75', '76-90','\u226590'),
                   right=FALSE)) %>%

  # get rid of extra variables
  select(country,
         iso3,
         year,
         wrd_pcnt,
         var)


# Find the countries with empty data for latest year and see if there are data for the previous year
wrd_prev_year_data <- f3.2.5_data %>%
  filter(year == report_year - 1 & is.na(wrd_pcnt)) %>%
  select(iso3) %>%
  inner_join(filter(f3.2.5_data, year == report_year - 2), by = "iso3") %>%
  filter(!is.na(wrd_pcnt))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f3.2.5_data <- f3.2.5_data %>%
  filter(year == report_year - 1) %>%
  anti_join(wrd_prev_year_data, by= "iso3") %>%
  rbind(wrd_prev_year_data)


# summary dataset for quoting numbers in the text
f3.2.5_txt <- filter(notification, year  >= report_year - 2) %>%
  select(year,
         c_newinc,
         newinc_rdx) %>%
  group_by(year) %>%
  summarise(across(c_newinc:newinc_rdx, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(wrd_pct = newinc_rdx * 100/c_newinc) %>%
  select(-c_newinc) %>%
  pivot_wider(names_from = year,
              values_from = c(newinc_rdx, wrd_pct))

# Add info about testing at least half of new cases in the high burden countries
f3.2.5_txt <- filter(notification,
                     year  >= report_year - 2 &
                       newinc_rdx >= 0.5 * c_newinc &
                       (iso3 %in% hbc30$iso3 | iso3 %in% hbtbhiv30$iso3 | iso3 %in% hbmdr30$iso3)) %>%
  select(year, country) %>%
  group_by(year) %>%
  summarise(hbcs = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year,
              names_prefix = "hbcs_",
              values_from = hbcs) %>%
  cbind(f3.2.5_txt)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.6 ----
# (Panel plot of TB cases with known HIV status by WHO region and globally since 2004)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Calculate regional aggregates
f3.2.6_data <- TBHIV_for_aggregates %>%
  filter(year >= 2004) %>%
  select(g_whoregion,
         year,
         hivtest_pct_numerator,
         hivtest_pct_denominator) %>%
  group_by(year, g_whoregion) %>%
  summarise(across(hivtest_pct_numerator:hivtest_pct_denominator, sum, na.rm = TRUE)) %>%
  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion) %>%
  ungroup()

# Calculate Global aggregates
hivstatus_global <- TBHIV_for_aggregates %>%
  filter(year >= 2004) %>%
  select(year,
         hivtest_pct_numerator,
         hivtest_pct_denominator) %>%
  group_by(year) %>%
  summarise(across(hivtest_pct_numerator:hivtest_pct_denominator, sum, na.rm = TRUE)) %>%
  mutate(entity = "Global")

# COmbine regional with global
f3.2.6_data <- rbind(f3.2.6_data, hivstatus_global) %>%

  # Calculate % with known HIV status
  mutate(hivstatus_pct = hivtest_pct_numerator * 100 / hivtest_pct_denominator) %>%

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))

# summary dataset for quoting numbers in the text
f3.2.6_txt <- filter(f3.2.6_data, year>=report_year-2 & entity %in% c("African Region", "European Region", "Global")) %>%
  select(year, entity, hivstatus_pct) %>%
  pivot_wider(names_from = c(entity, year),
              values_from = hivstatus_pct) %>%
  # handle spaces in column names
  mutate(AFR_2020 = `African Region_2020`,
         EUR_2020 = `European Region_2020`)

# Add the numbers that are HIV-positive
f3.2.6_txt <- filter(TBHIV_for_aggregates, year == report_year-1) %>%
  summarise(across(c(hivtest_pos_pct_numerator, hivtest_pos_pct_denominator), sum, na.rm = TRUE)) %>%
  mutate(hivtest_pos_pct = hivtest_pos_pct_numerator * 100 / hivtest_pos_pct_denominator) %>%
  cbind(f3.2.6_txt)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.7 ----
# (World map showing percent of TB cases with known HIV status)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.2.7_data <- TBHIV_for_aggregates %>%
  filter(year >= report_year - 2) %>%
  select(iso3,
         country,
         year,
         hivtest_pct_denominator,
         hivtest_pct_numerator) %>%

  # Calculate % with known HIV status
  mutate(hivstatus_pct = ifelse(hivtest_pct_denominator > 0,
                                hivtest_pct_numerator * 100 / hivtest_pct_denominator,
                                NA))  %>%

  # Assign the categories for the map
  mutate(var = cut(hivstatus_pct,
                   c(0, 50, 76, 90, Inf),
                   c('0-49', '50-75', '76-89', "\u226590"),
                   right=FALSE)) %>%

  # get rid of extra variables
  select(country,
         iso3,
         year,
         hivstatus_pct,
         var)

# Find the countries with empty data for latest year and see if there are data for the previous year
hivstatus_prev_year_data <- f3.2.7_data %>%
  filter(year == report_year - 1 & is.na(hivstatus_pct)) %>%
  select(iso3) %>%
  inner_join(filter(f3.2.7_data, year == report_year - 2)) %>%
  filter(!is.na(hivstatus_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f3.2.7_data <- f3.2.7_data %>%
  filter(year == report_year - 1) %>%
  anti_join(hivstatus_prev_year_data, by= "iso3") %>%
  rbind(hivstatus_prev_year_data)

# Summary data for text
f3.2.7_txt <- filter(f3.2.7_data, hivstatus_pct >= 90) %>%
  summarise(over_90 = n())


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.8 ----
# (Panel plot of TB cases tested for susceptibility to rifampicin by WHO region and globally since 2009)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The calculations here are a bit messy for years prior to 2018. It is much simpler starting in 2018 when
# we switched to using routine DR surveillance records only. Prior to that we mixed and matched between
# that and the DR-TB detection section of the data collection form. Keeping this for consistency with
# previously published reports

# 1. Get DR-TB detection data
dst_notif_data <- notification %>%
  filter(year >= 2009) %>%
  select(year,
         iso3,
         g_whoregion,
         new_labconf,
         c_ret,
         rdst_new,
         rdst_ret) %>%

  rowwise() %>%
  mutate(

    # numerator
    dst_notif_num = ifelse(is.na(rdst_new) & is.na(rdst_ret),
                           NA,
                           sum(c_across(rdst_new:rdst_ret), na.rm = TRUE)),

    # denominator is a bit of a fudge: new_labconf + c_ret
    dst_notif_denom = ifelse(is.na(new_labconf) & is.na(c_ret),
                             NA,
                             sum(c_across(new_labconf:c_ret), na.rm = TRUE))) %>%
  ungroup()


# 2. Get routine DR surveillance data
# (numerator and denominator variables are different according to the year, but
#  don't use 2015, 2016 numerator data)
dst_drs_data <- dr_surveillance %>%
  filter(year >= 2009) %>%
  select(year,
         iso3,
         dst_rlt_new,
         dst_rlt_ret,
         pulm_labconf_new,
         pulm_labconf_ret,
         r_rlt_new,
         r_rlt_ret) %>%

  rowwise() %>%
  mutate(
    # numerator
    dst_drs_num = ifelse(year < 2015,
                         ifelse(is.na(dst_rlt_new) & is.na(dst_rlt_ret),
                                NA,
                                sum(c_across(dst_rlt_new:dst_rlt_ret), na.rm = TRUE)),
                         ifelse(year >= 2017,
                                sum(c_across(r_rlt_new:r_rlt_ret), na.rm = TRUE),
                                NA)),
    # denominator
    dst_drs_denom = ifelse(year >= 2017,
                           sum(c_across(pulm_labconf_new:pulm_labconf_ret), na.rm = TRUE),
                            NA)
  ) %>%
  ungroup()


# Link the two data sets

dst_data <- dst_notif_data %>%
  left_join(dst_drs_data, by = c("year", "iso3")) %>%

  # To calculate the percentage DST coverage we need to identify the greater of the two numerators
  # Note the exception made for South Africa in 2017


  mutate(
    dst_num = ifelse(year == 2017 & iso3 == "ZAF",
                     dst_notif_num,
                     ifelse(year >= 2017,
                            dst_drs_num,
                           ifelse(NZ(dst_drs_num) >= NZ(dst_notif_num),
                                  dst_drs_num,
                                  dst_notif_num))),

    dst_denom = ifelse(year == 2017 & iso3 == "ZAF",
                       dst_notif_denom,
                       ifelse(year >= 2017,
                              dst_drs_denom,
                              dst_notif_denom))) %>%

  # Set numerator to NA if the denominator is NA for a country-year
  mutate(dst_num = ifelse(is.na(dst_denom), NA, dst_num)) %>%

  # Drop unwanted variables
  select(iso3,
         year,
         g_whoregion,
         dst_num,
         dst_denom) %>%

  # Drop rows with empty numerators and denominator
  filter(!is.na(dst_num) & !is.na(dst_denom))


f3.2.8_data <- dst_data %>%
  group_by(g_whoregion, year) %>%
  summarise(across(dst_num:dst_denom, sum, na.rm = TRUE)) %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  ungroup() %>%
  select(-g_whoregion)

dst_global <- dst_data %>%
  group_by(year) %>%
  summarise(across(dst_num:dst_denom, sum, na.rm = TRUE)) %>%
  mutate(entity = "Global") %>%
  ungroup()

# Phew! Bring it all together now
# COmbine regional with global
f3.2.8_data <- rbind(f3.2.8_data, dst_global) %>%

  # Calculate % tested for rifampicin resistance
  mutate(dst_pcnt = dst_num * 100 / dst_denom) %>%

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))


# summary data for text
f3.2.8_txt <- filter(f3.2.8_data, year >= report_year-3 & entity %in% c("European Region", "Global")) %>%
  select(year, entity, dst_pcnt) %>%
  pivot_wider(names_from = c(entity, year),
              names_prefix = "dst_pct_",
              values_from = dst_pcnt) %>%
  # Handle spaces in variable name
  mutate(dst_pct_EUR_2020 = `dst_pct_European Region_2020`)

# Add numbers of drug-resistant cases detected
f3.2.8_txt <- filter(notification, year >= report_year - 2) %>%
  select(year,
         c_newinc,
         conf_rr_nfqr,
         conf_rr_fqr,
         conf_rrmdr) %>%
  group_by(year) %>%
  summarise(across(c_newinc:conf_rrmdr, sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(dr_tb = conf_rr_nfqr + conf_rr_fqr + conf_rrmdr) %>%
  select(-conf_rrmdr) %>%
  pivot_wider(names_from = year,
              values_from = c(c_newinc,
                              conf_rr_nfqr,
                              conf_rr_fqr,
                              dr_tb)) %>%
  select(-conf_rr_nfqr_2019, -conf_rr_fqr_2019) %>%
  mutate(dr_tb_change_pct = abs(dr_tb_2020 - dr_tb_2019) * 100 / dr_tb_2019,
         c_newinc_change_pct= abs(c_newinc_2020 - c_newinc_2019) * 100 / c_newinc_2019 ) %>%
  select(-c_newinc_2020, -c_newinc_2019) %>%
  cbind(f3.2.8_txt)






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.9 ----
# (World map showing percent of TB cases tested for susceptibility to rifampicin)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.2.9_data <- dr_surveillance %>%
  filter(year >= report_year - 2) %>%
  select(year,
         country,
         iso3,
         pulm_labconf_new,
         pulm_labconf_ret,
         r_rlt_new,
         r_rlt_ret) %>%

  # Calculate coverage of DST percentages
  mutate(
    dst_pct = ifelse((NZ(pulm_labconf_new) + NZ(pulm_labconf_ret)) == 0 |
                       is.na(r_rlt_new) & is.na(r_rlt_ret), NA,
                     (NZ(r_rlt_new) + NZ(r_rlt_ret)) * 100 /
                       (NZ(pulm_labconf_new) + NZ(pulm_labconf_ret)))
  ) %>%

  # Assign the categories for the map
  mutate(var = cut(dst_pct,
                   c(0, 20, 50, 80, Inf),
                   c('0-19.9', '20-49.9', '50-79.9', '\u226580'),
                   right=FALSE)) %>%

  # get rid of extra variables
  select(country,
         iso3,
         year,
         dst_pct,
         var)


# Find the countries with empty data for latest year and see if there are data for the previous year
dst_prev_year_data <- f3.2.9_data %>%
  filter(year == report_year - 1 & is.na(dst_pct)) %>%
  select(iso3) %>%
  inner_join(filter(f3.2.9_data, year == report_year - 2), by = "iso3") %>%
  filter(!is.na(dst_pct))

# Now combine into one dataframe, with previous data used if latest year's data are not available
f3.2.9_data <- f3.2.9_data %>%
  filter(year == report_year - 1) %>%
  anti_join(dst_prev_year_data, by= "iso3") %>%
  rbind(dst_prev_year_data)


# summary numbers for the text
f3.2.9_txt <- filter(f3.2.9_data, dst_pct >= 80 & iso3 %in% hbmdr30$iso3) %>%
  arrange(country) %>%
  select(country)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.10 ----
# (World map showing percent of MDR/RR-TB cases tested for susceptibility to fluoroquinolones)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.2.10_data <- dr_surveillance %>%
  filter(year >= report_year - 1) %>%
  select(iso3,
         country,
         year,
         rr_new,
         rr_ret,
         rr_dst_rlt_fq) %>%

  # Calculate percentage RR cases with 2nd line DST
  mutate(fqdst_pct = ifelse( (NZ(rr_new) + NZ(rr_ret)) > 0,
                              rr_dst_rlt_fq * 100 / (NZ(rr_new) + NZ(rr_ret)),
                              NA)) %>%

  # Assign the categories for the map
  mutate(var = cut(fqdst_pct,
                   c(0, 20, 50, 80, Inf),
                   c('0-19.9', '20-49.9', '50-79.9', '\u226580'),
                   right=FALSE)) %>%

  # get rid of extra variables
  select(country,
         iso3,
         year,
         fqdst_pct,
         var)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.2.11 ----
# (Panel plot of RR-TB cases tested for susceptibility to fluoroquinolones by WHO region and globally since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.2.11_data <- dr_surveillance %>%
  filter(year >= 2015) %>%
  select(iso3,
         g_whoregion,
         year,
         # denominator changed in 2017 from mdr to rr
         mdr_new,
         mdr_ret,
         xpert_dr_r_new,
         xpert_dr_r_ret,
         rr_new,
         rr_ret,
         # numerator changed in 2017 from mdr to rr and in 2019 from sld to fq
         mdr_dst_rlt,
         rr_dst_rlt,
         rr_dst_rlt_fq) %>%

  group_by(year, g_whoregion) %>%
  summarise(across(mdr_new:rr_dst_rlt_fq, sum, na.rm = TRUE)) %>%
  ungroup() %>%

  # Calculate the numerators and denominators depending on the year
  mutate(fqdst_pct_denominator = ifelse(year < 2017,
                                        mdr_new + mdr_ret + xpert_dr_r_new + xpert_dr_r_ret,
                                        NA),
         fqdst_pct_numerator = ifelse(year < 2017,
                                      mdr_dst_rlt,
                                      NA)) %>%

  mutate(fqdst_pct_denominator = ifelse(year >= 2017,
                                        rr_new + rr_ret,
                                        fqdst_pct_denominator),
         fqdst_pct_numerator = ifelse(year %in% c(2017, 2018),
                                      rr_dst_rlt,
                                      fqdst_pct_numerator)) %>%

  mutate(fqdst_pct_numerator = ifelse(year >= 2019,
                                      rr_dst_rlt_fq,
                                      fqdst_pct_numerator)) %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%

  # get rid of extra variables
  select(entity,
         year,
         fqdst_pct_numerator,
         fqdst_pct_denominator)

# Calculate global aggregaes
fqdst_global <- f3.2.11_data %>%
  group_by(year) %>%
  summarise(across(fqdst_pct_numerator:fqdst_pct_denominator, sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(entity = 'Global')

# Add global to the regional aggregates
f3.2.11_data <- rbind(f3.2.11_data, fqdst_global) %>%

  # Calculate the percentages
  mutate(fqdst_pct = fqdst_pct_numerator * 100 / fqdst_pct_denominator) %>%

  # Change the order of the entities
  mutate(entity = factor(entity,
                         levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region")))






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Dropped the part of saving to a .rda file. Instead, the
# .rmd files will source this script to load the data into the
# working environment
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -




