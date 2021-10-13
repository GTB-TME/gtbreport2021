# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch3-4-txt.rmd
# and ch3-4-figs.rmd
# Hazim Timimi, August 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load data packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(stringr)
library(dplyr)
library(tidyr)
library(here)


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
outcomes  <- get_timestamped_csv('tx', csv_datestamp)

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
#est_dr_country <- read.csv(here(paste0('csv/db/db_dr_country_', csv_estimate_datestamp, '.csv')))
#est_dr_group <- read.csv(here(paste0('csv/db/db_dr_group_', csv_estimate_datestamp, '.csv')))

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

# Function to calculate outcome percentages for plotting as stacked bars ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

calculate_outcomes_pct <- function(df, prefix_string) {

  # Simplify outcome variable names so can calculate %
  # Remove the prefix) so data frame has columns called coh, succ, fail, died, lost and c_neval

  names(df) <- str_replace(names(df), prefix_string, "")

  df <- mutate(df,
               `Treatment success` = ifelse(NZ(coh) > 0,
                                            succ * 100 / coh,
                                            NA),
               Failure = ifelse(NZ(coh) > 0,
                                fail * 100 / coh,
                                NA),
               Died = ifelse(NZ(coh) > 0,
                             died * 100 / coh,
                             NA),
               `Lost to follow-up` = ifelse(NZ(coh) > 0,
                                            lost * 100 / coh,
                                            NA),
               `Not evaluated` = ifelse(NZ(coh) > 0,
                                        c_neval * 100 / coh,
                                        NA))

  return(df)

}



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.1 (DELETED but retailing code for stats in text)  ----
# (Line chart of MDR/RR-TB cases detected, MDR/RR-TB put on treatment since 2009 and estimated incidence in the latest year)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.4.1_old_data <- filter(notification, year >= 2010) %>%
  select(year,
         rapid_dx_dr_r,
         conf_mdr,
         conf_rrmdr,
         conf_rr_nfqr,
         conf_rr_fqr,
         conf_mdr_tx,
         unconf_mdr_tx,
         conf_rrmdr_tx,
         unconf_rrmdr_tx,
         conf_rr_nfqr_tx,
         unconf_rr_nfqr_tx,
         conf_rr_fqr_tx) %>%

  group_by(year) %>%
  summarise(across(rapid_dx_dr_r:conf_rr_fqr_tx, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  rowwise() %>%
  # Derive total number detected and total enrolled on treatment
  mutate(rr_detected = ifelse(year < 2014,
                              rapid_dx_dr_r + conf_mdr,
                              # the next three are mutually exclusive so can be added
                              conf_rrmdr + conf_rr_nfqr + conf_rr_fqr),
         # treatment variables are in mutually exclusive sets so again can be added
         rr_treated = sum(across(conf_mdr_tx:conf_rr_fqr_tx), na.rm = TRUE)) %>%
  ungroup() %>%

  # drop unneeded variables
  select(year,
         rr_detected,
         rr_treated)

# rr_inc_global <- filter(est_dr_group, year==report_year-1, group_type=="global") %>%
#   select(year,
#          e_inc_rr_num,
#          e_inc_rr_num_lo,
#          e_inc_rr_num_hi)
#
# # Merge the two for the data to be plotted
# f3.4.1_data <- f3.4.1_data %>%
#   left_join(rr_inc_global, by = "year")

# Summary for section text
f3.4.1_old_txt <- filter(f3.4.1_old_data, year >= report_year-2) %>%
  select(year, rr_treated) %>%
  pivot_wider(names_from = year,
              names_prefix = "rr_treated_",
              values_from = rr_treated) %>%
  # Calculate percent change
  mutate(rr_treat_change_pct = abs(rr_treated_2020 - rr_treated_2019) * 100 / rr_treated_2019)

# # Add estimated incidence 2020
# f3.4.1_txt <- filter(f3.4.1_data, year >= report_year-1) %>%
#   select(starts_with("e_inc_rr")) %>%
#   cbind(f3.4.1_txt)
#
# # Clean up
# rm(rr_inc_global)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.2 (DELETED) ----
# (Forest plot of TB treatment coverage for MDR/RR-TB in 30 countries, regionally and globally)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# # A. Countries
# # - - - - - - - -
# coverage_inc_country <- filter(est_dr_country, year==report_year-1) %>%
#   select(year,
#          iso3,
#          e_inc_rr_num,
#          e_inc_rr_num_lo,
#          e_inc_rr_num_hi) %>%
#
#   # restrict to high burden countries
#   inner_join(hbmdr30, by = "iso3")
#
# coverage_country <- filter(notification, year==report_year-1) %>%
#   select(entity = country,
#          iso3,
#          unconf_rr_nfqr_tx,
#          conf_rr_nfqr_tx,
#          conf_rr_fqr_tx)  %>%
#   mutate(rr_tx = ifelse(is.na(unconf_rr_nfqr_tx) & is.na(conf_rr_nfqr_tx) & is.na(conf_rr_fqr_tx),
#                         NA,
#                         NZ(unconf_rr_nfqr_tx) + NZ(conf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx))) %>%
#   inner_join(coverage_inc_country, by = "iso3") %>%
#   select(-iso3) %>%
#   mutate(c_rr_coverage = rr_tx * 100 / e_inc_rr_num,
#          c_rr_coverage_lo = rr_tx * 100  / e_inc_rr_num_hi,
#          c_rr_coverage_hi = rr_tx * 100  / e_inc_rr_num_lo,
#          # highlight countries with no data
#          entity = ifelse(is.na(rr_tx), paste0(entity, "*"), entity )) %>%
#   select(entity,
#          c_rr_coverage,
#          c_rr_coverage_lo,
#          c_rr_coverage_hi) %>%
#   arrange(desc(c_rr_coverage))
#
# # B. Regions
# # - - - - - - - -
# coverage_inc_region <- filter(est_dr_group, year==report_year-1 & group_type=="g_whoregion") %>%
#   select(g_whoregion = group_name,
#          e_inc_rr_num,
#          e_inc_rr_num_lo,
#          e_inc_rr_num_hi)
#
# coverage_region <- filter(notification, year==report_year-1) %>%
#   select(g_whoregion,
#          unconf_rr_nfqr_tx,
#          conf_rr_nfqr_tx,
#          conf_rr_fqr_tx)  %>%
#   # calculate regional aggregates
#   group_by(g_whoregion) %>%
#   summarise(across(unconf_rr_nfqr_tx:conf_rr_fqr_tx, sum, na.rm=TRUE)) %>%
#   ungroup() %>%
#   mutate(rr_tx = unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx) %>%
#
#   # merge with incidence estimates
#   inner_join(coverage_inc_region, by = "g_whoregion") %>%
#
#   # Calculate coverage
#   mutate(c_rr_coverage = rr_tx * 100 / e_inc_rr_num,
#          c_rr_coverage_lo = rr_tx * 100  / e_inc_rr_num_hi,
#          c_rr_coverage_hi = rr_tx * 100  / e_inc_rr_num_lo) %>%
#
#   # merge with regional names and simplify to match structure of country table
#   inner_join(who_region_shortnames, by = "g_whoregion") %>%
#   select(entity,
#          c_rr_coverage,
#          c_rr_coverage_lo,
#          c_rr_coverage_hi) %>%
#   arrange(desc(c_rr_coverage))
#
# # C. Global
# # - - - - - - - -
# coverage_inc_global <- filter(est_dr_group, year==report_year-1 & group_type=="global") %>%
#   select(e_inc_rr_num,
#          e_inc_rr_num_lo,
#          e_inc_rr_num_hi) %>%
#   mutate(entity="Global")
#
# coverage_global <- filter(notification, year==report_year-1) %>%
#   select(unconf_rr_nfqr_tx,
#          conf_rr_nfqr_tx,
#          conf_rr_fqr_tx)  %>%
#   # calculate global aggregate
#   summarise(across(unconf_rr_nfqr_tx:conf_rr_fqr_tx, sum, na.rm=TRUE)) %>%
#   mutate(rr_tx = unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx) %>%
#   mutate(entity="Global") %>%
#
#   inner_join(coverage_inc_global, by="entity") %>%
#
#   # Calculate coverage
#   mutate(c_rr_coverage = rr_tx * 100 / e_inc_rr_num,
#          c_rr_coverage_lo = rr_tx * 100  / e_inc_rr_num_hi,
#          c_rr_coverage_hi = rr_tx * 100  / e_inc_rr_num_lo) %>%
#   select(entity,
#          c_rr_coverage,
#          c_rr_coverage_lo,
#          c_rr_coverage_hi)
#
# # D. Bring them all together
# # - - - - - - - - - - - - -
#
# # Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
# coverage_dummy1 <- data.frame(entity = " ", c_rr_coverage = NA, c_rr_coverage_lo = 0, c_rr_coverage_hi = 100)
# coverage_dummy2 <- data.frame(entity = "  ", c_rr_coverage = NA, c_rr_coverage_lo = 0, c_rr_coverage_hi = 100)
#
#
# # Create combined dataframe in order of countries then regional and global estimates
# f3.4.2_data <- rbind(coverage_country,
#                      coverage_dummy1,
#                      coverage_region,
#                      coverage_dummy2,
#                      coverage_global) %>%
#
#   # The dataframe is in the order I want, so make entity an ordered factor based on
#   # what I already have. That way ggplot will not reorder by entity name
#   # But I need to reverse order for plotting
#
#   mutate(entity = factor(entity,
#                          levels = rev(entity)))
#
# # Simple dataframe of numbers for section text
# f3.4.2_txt <- coverage_global %>%
#   select(c_rr_coverage)
#
# # remove the temporary dataframes
# rm(list=ls(pattern = "^coverage"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.1 (previously 3.4.3) ----
# (Bar chart of numbers treated for MDR-TB each year since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.4.1_data <- filter(notification, year >=2015) %>%

  select(year,
         unconf_rrmdr_tx,
         conf_rrmdr_tx,
         unconf_rr_nfqr_tx,
         conf_rr_nfqr_tx,
         conf_rr_fqr_tx,
         rrmdr_014_tx) %>%

  group_by(year) %>%
  summarise(across(unconf_rrmdr_tx:rrmdr_014_tx, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # Calculate total treated (all ages and over-15s). Set all ages to NA when we have numbers for children
  # and set adults to NA when we don;t have children. That way we can have a stacked bar chart that
  # handles the early years without children and the latter with children
  mutate(rrmdr_all_tx = ifelse(rrmdr_014_tx > 0,
                               NA,
                               unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx)) %>%
  mutate(rrmdr_15plus_tx = ifelse(rrmdr_014_tx > 0,
                                  unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx - rrmdr_014_tx,
                                  NA)) %>%
  mutate(rrmdr_014_tx = ifelse(rrmdr_014_tx == 0,
                               NA,
                               rrmdr_014_tx)) %>%

  # restrict to essential variables
  select(year,
         rrmdr_all_tx,
         rrmdr_014_tx,
         rrmdr_15plus_tx) %>%

  # pivot to long format
  pivot_longer(cols = starts_with("rrmdr_"),
               names_to = "age_group",
               names_pattern = "rrmdr_(.*)_tx",
               values_to = "how_many")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.2 (was 3.4.4) ----
# (Panel of line charts of MDR/RR-TB cases detected, MDR/RR-TB put on treatment
# since 2009 for 30 countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.4.2_data <- filter(notification, year >= 2010 & iso3 %in% hbmdr30$iso3) %>%
  select(country,
         iso3,
         year,
         rapid_dx_dr_r,
         conf_mdr,
         conf_rrmdr,
         conf_rr_nfqr,
         conf_rr_fqr,
         conf_mdr_tx,
         unconf_mdr_tx,
         conf_rrmdr_tx,
         unconf_rrmdr_tx,
         conf_rr_nfqr_tx,
         unconf_rr_nfqr_tx,
         conf_rr_fqr_tx) %>%

  rowwise() %>%
  # Derive total number detected and total enrolled on treatment
  mutate(rr_detected = ifelse(year < 2014,
                              sum(across(rapid_dx_dr_r:conf_mdr), na.rm = TRUE),
                              # the next three are mutually exclusive so can be added
                              sum(across(conf_rrmdr:conf_rr_fqr), na.rm = TRUE)),
         # treatment variables are in mutually exclusive sets so again can be added
         rr_treated = sum(across(conf_mdr_tx:conf_rr_fqr_tx), na.rm = TRUE)) %>%
  ungroup() %>%

  # drop unneeded variables
  select(country,
         iso3,
         year,
         rr_detected,
         rr_treated)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.3 (was 3.4.5) ----
# (Irwin's doughnuts on completion of UNHLM targets)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Summary dataset for simple quoting of numbers
f3.4.3_txt <- filter(notification, year >= 2018) %>%
  summarise(across(c(conf_rrmdr_tx,
                     unconf_rrmdr_tx,
                     conf_rr_nfqr_tx,
                     unconf_rr_nfqr_tx,
                     conf_rr_fqr_tx,
                     rrmdr_014_tx), sum, na.rm=TRUE)) %>%

  # Derive total enrolled on MDR treatment
  rowwise() %>%
  mutate(rr_treated = sum(across(conf_rrmdr_tx:conf_rr_fqr_tx), na.rm = TRUE)) %>%
  ungroup() %>%
  select(-conf_rrmdr_tx,
         -unconf_rrmdr_tx,
         -conf_rr_nfqr_tx,
         -unconf_rr_nfqr_tx,
         -conf_rr_fqr_tx) %>%

  # Calculate percentage complete for each UNHLM 2018-2022 target
  mutate(rr_treated_pct  = rr_treated * 100 / 1.5e6,  # target: 1.5 million treated for drug-resistant TB
         rrmdr_014_tx_pct = rrmdr_014_tx * 100 / 115e3 # target: 115 thousand children treated for drug-resistant TB
  )



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.6  (DELETED)----
# (Bubble map of difference between MDR/RR-TB patients starting treatment and
#  estimated MDR/RR-TB incidence for 10 countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# f3.4.6_data  <- filter(est_dr_country, year == report_year - 1) %>%
#   select(iso3,
#          year,
#          e_inc_rr_num) %>%
#
#   # Link to notifications
#   inner_join(notification, by = c("iso3", "year")) %>%
#   select(iso3,
#          country,
#          e_inc_rr_num,
#          conf_rr_nfqr_tx,
#          unconf_rr_nfqr_tx,
#          conf_rr_fqr_tx) %>%
#
#   # Filter out countries that have not reported anything yet for the latest year
#   filter(!is.na(conf_rr_nfqr_tx) | !is.na(unconf_rr_nfqr_tx) | !is.na(conf_rr_fqr_tx)) %>%
#
#   # Calculate the gap and use that for the bubble sizes
#   mutate(size = e_inc_rr_num - (NZ(conf_rr_nfqr_tx) + NZ(unconf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx))) %>%
#
#   # limit to the top 10 by size of gap
#   top_n(10, size) %>%
#
#   # sort in descending order so can list the country names in the figure footnote
#   arrange(desc(size))
#
# # Summary number of gaps for the section text
# # Get global incidence
# f3.4.6_txt <- filter(est_dr_group, year == report_year-1 & group_type == "global") %>%
#   select(e_inc_rr_num)
#
# # Add global enrolments
# f3.4.6_txt <- filter(notification, year == report_year-1) %>%
#   select(year,
#          conf_rr_nfqr_tx,
#          unconf_rr_nfqr_tx,
#          conf_rr_fqr_tx) %>%
#   # calculate global aggregate
#   group_by(year) %>%
#   summarise(across(conf_rr_nfqr_tx:conf_rr_fqr_tx, sum, na.rm=TRUE)) %>%
#   ungroup() %>%
#   mutate(enrolled = conf_rr_nfqr_tx + unconf_rr_nfqr_tx + conf_rr_fqr_tx) %>%
#   cbind(f3.4.6_txt) %>%
#
#   # Calculate the global gap and drop the other variables
#   mutate(gap = e_inc_rr_num - enrolled) %>%
#   select(gap)
#
# # Calculate % of global gap contributed by the top 10 countries
# f3.4.6_txt <- f3.4.6_data %>%
#   summarise(gap_top_ten = sum(size)) %>%
#   cbind(f3.4.6_txt) %>%
#   mutate(pct_gap_top_ten = gap_top_ten * 100 / gap)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.4 (was 3.4.7)  ----
# (horizontal bar chart showing TB treatment outcomes globally by year since 2012 for MDR/RR-TB)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.4.4_data <- outcomes %>%

  filter(between(year, 2012, report_year - 3)) %>%
  select(iso2,
         year,
         contains("mdr_")) %>%

  # calculate global aggregates
  group_by(year) %>%
  summarise(across(mdr_coh:c_mdr_neval, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_") %>%

  # Drop the actual numbers and keep percentages, plu sother unwanted variables
  select(-coh, -succ, -fail, -died, -lost, -c_neval, -cur, -cmplt) %>%

  # flip into long format
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")

# Summary for section text
f3.4.4_txt <- filter(f3.4.4_data, year %in% c(2012, report_year-3) & outcome=="Treatment success") %>%
  select(-outcome) %>%
  pivot_wider(names_from = year,
              names_prefix = "c_tsr_",
              values_from = value)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.5 (was 3.4.8) ----
# (Horizontal bar chart showing TB treatment outcomes in MDR/RR-TB cases for WHO regions and globally")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Code similar to others so maybe make a reusable function?

# A. Regional aggregates
# - - - - - - - - - - - -
txout_region  <- filter(outcomes, year==report_year - 3) %>%

  select(iso2,
         g_whoregion,
         contains("mdr_")) %>%
  select(-mdr_cur, -mdr_cmplt) %>%

  group_by(g_whoregion) %>%
  summarise(across(mdr_coh:c_mdr_neval, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion) %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_") %>%

  # Sort regions in descending order of success rate
  arrange(desc(`Treatment success`))


# B. Global aggregates
# - - - - - - - - - - - -
txout_global  <- filter(outcomes, year==report_year - 3) %>%

  select(iso2,
         contains("mdr_")) %>%
  select(-mdr_cur, -mdr_cmplt) %>%

  summarise(across(mdr_coh:c_mdr_neval, sum, na.rm=TRUE)) %>%
  ungroup()  %>%
  mutate(entity="Global")  %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("mdr_")


# Create a dummy record a gap in the output to separate countries, regions and global parts
txout_dummy <- data.frame(entity = " ", coh = NA, succ = NA, fail = NA,
                          died = NA, lost = NA, c_neval = NA,
                          Failure = NA, Died = NA)

# Had to use mutate to create the next 3 fields because data.frame converted spaces to dots. Grrr
txout_dummy <- txout_dummy %>%
  mutate(`Treatment success` = NA,
         `Lost to follow-up` = NA,
         `Not evaluated` = NA)


# Create combined table in order of countries then regional and global estimates
f3.4.5_data <- rbind(txout_region, txout_dummy, txout_global) %>%

  # Keep record of current order (in reverse) so plot comes out as we want it
  mutate(entity = factor(entity, levels=rev(entity))) %>%

  # Drop the actual numbers and keep percentages
  select(-coh, -succ, -fail, -died, -lost, -c_neval) %>%

  # Flip into long mode for stacked bar plotting
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")

# Summary for section text
f3.4.5_txt <- filter(txout_region, entity %in% c("African Region", "European Region")) %>%
  select(entity,
         c_tsr = `Treatment success`) %>%
  pivot_wider(names_from = entity,
              names_prefix = "c_tsr_",
              values_from = c_tsr) %>%
  # deal with spaces in variable names
  select(c_tsr_AFR = `c_tsr_African Region`,
         c_tsr_EUR = `c_tsr_European Region`)

# tidy up
rm(list = ls(pattern = "^txout_"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.6 (was 3.4.9) ----
# (World map showing which countries used bedaquiline in drug-resistant TB treatment regimens)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.4.6_data <- filter(notification, year == report_year-1) %>%
  select(country,
         iso3,
         mdrxdr_bdq_used) %>%

  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdrxdr_bdq_used = ifelse(mdrxdr_bdq_used==3, NA, mdrxdr_bdq_used)) %>%

  # Assign the categories for the map
  mutate(var = factor(mdrxdr_bdq_used,
                      levels = c(1, 0),
                      labels = c("Used", "Not used")))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.7 (was 3.4.10) ----
# (World map showing which countries used all-oral longer MDR-TB treatment regimens)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.4.7_data <- filter(notification, year == report_year-1) %>%
  select(country,
         iso3,
         mdrxdr_alloral_used) %>%

  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdrxdr_alloral_used = ifelse(mdrxdr_alloral_used==3, NA, mdrxdr_alloral_used)) %>%

  # Assign the categories for the map
  mutate(var = factor(mdrxdr_alloral_used,
                      levels = c(1, 0),
                      labels = c("Used", "Not used")))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.8 (was 3.4.11) ----
# (World map showing which countries used shorter MDR-TB treatment regimens)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.4.8_data <- filter(notification, year == report_year-1) %>%
  select(country,
         iso3,
         mdr_alloral_short_used) %>%

  # Change the "No data" option 3 to NA to avoid weird effects in the map legend
  mutate(mdr_alloral_short_used = ifelse(mdr_alloral_short_used==3, NA, mdr_alloral_short_used)) %>%


  # Assign the categories for the map
  mutate(var = factor(mdr_alloral_short_used,
                      levels = c(1, 0),
                      labels = c("Used", "Not used")))


# Summary for section text from the last 3 figures' data
f3.4.6_8_txt <- filter(f3.4.6_data, mdrxdr_bdq_used == 1) %>%
  summarise(bdq = n())

f3.4.6_8_txt <- filter(f3.4.7_data, mdrxdr_alloral_used == 1) %>%
  summarise(alloral_long = n()) %>%
  cbind(f3.4.6_8_txt)

f3.4.6_8_txt <- filter(f3.4.8_data, mdr_alloral_short_used == 1) %>%
  summarise(alloral_short = n()) %>%
  cbind(f3.4.6_8_txt)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.4.9 (was 3.4.12) ----
# (World map showing proportion of drug-resistant TB patients followed up for adverse events)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.4.9_data <- filter(notification, year==report_year-1) %>%

  select(country,
         iso3,
         unconf_rr_nfqr_tx,
         conf_rr_nfqr_tx,
         conf_rr_fqr_tx,
         mdr_tx_adsm) %>%

  # Calculate percent with active follow up
  mutate(adsm_pct = ifelse(NZ(unconf_rr_nfqr_tx) + NZ(conf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx) > 0,
                           mdr_tx_adsm * 100 / (NZ(conf_rr_nfqr_tx) + NZ(conf_rr_nfqr_tx) + NZ(conf_rr_fqr_tx)),
                           NA)) %>%

  # Assign the categories for the map
  mutate(var = cut(adsm_pct,
                   c(0, 25, 50, 75, Inf),
                   c('0-24', '25-49', '50-74', '\u226575'),
                   right=FALSE)) %>%

  # get rid of extra variables
  select(country,
         iso3,
         adsm_pct,
         var)


