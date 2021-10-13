# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch3-3-txt.rmd
# and ch3-3-figs.rmd
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
# est_dr_country <- read.csv(here(paste0('csv/db/db_dr_country_', csv_estimate_datestamp, '.csv')))
# est_dr_group <- read.csv(here(paste0('csv/db/db_dr_group_', csv_estimate_datestamp, '.csv')))

# Get Pete's aggregate incidence estimates by agre group and sex
load(here('disaggregation/dboutput/db_estimates_group.Rdata'))

# Get Philippe's output file of cumulative lives saved for table 3.3.1
lives_saved <- read.csv(here(paste0('csv/RegionalLivesSaved_', csv_estimate_datestamp, '.csv')))

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
# Data: figure 3.3.1 ----
# (Forest plot of TB treatment coverage in 30 countries, regionally and globally)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# A. Countries
# - - - - - - - -
coverage_inc_country <- filter(est_country, year==report_year-1) %>%
  select(year,
         iso3,
         e_inc_num = inc.num,
         e_inc_num_lo = inc.lo.num,
         e_inc_num_hi = inc.hi.num) %>%

  # restrict to high burden countries
  inner_join(hbc30, by = "iso3")

coverage_country <- filter(notification, year==report_year-1) %>%
  select(entity = country,
         iso3,
         c_newinc)  %>%
  inner_join(coverage_inc_country, by = "iso3") %>%
  select(-iso3) %>%
  mutate(c_cdr = c_newinc * 100 / e_inc_num,
         c_cdr_lo = c_newinc * 100  / e_inc_num_hi,
         c_cdr_hi = c_newinc * 100  / e_inc_num_lo,
         # highlight countries with no data
         entity = ifelse(is.na(c_newinc), paste0(entity, "*"), entity )) %>%
  select(entity,
         c_cdr,
         c_cdr_lo,
         c_cdr_hi) %>%
  arrange(desc(c_cdr))


# B. Regions
# - - - - - - - -
coverage_inc_region <- filter(est_regional, year==report_year-1) %>%
  select(g_whoregion = g.whoregion,
         e_inc_num = inc.num,
         e_inc_num_lo = inc.lo.num,
         e_inc_num_hi = inc.hi.num)

coverage_region <- filter(notification, year==report_year-1) %>%
  select(g_whoregion,
         c_newinc) %>%
  # calculate regional aggregates
  group_by(g_whoregion) %>%
  summarise(across(c_newinc, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # merge with incidence estimates
  inner_join(coverage_inc_region, by = "g_whoregion") %>%

  # Calculate coverage
  mutate(c_cdr = c_newinc * 100 / e_inc_num,
         c_cdr_lo = c_newinc * 100  / e_inc_num_hi,
         c_cdr_hi = c_newinc * 100  / e_inc_num_lo) %>%

  # merge with regional names and simplify to match structure of country table
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(entity,
         c_cdr,
         c_cdr_lo,
         c_cdr_hi) %>%
  arrange(desc(c_cdr))


# C. Global (Calculate for two years as CDR for the earlier year is needed for the text)
# - - - - - - - -
coverage_inc_global <- filter(est_global, year>=report_year-2) %>%
  select(year,
         e_inc_num = inc.num,
         e_inc_num_lo = inc.lo.num,
         e_inc_num_hi = inc.hi.num) %>%
  mutate(entity="Global")

coverage_global <- filter(notification, year>=report_year-2) %>%
  select(year,
         c_newinc) %>%
  # calculate global aggregate
  group_by(year) %>%
  summarise(across(c_newinc, sum, na.rm=TRUE)) %>%
  mutate(entity="Global") %>%
  ungroup() %>%
  inner_join(coverage_inc_global, by=c("entity", "year")) %>%

  # Calculate coverage
  mutate(c_cdr = c_newinc * 100 / e_inc_num,
         c_cdr_lo = c_newinc * 100  / e_inc_num_hi,
         c_cdr_hi = c_newinc * 100  / e_inc_num_lo)
# %>%
#   select(entity,
#          year,
#          c_cdr,
#          c_cdr_lo,
#          c_cdr_hi)

coverage_global_latest <- filter(coverage_global, year==report_year-1) %>%
  select(entity,
         c_cdr,
         c_cdr_lo,
         c_cdr_hi)

# D. Bring them all together
# - - - - - - - - - - - - -

# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
coverage_dummy1 <- data.frame(entity = " ", c_cdr = NA, c_cdr_lo = 0, c_cdr_hi = 100)
coverage_dummy2 <- data.frame(entity = "  ", c_cdr = NA, c_cdr_lo = 0, c_cdr_hi = 100)


# Create combined dataframe in order of countries then regional and global estimates
f3.3.1_data <- rbind(coverage_country,
                     coverage_dummy1,
                     coverage_region,
                     coverage_dummy2,
                     coverage_global_latest) %>%

  # The dataframe is in the order I want, so make entity an ordered factor based on
  # what I already have. That way ggplot will not reorder by entity name
  # But I need to reverse order for plotting

  mutate(entity = factor(entity,
                         levels = rev(entity)))


# Create summary for the text
f3.3.1_txt <- coverage_global %>%
  select(-entity, -e_inc_num_lo, -e_inc_num_hi) %>%
  # Calculate the gap between incidence and notifications
  mutate(global_gap = e_inc_num - c_newinc) %>%
  pivot_wider(names_from = year,
              values_from = c(c_cdr, c_cdr_lo, c_cdr_hi, e_inc_num, c_newinc, global_gap)) %>%
  # Calculate the % change in notifications
  mutate(pct_change_c_newinc = abs(c_newinc_2020 - c_newinc_2019) * 100 / c_newinc_2019)

# Add EUR and EMR
f3.3.1_txt <- filter(coverage_region, entity %in% c("European Region", "Eastern Mediterranean Region")) %>%
  select(entity, c_cdr) %>%
  pivot_wider(names_from = entity,
              values_from = c_cdr)  %>%
  # handle spaces in column names
  select(c_cdr_EUR = `European Region`,
         c_cdr_EMR = `Eastern Mediterranean Region`) %>%
  cbind(f3.3.1_txt)

# Add Mozambique pct pulmonary bac confirmed
f3.3.1_txt <- filter(notification, year==report_year-1 & iso3 == "MOZ") %>%
  select(new_labconf, new_clindx,
         ret_rel_labconf, ret_rel_clindx) %>%
  mutate(bacconf_pct_MOZ = (new_labconf + ret_rel_labconf) * 100 / (new_labconf + new_clindx + ret_rel_labconf + ret_rel_clindx)) %>%
  select(bacconf_pct_MOZ) %>%
  cbind(f3.3.1_txt)


# Create ist of countries with cdr < 50% for the text
f3.3.1_txt_list <- filter(coverage_country, c_cdr < 50) %>%
  select(entity) %>%
  arrange(entity)

# remove the temporary dataframes
rm(list=ls(pattern = "^coverage"))





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.3.2 ----
# (Bubble map of difference between notifications and estimated incidence for 10 countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f3.3.2_data  <- filter(est_country, year == report_year - 1) %>%
  select(iso3,
         year,
         e_inc_num = inc.num
  ) %>%

  # Link to notifications
  inner_join(notification, by =c("year", "iso3")) %>%
  select(iso3,
         country,
         e_inc_num,
         c_newinc) %>%

  # Calculate the gap and use that for the bubble sizes
  mutate(size = e_inc_num - c_newinc) %>%

  # limit to the top 10 by size of gap
  top_n(10, size) %>%

  # sort in descending order so can list the country names in the figure footnote
  arrange(desc(size))


# Summary number of gaps for the section text
# Get global incidence
f3.3.2_txt <- filter(est_global, year == report_year-1) %>%
  select(e_inc_num = inc.num)

# Add global notification
f3.3.2_txt <- filter(notification, year == report_year-1) %>%
  select(year,
         c_newinc) %>%
  # calculate global aggregate
  group_by(year) %>%
  summarise(across(c_newinc, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  cbind(f3.3.2_txt) %>%

  # Calculate the global gap and drop the other variables
  mutate(gap = e_inc_num - c_newinc) %>%
  select(gap) %>%

  # Calculate % of global gap contributed by the top 10 countries
  cbind(f3.3.2_data) %>%
  mutate(pct_gap = size * 100 / gap) %>%

  # flip wider for easy quoting in text
  select(iso3, pct_gap) %>%
  pivot_wider(names_from = iso3,
              names_prefix = "pct_gap_",
              values_from = pct_gap) %>%

  # Add total % accounted for by all ten countries
  mutate(pct_gap_top_ten = rowSums(.))








# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.3.3 ----
# (Line and ribbon chart of estimated TB/HIV incidence, number notified and number on antiretroviral therapy)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

inctbhiv_data <- filter(est_global, year >= 2004) %>%
  select(year,
         e_inc_tbhiv_num = inc.h.num,
         e_inc_tbhiv_num_lo = inc.h.lo.num,
         e_inc_tbhiv_num_hi = inc.h.hi.num)



f3.3.3_data <- filter(TBHIV_for_aggregates, year>=2004) %>%
  select(year,
         hivtest_pos,
         hiv_art,
         # new variables from 2015 onwards
         newrel_hivpos,
         newrel_art) %>%
  group_by(year) %>%
  summarise(across(hivtest_pos:newrel_art, sum, na.rm=TRUE)) %>%

  # Merge pre/post 2014 variables
  mutate(hivpos = ifelse(year < 2015,
                         hivtest_pos,
                         newrel_hivpos),
         art = ifelse(year < 2015,
                      hiv_art,
                      newrel_art)) %>%
  select(year,
         hivpos,
         art) %>%

  inner_join(inctbhiv_data, by = "year")

# Summary of numbers for the section text
f3.3.3_txt <- filter(f3.3.3_data, year>=report_year-2) %>%
  mutate(c_art_notified = art * 100 / hivpos,
         c_art_estimated = art * 100 / e_inc_tbhiv_num) %>%
  select(year,
         c_art_notified,
         c_art_estimated) %>%
  pivot_wider(names_from = year,
              values_from = c(c_art_notified, c_art_estimated))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.3.4 ----
# (Forest plot of TB treatment coverage in 30 countries, regionally and globally)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# A. Countries
# - - - - - - - -
coverage_inc_country <- filter(est_country, year==report_year-1) %>%
  select(year,
         iso3,
         e_inc_tbhiv_num = inc.h.num,
         e_inc_tbhiv_num_lo = inc.h.lo.num,
         e_inc_tbhiv_num_hi = inc.h.hi.num) %>%

  # restrict to high burden countries
  inner_join(hbtbhiv30, by = "iso3")

coverage_country <- filter(TBHIV_for_aggregates, year==report_year-1) %>%
  select(entity = country,
         iso3,
         newrel_art)  %>%
  inner_join(coverage_inc_country, by = "iso3") %>%
  select(-iso3) %>%
  mutate(c_art = newrel_art * 100 / e_inc_tbhiv_num,
         c_art_lo = newrel_art * 100  / e_inc_tbhiv_num_hi,
         c_art_hi = newrel_art * 100  / e_inc_tbhiv_num_lo,
         # highlight countries with no data
         entity = ifelse(is.na(newrel_art), paste0(entity, "*"), entity )) %>%
  select(entity,
         c_art,
         c_art_lo,
         c_art_hi) %>%
  arrange(desc(c_art))


# B. Regions
# - - - - - - - -
coverage_inc_region <- filter(est_regional, year==report_year-1) %>%
  select(g_whoregion = g.whoregion,
         e_inc_tbhiv_num = inc.h.num,
         e_inc_tbhiv_num_lo = inc.h.lo.num,
         e_inc_tbhiv_num_hi = inc.h.hi.num)

coverage_region <- filter(TBHIV_for_aggregates, year==report_year-1) %>%
  select(g_whoregion,
         newrel_art) %>%
  # calculate regional aggregates
  group_by(g_whoregion) %>%
  summarise(across(newrel_art, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # merge with incidence estimates
  inner_join(coverage_inc_region, by = "g_whoregion") %>%

  # Calculate coverage
  mutate(c_art = newrel_art * 100 / e_inc_tbhiv_num,
         c_art_lo = newrel_art * 100  / e_inc_tbhiv_num_hi,
         c_art_hi = newrel_art * 100  / e_inc_tbhiv_num_lo) %>%

  # merge with regional names and simplify to match structure of country table
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(entity,
         c_art,
         c_art_lo,
         c_art_hi) %>%
  arrange(desc(c_art))

# C. Global
# - - - - - - - -
coverage_inc_global <- filter(est_global, year==report_year-1) %>%
  select(e_inc_tbhiv_num = inc.h.num,
         e_inc_tbhiv_num_lo = inc.h.lo.num,
         e_inc_tbhiv_num_hi = inc.h.hi.num) %>%
  mutate(entity="Global")

coverage_global <- filter(TBHIV_for_aggregates, year==report_year-1) %>%
  select(newrel_art) %>%
  # calculate global aggregate
  summarise(across(newrel_art, sum, na.rm=TRUE)) %>%
  mutate(entity="Global") %>%
  inner_join(coverage_inc_global, by="entity") %>%

  # Calculate coverage
  mutate(c_art = newrel_art * 100 / e_inc_tbhiv_num,
         c_art_lo = newrel_art * 100  / e_inc_tbhiv_num_hi,
         c_art_hi = newrel_art * 100  / e_inc_tbhiv_num_lo) %>%
  select(entity,
         c_art,
         c_art_lo,
         c_art_hi)

# D. Bring them all together
# - - - - - - - - - - - - -

# Create dummy records so can see a horizontal line in the output to separate countries, regions and global parts
coverage_dummy1 <- data.frame(entity = " ", c_art = NA, c_art_lo = 0, c_art_hi = 100)
coverage_dummy2 <- data.frame(entity = "  ", c_art = NA, c_art_lo = 0, c_art_hi = 100)


# Create combined dataframe in order of countries then regional and global estimates
f3.3.4_data <- rbind(coverage_country,
                      coverage_dummy1,
                      coverage_region,
                      coverage_dummy2,
                      coverage_global) %>%

  # The dataframe is in the order I want, so make entity an ordered factor based on
  # what I already have. That way ggplot will not reorder by entity name
  # But I need to reverse order for plotting

  mutate(entity = factor(entity,
                         levels = rev(entity)))


# Summary of numbers for the section text
f3.3.4_txt <- filter(f3.3.4_data, entity %in% c("Gabon", "Mozambique")) %>%
  select(entity, c_art) %>%
  pivot_wider(names_from = entity,
              values_from = c_art)

# Add number of countries with coverage >= 50%
f3.3.4_txt <- filter(f3.3.4_data, c_art >= 50) %>%
  summarise(over_50 = n()) %>%
  cbind(f3.3.4_txt)

# remove the temporary dataframes
rm(list=ls(pattern = "^coverage"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.3.5 ----
# (Horizontal bar chart showing TB treatment outcomes for WHO regions and globally)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# A. Regional aggregates
# - - - - - - - - - - - -
txout_region  <- filter(outcomes, year==report_year - 2) %>%

  select(iso2,
         g_whoregion,
         newrel_coh,
         newrel_succ,
         newrel_fail,
         newrel_died,
         newrel_lost,
         c_newrel_neval) %>%

  group_by(g_whoregion) %>%
  summarise(across(newrel_coh:c_newrel_neval, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion) %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("newrel_") %>%

  # Sort regions in descending order of success rate
  arrange(desc(`Treatment success`))


# B. Global aggregates
# - - - - - - - - - - - -
txout_global  <- filter(outcomes, year==report_year - 2) %>%

  select(iso2,
         newrel_coh,
         newrel_succ,
         newrel_fail,
         newrel_died,
         newrel_lost,
         c_newrel_neval) %>%

  summarise(across(newrel_coh:c_newrel_neval, sum, na.rm=TRUE)) %>%
  ungroup()  %>%
  mutate(entity="Global")  %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("newrel_")


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
f3.3.5_data <- rbind(txout_region, txout_dummy, txout_global) %>%

  # Keep record of current order (in reverse) so plot comes out as we want it
  mutate(entity = factor(entity, levels=rev(entity))) %>%

  # Drop the actual numbers and keep percentages
  select(-coh, -succ, -fail, -died, -lost, -c_neval) %>%

  # Flip into long mode for stacked bar plotting
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")

# Create summary for section text
f3.3.5_txt <- filter(f3.3.5_data, outcome == "Treatment success" &
                       entity %in% c("Global", "Eastern Mediterranean Region", "Region of the Americas")) %>%
  pivot_wider(names_from = entity,
              names_prefix = "tsr_",
              values_from = value) %>%
  #simplify variable names
  select(tsr_EMR = `tsr_Eastern Mediterranean Region`,
         tsr_AMR = `tsr_Region of the Americas`,
         tsr_Global)

# tidy up
rm(list = ls(pattern = "^txout_"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.3.6 ----
# (Panel of 3 horizontal bar charts showing TB treatment outcomes globally by year since 2012 for TB, TB/HIV and MDR/RR-TB)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

txout_tb_data <- outcomes %>%
  filter(between(year, 2012, report_year - 2)) %>%
  select(iso2,
         year,
         newrel_coh,
         newrel_succ,
         newrel_fail,
         newrel_died,
         newrel_lost,
         c_newrel_neval) %>%

  # calculate global aggregates
  group_by(year) %>%
  summarise(across(newrel_coh:c_newrel_neval, sum, na.rm=TRUE)) %>%
  ungroup()%>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("newrel_") %>%

  # Drop the actual numbers and keep percentages
  select(-coh, -succ, -fail, -died, -lost, -c_neval) %>%

  # Add tx group type
  mutate(subgroup = "New and relapse TB cases")

txout_hiv_data <- outcomes %>%
  filter(between(year, 2012, report_year - 2)) %>%
  select(iso2,
         year,
         country,
         contains("tbhiv_")) %>%

  # calculate global aggregates
  group_by(year) %>%
  summarise(across(tbhiv_coh:c_tbhiv_neval, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("tbhiv_") %>%

  # Drop the actual numbers and keep percentages
  select(-coh, -succ, -fail, -died, -lost, -c_neval) %>%

  # Add tx group type
  mutate(subgroup = "New and relapse HIV-positive TB cases")

# Combine the two data frames
f3.3.6_data <- rbind(txout_tb_data, txout_hiv_data) %>%

  # flip into long format
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome") %>%

  # Determine the order of subgroup for plotting
  mutate(subgroup = factor(subgroup,
                           levels = c("New and relapse TB cases",
                                      "New and relapse HIV-positive TB cases")))

# tidy up
rm(list = ls(pattern = "^txout_"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.3.7 ----
# (Horizontal bar chart showing TB treatment outcomes in HIV-positive cases for WHO regions and globally)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# A. Regional aggregates
# - - - - - - - - - - - -
txout_region  <- filter(outcomes, year==report_year - 2) %>%

  select(iso2,
         g_whoregion,
         contains("tbhiv_")) %>%

  group_by(g_whoregion) %>%
  summarise(across(tbhiv_coh:c_tbhiv_neval, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion) %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("tbhiv_") %>%

  # Sort regions in descending order of success rate
  arrange(desc(`Treatment success`))


# B. Global aggregates
# - - - - - - - - - - - -
txout_global  <- filter(outcomes, year==report_year - 2) %>%

  select(iso2,
         contains("tbhiv_")) %>%

  summarise(across(tbhiv_coh:c_tbhiv_neval, sum, na.rm=TRUE)) %>%
  ungroup()  %>%
  mutate(entity="Global")  %>%

  # Calculate outcome proportions for plotting as stacked bars
  calculate_outcomes_pct("tbhiv_")


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
f3.3.7_data <- rbind(txout_region, txout_dummy, txout_global) %>%

  # Keep record of current order (in reverse) so plot comes out as we want it
  mutate(entity = factor(entity, levels=rev(entity))) %>%

  # Drop the actual numbers and keep percentages
  select(-coh, -succ, -fail, -died, -lost, -c_neval) %>%

  # Flip into long mode for stacked bar plotting
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")

# Create summary for section text
f3.3.7_txt <- filter(f3.3.7_data, outcome == "Treatment success" & entity == "Global") %>%
  #simplify variable name
  select(tsr_tbhiv_Global = value)

# tidy up
rm(list = ls(pattern = "^txout_"))




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.3.8 ----
# (Horizontal bar chart showing TB treatment success rates in children for WHO regions and globally)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# A. Regional aggregates
# - - - - - - - - - - - -
txout_region  <- filter(outcomes, year==report_year - 2) %>%

  select(iso2,
         g_whoregion,
         newrel_014_coh,
         newrel_014_succ) %>%

  group_by(g_whoregion) %>%
  summarise(across(newrel_014_coh:newrel_014_succ, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion) %>%

  # Calculate treatment success rate
  mutate(c_tsr_014 = newrel_014_succ * 100/newrel_014_coh) %>%

  # Sort regions in descending order of success rate
  arrange(desc(c_tsr_014))

# B. Global aggregates
# - - - - - - - - - - - -
txout_global  <- filter(outcomes, year==report_year - 2) %>%

  select(iso2,
         newrel_014_coh,
         newrel_014_succ) %>%

  summarise(across(newrel_014_coh:newrel_014_succ, sum, na.rm=TRUE)) %>%
  ungroup()  %>%
  mutate(entity="Global")  %>%

  # Calculate treatment success rate
  mutate(c_tsr_014 = newrel_014_succ * 100/newrel_014_coh)

# Create a dummy record to make a gap in the output to separate regional and global parts
txout_dummy <- data.frame(entity = " ", newrel_014_coh = NA, newrel_014_succ = NA, c_tsr_014 = NA)

# Create combined table in order of countries then regional and global estimates
f3.3.8_data <- rbind(txout_region, txout_dummy, txout_global) %>%

  # Keep record of current order (in reverse) so plot comes out as we want it
  mutate(entity = factor(entity, levels=rev(entity))) %>%

  # Drop the actual numbers and keep percentages
  select(-newrel_014_coh,
         -newrel_014_succ)

# Create summary for section text and footnote
f3.3.8_txt <- filter(f3.3.8_data, entity == "Global") %>%
  #simplify variable name
  select(tsr_014_Global = c_tsr_014)

# Add number of countries that reported and total cohort
f3.3.8_txt <- filter(outcomes, year==report_year - 2 &
                       !is.na(newrel_014_coh) & !is.na(newrel_014_succ)) %>%
  summarise(countries = n(),
            kids_coh = sum(newrel_014_coh, na.rm=TRUE)) %>%
  cbind(f3.3.8_txt)

# Calculate percent of notified that had an outcome reported
f3.3.8_txt <- filter(notification, year==report_year-2) %>%
  summarise(kids_notified = sum(c_new_014, na.rm=TRUE)) %>%
  cbind(f3.3.8_txt) %>%
  mutate(kids_outcome_pct = kids_coh * 100 / kids_notified)

# tidy up
rm(list = ls(pattern = "^txout_"))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 3.3.9 ----
# (Panel of bar charts showing treatment outcomes in absolute numbers by year since 2000 globally and for WHO regions)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

txoutnum_regional <- outcomes %>%
  filter(year >= 2000 & year <= report_year - 2) %>%
  select(year,
         g_whoregion,
         new_sp_coh,
         new_sp_cur,
         new_sp_cmplt,
         c_new_sp_neval,
         new_snep_coh,
         new_snep_cmplt,
         c_new_snep_neval,
         newrel_coh,
         newrel_succ,
         c_newrel_neval)%>%
  group_by(year, g_whoregion) %>%
  summarise(across(new_sp_coh:c_newrel_neval, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion)

txoutnum_global <- outcomes %>%
  filter(year >= 2000 & year <= report_year - 2) %>%
  select(year,
         g_whoregion,
         new_sp_coh,
         new_sp_cur,
         new_sp_cmplt,
         c_new_sp_neval,
         new_snep_coh,
         new_snep_cmplt,
         c_new_snep_neval,
         newrel_coh,
         newrel_succ,
         c_newrel_neval)%>%
  group_by(year) %>%
  summarise(across(new_sp_coh:c_newrel_neval, sum, na.rm=TRUE)) %>%
  ungroup() %>%

  mutate(entity = "Global")

#Combine regional and global data and reorganise
f3.3.9_data <- rbind(txoutnum_regional, txoutnum_global) %>%

  mutate(entity = factor(entity,
                         levels = c("African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region",
                                    "Global") )) %>%

  # Simplify the data for plotting
  mutate(success = (new_sp_cur + new_sp_cmplt + new_snep_cmplt + newrel_succ) / 1e6,
         neval = (c_new_sp_neval + c_new_snep_neval + c_newrel_neval) / 1e6,
         coh = (new_sp_coh + new_snep_coh + newrel_coh) / 1e6) %>%
  mutate(fail_other = coh - success - neval) %>%
  select(entity,
         year,
         `Treatment success` = success,
         `Failure/Died/Lost to follow-up` = fail_other,
         `Not evaluated` = neval) %>%

  # Flip into long mode for stacked bar plotting
  pivot_longer(cols = `Treatment success`:`Not evaluated`,
               names_to = "outcome")

# tidy up
rm(list = ls(pattern = "^txoutnum_"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: Table 3.3.1 ----
# (Table showing cumulative number of lives saved by WHO region and globally)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Uses the lives_saved table which Hazim tweaked from Philippe's original
# to standardise numbers to 2 significant figures to match what Irwin is showing
# in table E2
t3.3.1_data <- lives_saved %>%
  # Add long region names
  left_join(who_region_shortnames, by = c("Region" = "g_whoregion")) %>%
  mutate(entity = ifelse(Region=="Global", "Global", as.character(entity))) %>%

  # Put entity at the beginning and drop Region
  select(entity,
         saved.hivneg,
         saved.hivneg.ui,
         saved.hivpos,
         saved.hivpos.ui,
         saved,
         saved.ui,
         -Region)  %>%

  # Change the order based on the bizarre method chosen by WHO Press ...
  mutate(entity = factor(entity,
                         levels = c("African Region", "Region of the Americas", "South-East Asia Region",
                                    "European Region", "Eastern Mediterranean Region", "Western Pacific Region",
                                    "Global"))) %>%
  arrange(entity)


# Get summary for easy quoting in the text
t3.3.1_txt <- filter(t3.3.1_data, entity=="Global") %>%
  select(saved)

