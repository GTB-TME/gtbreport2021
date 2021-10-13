# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ch4.Rmd
# Hazim Timimi, August 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load data packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(stringr)
library(dplyr)
library(tidyr)
library(ghost)   # GHO data
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
strategy <- get_timestamped_csv('sty', csv_datestamp)
country_group_membership <- get_timestamped_csv('grpmbr', csv_datestamp)
estimates_ltbi <- get_timestamped_csv('ltbi', csv_datestamp)
estimates_population <- get_timestamped_csv('pop', csv_datestamp)

# Convert total population to numeric to avoid integer overflow errors
estimates_population$e_pop_num <- as.numeric(estimates_population$e_pop_num)

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


# Add simple function to convert NA to zero
NZ <- function(x) {
  ifelse(is.na(x),
         0,
         x)
}





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 4.1 ----
# (Bar chart showing numbers provided with TB preventive treatment each year since 2015)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.1_data <- filter(notification, year %in% seq(2015, report_year - 1)) %>%
  select(iso2,
         year,
         hiv_ipt_reg_all,
         hiv_ipt,
         # These next ones introduced dcyear 2021 by GAM
         hiv_all_tpt,
         hiv_new_tpt,
         hiv_elig_all_tpt,
         hiv_elig_new_tpt) %>%

  # Use the first non-empty tpt/ipt variable (not sure this is the best approach as it has become so
  # darn complicated in 2021 ...)
  mutate(hiv_tpt = coalesce(hiv_ipt_reg_all, hiv_ipt, hiv_all_tpt, hiv_elig_all_tpt, hiv_new_tpt, hiv_elig_new_tpt)) %>%

  select(iso2, year, hiv_tpt) %>%

  # Join to tpt data for household contacts in the strategy view
  inner_join( select(strategy,
                     iso2, year,
                     # next one added 2016 dcyear
                     newinc_con04_prevtx,
                     # next one used 2018 dcyear only
                     newinc_con5plus_prevtx,
                     # next one added 2019 dcyear
                     newinc_con_prevtx), by=c("iso2", "year")) %>%

  # Calculate the "5 and over" fraction of tpt for household contacts
  mutate(prevtx_5plus = ifelse(NZ(newinc_con_prevtx) > 0 & NZ(newinc_con04_prevtx) > 0,
                               newinc_con_prevtx - newinc_con04_prevtx,
                               newinc_con_prevtx)) %>%

  # Convert negative prevtx_5plus caused by weird combination of carry overs to zero
  mutate(prevtx_5plus = ifelse(NZ(prevtx_5plus) < 0 , 0, prevtx_5plus)) %>%

  # deal with 2017 variable
  mutate(prevtx_5plus = ifelse(year == 2017 ,
                               newinc_con5plus_prevtx,
                               prevtx_5plus)) %>%

  # Keep variables for HIV, contacts < 5 and contacts 5 plus
  select(iso2, year,
         hiv_tpt,
         house_con04_tpt = newinc_con04_prevtx,
         house_con5plus_tpt = prevtx_5plus) %>%

  # Calculate the global totals by year ready for the plot
  group_by(year) %>%
  summarise_at(vars(-iso2), sum, na.rm=TRUE) %>%
  ungroup() %>%

  # Finally, switch to a long format ready for plotting
  pivot_longer(cols = hiv_tpt:house_con5plus_tpt,
               names_to = "TPT_category",
               values_to = "how_many")

# Create summary stats for the text
f4.1_txt <- f4.1_data %>%
  group_by(year) %>%
  summarise(tot_tpt = sum(how_many)) %>%
  pivot_wider(names_from = year,
              names_prefix = "tot_tpt_",
              values_from = tot_tpt) %>%
  mutate(delta_18_19 = tot_tpt_2019 - tot_tpt_2018,
         pct_20_19 = abs(tot_tpt_2020 - tot_tpt_2019) * 100 / tot_tpt_2019,
         tot_tpt_18_20 = tot_tpt_2018 + tot_tpt_2019 + tot_tpt_2020) %>%
  mutate(pct_tpt_target = tot_tpt_18_20 * 100 / 30e6)

# Add stats just for household contacts
f4.1_txt <- filter(f4.1_data, TPT_category != "hiv_tpt" & year >= 2019) %>%
  group_by(year) %>%
  summarise(tot_con_tpt = sum(how_many)) %>%
  pivot_wider(names_from = year,
              names_prefix = "tot_con_tpt_",
              values_from = tot_con_tpt) %>%
  mutate(con_20_19_pct = abs(tot_con_tpt_2020 - tot_con_tpt_2019) * 100 / tot_con_tpt_2019) %>%
  cbind(f4.1_txt)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data: figure 4.2 ----
# (Irwin's doughnuts -- % completion of UNHLM targets)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.2_txt <- filter(f4.1_data, year>=2018) %>%
  group_by(TPT_category) %>%
  summarise(tot=sum(how_many)) %>%
  pivot_wider(names_from = TPT_category,
              values_from = tot) %>%
  mutate(all_con = house_con04_tpt + house_con5plus_tpt) %>%
  # Calculate percent of target
  mutate(con04_target = house_con04_tpt * 100 / 4e6,
         con5plus_target = house_con5plus_tpt * 100 / 20e6,
         all_con_target = all_con * 100 / 24e6)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 4.6  (was 4.3) ----
# (Panel plots showing numbers of people living with HIV provided with TB preventive treatment each year since 2005 by WHO region and globally)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.6_data <- filter(notification, year %in% seq(2005, report_year - 1)) %>%
  select(iso2,
         g_whoregion,
         year,
         hiv_ipt_reg_all,
         hiv_ipt,
         # These next ones introduced dcyear 2021 by GAM
         hiv_all_tpt,
         hiv_new_tpt,
         hiv_elig_all_tpt,
         hiv_elig_new_tpt) %>%

  # Create calculated variables for TPT among all enrolled and TPT among newly enrolled
  # filling in gaps for missing data
  # The choice for 2020 GAM data is rather murky ...
  mutate(hiv_tpt_all = coalesce(hiv_ipt_reg_all, hiv_ipt, hiv_all_tpt, hiv_elig_all_tpt, hiv_new_tpt, hiv_elig_new_tpt),
         hiv_tpt_new = coalesce(hiv_ipt, hiv_ipt_reg_all, hiv_new_tpt, hiv_elig_new_tpt, hiv_all_tpt, hiv_elig_all_tpt)) %>%

  # Calculate regional aggregates
  group_by(year, g_whoregion) %>%
  summarise_at(vars(hiv_tpt_all:hiv_tpt_new),
               sum,
               na.rm = TRUE) %>%
  ungroup() %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion)

# Create global aggregates
f4.6_data_global <- f4.6_data %>%
  group_by(year) %>%
  summarise_at(vars(hiv_tpt_all:hiv_tpt_new),
               sum,
               na.rm = TRUE) %>%
  mutate(entity = 'Global')

# Add global to the regional aggregates
f4.6_data <- rbind(f4.6_data, f4.6_data_global)

# Only want hiv_tpt_all for years after 2016
f4.6_data <- f4.6_data %>%
  mutate(hiv_tpt_all = ifelse(year < 2017,
                              NA,
                              hiv_tpt_all))

# Change the entity order
f4.6_data$entity <- factor(f4.6_data$entity,
                          levels = c("Global", "African Region", "Region of the Americas", "South-East Asia Region",
                                     "European Region", "Eastern Mediterranean Region", "Western Pacific Region"))

# Summary for quoting in the text
f4.6_txt <- filter(f4.6_data, entity=="Global" & year >= 2018) %>%
  select(-hiv_tpt_new, -entity) %>%
  pivot_wider(names_from = year,
              names_prefix = "hiv_tpt_",
              values_from = hiv_tpt_all) %>%

  mutate(pct_20_19 = abs(hiv_tpt_2020 - hiv_tpt_2019) * 100 / hiv_tpt_2019,
         hiv_tpt_18_20 = hiv_tpt_2018 + hiv_tpt_2019 + hiv_tpt_2020 )

# Add cumulative total since 2005
f4.6_txt <- filter(f4.6_data, entity=="Global" & year %in% 2005:2017) %>%
  summarise(hiv_tpt_05_17 = sum(hiv_tpt_new)) %>%
  cbind(f4.6_txt) %>%
  mutate(hiv_tpt_05_20 = hiv_tpt_05_17 + hiv_tpt_18_20) %>%
  mutate(hiv_tpt_05_20_pct = hiv_tpt_05_20 * 100 / 37.7e6)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 4.7 (was 4.4) ----
# (Horizontal bar chart showing the provision of TB preventive treatment as a proportion of the global total for the top 6 countries)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.7_data <- filter(notification, year == report_year - 1) %>%
  select(iso2,
         country,
         hiv_ipt_reg_all,
         hiv_ipt,
         # These next ones introduced dcyear 2021 by GAM
         hiv_all_tpt,
         hiv_new_tpt,
         hiv_elig_all_tpt,
         hiv_elig_new_tpt) %>%

  # Use the first non-empty tpt/ipt variable (not sure this is the best approach as it has become so
  # darn complicated in 2021 ...)
  mutate(hiv_tpt = coalesce(hiv_ipt_reg_all, hiv_ipt, hiv_all_tpt, hiv_elig_all_tpt, hiv_new_tpt, hiv_elig_new_tpt)) %>%
  select(iso2, country, hiv_tpt) %>%

  # Calculate the proportion of global total for each country
  mutate(proportion = hiv_tpt * 100 / sum(hiv_tpt, na.rm = TRUE)) %>%

  # Sort  in descending order of TPT provision
  arrange(desc(proportion))

# Summary for quoting in the text: Capture the global total for the section text before filtering for the top 6
f4.7_txt <- f4.7_data %>%
  summarise(hiv_tpt_glob = sum(hiv_tpt, na.rm=TRUE))

# Take the top 6 countries only for the figure
f4.7_data <- slice(f4.7_data, 1:6)

# Add proportion accounted for by the top 6 to the summary for quoting in the text
f4.7_txt <- f4.7_data %>%
  summarise(prop_top_6 = sum(proportion)) %>%
  cbind(f4.7_txt)

# Make list of the top 6 countries sorted alphabetically
f4.7_txt_list <- select(f4.7_data, country) %>%
  arrange(country)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 4.5 (OLD -- FIGURE DROPPED )----
# (Map showing provision of TB preventive treatment among PLHIV who started antiretroviral treatment)
# Keeping code as we use summary stats in the text
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.5_old_data <- filter(notification, year == report_year - 1) %>%
  select(country,
         iso3,
         hiv_elig_new_tpt,
         hiv_elig_new,
         hiv_new_tpt,
         hiv_new) %>%

  # Calculate % coverage among newly enrolled on ART. Prioritise the variables that are not restricted
  # to "eligible" PLHIV

  mutate(coverage = ifelse(!is.na(hiv_new_tpt) & NZ(hiv_new) > 0,
                           hiv_new_tpt * 100 / hiv_new,
                           ifelse(!is.na(hiv_elig_new_tpt) & NZ(hiv_elig_new) > 0,
                                  hiv_elig_new_tpt * 100 / hiv_elig_new,
                                  NA))) %>%

  # Assign the categories for the map
  mutate(var = cut(coverage,
                   c(0, 25, 50, 75, Inf),
                   c('0-24', '25-49', '50-74', '\u226575'),
                   right = FALSE))

# Summary for the text
f4.5_old_txt <- filter(f4.5_old_data,
                   !is.na(hiv_elig_new_tpt) & !is.na(hiv_elig_new) & iso3 %in% hbtbhiv30$iso3) %>%
  summarise(countries = n(),
            median = median(coverage),
            q1 = unname(quantile(coverage, 0.25)),
            q3 = unname(quantile(coverage, 0.75)))

rm(f4.5_old_data)


# Add summary for the text on TPT completion in PLHIV (the figure we were going to use was dropped)
completion_txt <- filter(notification, year == report_year - 2 &
                         hiv_all_tpt_completed > 0 & hiv_all_tpt_started > 0) %>%
  select(iso3,
         hiv_all_tpt_completed,
         hiv_all_tpt_started)  %>%
  mutate(completion = hiv_all_tpt_completed * 100 / hiv_all_tpt_started) %>%
  summarise(countries = n(),
            median = median(completion),
            q1 = unname(quantile(completion, 0.25)),
            q3 = unname(quantile(completion, 0.75)))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 4.3 (was 4.5) ----
# (Map showing evaluation for active TB and TB infection among household contacts of confirmed pulmonary TB cases)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.3_data <- filter(strategy, year == report_year - 1) %>%
  select(iso3,
         country,
         newinc_con,
         newinc_con_screen,
         rev_newinc_con,
         rev_newinc_con_screen) %>%

  # Calculate the proportion screened
  mutate(screened_pct = ifelse(!is.na(newinc_con) & newinc_con > 0,
                               newinc_con_screen * 100 / newinc_con,
                               NA)) %>%

  # Add any countries that reported from review of patient records
  mutate(screened_pct = ifelse(is.na(screened_pct) & !is.na(rev_newinc_con) & NZ(rev_newinc_con) > 0,
                               rev_newinc_con_screen * 100 / rev_newinc_con,
                               screened_pct))  %>%

  # Assign the categories for the map
  mutate(var = cut(screened_pct,
                   c(0, 25, 50, 75, Inf),
                   c('0-24', '25-49', '50-74', '\u226575'),
                   right = FALSE))


# Summary for the text
f4.3_txt <- filter(strategy, year >= report_year - 2 &
                     !is.na(newinc_con) & !is.na(newinc_con_screen)) %>%
  select(iso3,
         year,
         newinc_con,
         newinc_con_screen) %>%
  group_by(year) %>%
  summarise(across(newinc_con:newinc_con_screen, sum, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(screened_pct = newinc_con_screen * 100 / newinc_con) %>%
  pivot_wider(names_from = year,
              values_from = newinc_con:screened_pct) %>%
  mutate(change_con_20_19_pct = abs(newinc_con_2020 - newinc_con_2019) * 100 / newinc_con_2019,
         change_screen_20_19_pct = abs(newinc_con_screen_2020 - newinc_con_screen_2019) * 100 / newinc_con_screen_2019)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 4.4 (was 4.6) ----
# (Map showing coverage of TB preventive treatment among eligible children aged under 5 years)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.4_data <- filter(estimates_ltbi, year == report_year - 1) %>%

  select(iso3,
         country,
         e_prevtx_kids_pct,
         e_prevtx_eligible,
         newinc_con04_prevtx)  %>%

  # Assign the categories for the map
  mutate(var = cut(e_prevtx_kids_pct,
                   c(0, 25, 50, 90, Inf),
                   c('0-24', '25-49', '50-89', '\u226590'),
                   right = FALSE))






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 4.5 (was 4.7) ----
# (Panel plot showing percentage completion vs number started by WHO region)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.5_data <- filter(strategy, year == report_year - 2) %>%

  select(country,
         g_whoregion,
         newinc_con_prevtx,
         newinc_con_prevtx_cmplt) %>%

  # Calculate completion rate
  mutate(pct_completed = ifelse(!is.na(newinc_con_prevtx_cmplt) & NZ(newinc_con_prevtx) > 0,
                                newinc_con_prevtx_cmplt * 100 /newinc_con_prevtx ,
                                NA)) %>%

  # merge with regional names
  inner_join(who_region_shortnames, by = "g_whoregion") %>%
  select(-g_whoregion) %>%

  # filter out empty lines
  filter(!is.na(pct_completed))


# Summary for the text
f4.5_txt <- f4.5_data %>%
  summarise(countries = n(),
            median = median(pct_completed),
            q1 = unname(quantile(pct_completed, 0.25)),
            q3 = unname(quantile(pct_completed, 0.75)))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 4.8 ----
# (Map showing countries using rifapentine for TB preventive treatment)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load external file sent by Sanofi to Dennis on use of rifapentime
f4.8_data <- read.csv(here('csv/rifapentine_2021-06-30.csv')) %>%

  mutate(rfp_usage = ifelse(NZ(rfp_supplied) == 1,
                           1,
                           ifelse(rfp_used_trials == 1,
                                  2,
                                  NA)) ) %>%

  # Assign the categories for the map
  mutate(var = factor(rfp_usage,
                      levels = c(1, 2),
                      labels = c("Current or past use", "Used in trials only")))


# Create a list of countries in which rifapentine is registered to display in the footnote.
f4.8_footnote <- f4.8_data %>%

  filter(rfp_registered == 1) %>%

  select(country)

# Summary for the text
f4.8_txt <- filter(f4.8_data, rfp_usage==1) %>%
  summarise(countries_used = n())




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 4.9 ----
# (Map showing ratio of TB notification rates among health care workers to those among the adult population)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

f4.9_data <- filter(strategy, year == report_year - 1) %>%

  select(iso3,
         country,
         hcw_tb_infected,
         hcw_tot)

# Get the total adult population aged between 15 and 64 (exclude those aged 65 and above)
pop_adults <- filter(estimates_population, year == report_year - 1) %>%

  select(iso3,
         e_pop_f15plus,
         e_pop_f65,
         e_pop_m15plus,
         e_pop_m65) %>%

  mutate(e_pop_adult = e_pop_f15plus + e_pop_m15plus - e_pop_f65 -  e_pop_m65 ) %>%

  select(iso3,
         e_pop_adult)

# Get the total notifications among adults aged between 15 and 64 (exclude those aged 65 and above)
notif_adults <- filter(notification, year == report_year - 1) %>%

  select(iso3,
         newrel_f15plus,
         newrel_f65,
         newrel_m15plus,
         newrel_m65) %>%

  mutate(newrel_adult = newrel_f15plus + newrel_m15plus - NZ(newrel_f65) -  NZ(newrel_m65) ) %>%

  select(iso3,
         newrel_adult) %>%

  # Join to the adult population
  inner_join(pop_adults, by = "iso3")

f4.9_data <- f4.9_data %>%

  inner_join(notif_adults, by = "iso3") %>%

  # Calculate notification rate ratio
  # Use as.numeric() to avoid integer overflow
  mutate(nrr = ifelse(NZ(hcw_tot) > 0 & NZ(newrel_adult) > 0,
                      (as.numeric(hcw_tb_infected) * as.numeric(e_pop_adult))
                      /
                        (as.numeric(hcw_tot) * as.numeric(newrel_adult)),
                      NA)) %>%

  # in previous years I had filtered out countries with fewer than 100 health care workers
  # as the rate ratios jumped around a lot but because these are very small countries they
  # don;t show up in the maps so won't bother anymore

  # Assign the categories for the map
  mutate(var = cut(nrr,
                   c(0, 1, 2, 3, Inf),
                   c('0-0.9', '1-1.9', '2-2.9', '\u22653'),
                   right=FALSE))

# Summary for the text
f4.9_txt <- filter(f4.9_data, hcw_tb_infected>0) %>%
  summarise(tot_hcw_tb = sum(hcw_tb_infected, na.rm=TRUE),
            countries_hcw_tb = n())

# Add number with nrr more than one when number of TB among hcw is 5 or more
f4.9_txt <- filter(f4.9_data, nrr > 1 & hcw_tb_infected >= 5) %>%
  summarise(countries_nrr = n()) %>%
  cbind(f4.9_txt)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Create dataframe for figure 4.10 ----
# (Map showing BCG immunisation policies)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load external file sent by BCG Atlas Project to Dennis
f4.10_data <- read.csv(here('csv/BCG_policy_2021-07-20.csv')) %>%

  select(-cat) %>%

  # filter out countries with no policy
  filter(var %in% 1:2) %>%

  # Add category names for the map legend
  mutate(var = factor(var,
                      levels = c(1, 2),
                      labels = c("BCG vaccination for all", "BCG for special groups")))

# summary for the text
f4.10_txt <- read.csv(here('csv/BCG_policy_2021-07-20.csv')) %>%
  # filter out countries with no policy
  filter(var %in% 1:2) %>%
  # filter out countries not in the TB database
  filter(iso3 %in% strategy$iso3) %>%
  group_by(var) %>%
  summarise(countries = n()) %>%
  pivot_wider(names_from = var,
              names_prefix = "countries_",
              values_from = countries)

# Also add summary of stats from the GHO
## We want indicator code WHS4_543
# bcg <- gho_data("WHS4_543") %>%
#   filter(TimeDim >= 2019 & SpatialDimType=="COUNTRY") %>%
#   select(iso3 = SpatialDim,
#          year = TimeDim,
#          value = Value) %>%
#   mutate(value = as.integer(value)) %>%
#   pivot_wider(names_from = year,
#               names_prefix = "bcg_",
#               values_from = value) %>%
#   mutate(diff_20_19 = bcg_2020 - bcg_2019)

# There was a problem with the OData service for GHO on 20th Septmeber so instead did this
# the old-fashioned way by downloading the CSV file from
# https://www.who.int/data/gho/data/indicators/indicator-details/GHO/bcg-immunization-coverage-among-1-year-olds-(-)

bcg <- read.csv(here("csv/gho_bcg_2021-09-20.csv")) %>%
  filter(Period >= 2019 & toupper(Location.type)=="COUNTRY") %>%
  select(iso3 = SpatialDimValueCode,
         year = Period,
         value = Value) %>%
  mutate(value = as.integer(value)) %>%
  pivot_wider(names_from = year,
              names_prefix = "bcg_",
              values_from = value) %>%
  mutate(diff_20_19 = bcg_2020 - bcg_2019)


# Calculate totals
# At least 95% coverage in 2020
bcg_txt <- filter(bcg, bcg_2020 >=95) %>%
  summarise(bcg_2020_gte95 = n())

# Reduction of 5% or more between 2019 and 2020
bcg_txt <- filter(bcg, diff_20_19 <= -5) %>%
  summarise(bcg_down_5 = n()) %>%
  cbind(bcg_txt)

# No reporters in 2020 after reporting in 2019
bcg_txt <- filter(bcg, is.na(bcg_2020) & !is.na(bcg_2019)) %>%
  summarise(no_rep_2020 = n()) %>%
  cbind(bcg_txt)
