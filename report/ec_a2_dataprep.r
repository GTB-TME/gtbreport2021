# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data preparation script for ec_a2.Rmd
# Hazim Timimi, August 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load data packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(dplyr)
library(stringr)
library(here)

# Establish dates of second snapshot and when SDG indicators were updated ---
# and the high income group definition two years ago
# (by hand because the date is not easily accessible in the database) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
snapshot_date <- "9 August 2021"
sdg_update_date <- "30 June 2021"
hic_prev_threshold <- "12 536" #value in 2019


# Set datestamps of CSV files to use ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
csv_datestamp <- '2021-08-11'


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
external_indicator_defs <- get_timestamped_csv('sdgdef', csv_datestamp)



# Establish the report year ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
report_year <- 2021

# Create a set of WHO region short names to use in figures and tables
who_region_shortnames <- data.frame(g_whoregion = c("AFR", "AMR", "EMR", "EUR", "SEA", "WPR"),
                                    entity = c("Africa", "The Americas", "Eastern Mediterranean",
                                               "Europe", "South-East Asia", "Western Pacific"))



# Create dataframe for table E2_1 (reporting rates) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

reporting_countries <- data_collection %>%
  filter(datcol_year == report_year & dc_form_description != 'Not requested') %>%
  select(iso3, g_whostatus)

reporting_rates <- notification %>%
  filter(year == report_year - 1) %>%

  # define reported as having a non-empty value for notified cases of TB, RR-TB or HIV-positive TB
  mutate(reported = ifelse(is.na(c_notified) & is.na(conf_rrmdr) & is.na(newrel_hivpos),
                           0,
                           1)) %>%
  select(iso3, g_whoregion, reported) %>%
  inner_join(reporting_countries) %>%

  # ignore the distinction between associate members and non members of WHO
  mutate(g_whostatus = ifelse(g_whostatus == "M",
                              "M",
                              NA)) %>%

  # calculate summaries by region and membership of WHO
  group_by(g_whoregion, g_whostatus) %>%
  summarise(total = n(),
            reported = sum(reported, na.rm = TRUE) )

# Create summary for all countries and areas by region
reporting_rates_tot <- reporting_rates %>%
  group_by(g_whoregion) %>%
  summarise_at(vars(total, reported),
               sum,
               na.rm = TRUE)

# Create a summary for WHO member states by region
reporting_rates_ms <- reporting_rates %>%
  filter(g_whostatus == "M") %>%
  select(g_whoregion,
         total_ms = total,
         reported_ms = reported)


# Combine the two summaries into one summary table
reporting_rates_tab <- reporting_rates_tot %>%
  inner_join(reporting_rates_ms,
             by="g_whoregion") %>%

  # Add the short WHO region names
  inner_join(who_region_shortnames,
             by="g_whoregion") %>%
  select(-g_whoregion)


# Calculate a global summary
reporting_rates_glob <- reporting_rates_tab %>%
  select(-entity) %>%
  summarise_all(sum) %>%
  mutate(entity = "Global")

# Add the global summary to the summary table
te2.1_data <- rbind(reporting_rates_tab, reporting_rates_glob)



# Create dataframe for table E2_2 (external indicators)  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# To ensure consistency of sorting of indicators from one report to the next, force it manually here

external_indicator_order <- data.frame(indicator_id = c("SI_POV_DAY1", "per_allsp.cov_pop_tot",
                                                        "SN.ITK.DEFC.ZS", "MDG_0000000029",
                                                        "NCD_GLUC_04", "SA_0000001462",
                                                        "M_Est_smk_curr_std", "UHC_INDEX_REPORTED",
                                                        "FINPROTECTION_CATA_TOT_10_POP", "GHED_CHE_pc_PPP_SHA2011",
                                                        "EG.CFT.ACCS.ZS", "NY.GDP.PCAP.PP.KD",
                                                        "SI.POV.GINI", "EN_LND_SLUM"),
                                       sort_order = seq(1:14))

te2.2_data <- external_indicator_defs %>%
  inner_join(external_indicator_order, by = "indicator_id") %>%
  arrange(sort_order) %>%

  # There seems to be a weird character in one of the definition fields that kable doesn't like,
  # Or maybe it interprets int$ as something special. So fudge by hand
  mutate(name_at_source = ifelse(indicator_id == "GHED_CHE_pc_PPP_SHA2011",
                                 "Current health expenditure (CHE) per capita in PPP int $",
                                 name_at_source)) %>%

  # Replace ">=" with a greater than or equal to entity code in indicator names
  mutate(display_name_in_profile = str_replace_all(display_name_in_profile, ">=", "&ge;"),
         name_at_source = str_replace_all(name_at_source, ">=", "&ge;")) %>%

  # Minor tweaks needed to the SDG number fields to match what was published previously
  mutate(sdg_number = ifelse(indicator_id %in% c("MDG_0000000029",
                                                 "NCD_GLUC_04",
                                                 "M_Est_smk_curr_std",
                                                 "NY.GDP.PCAP.PP.KD",
                                                 "SI.POV.GINI"),
                             paste(sdg_number,
                                   "<i>(alternative)</i>"),
                             sdg_number)) %>%
  mutate(sdg_number = ifelse(indicator_id == "SA_0000001462", "3.5.2 <i>(alternative)</i>", sdg_number)) %>%
  mutate(sdg_number = ifelse(indicator_id == "GHED_CHE_pc_PPP_SHA2011", "3.8.2 <i>(alternative)</i>", sdg_number))


# I dropped the approach of saving to a .rda file. Instead, this script is now sourced by
# ec_a2.rmd
