# Purpose: Determine what share of IPs constitute 80% of all targets by OU
# Author: Tim Essam
# Date: 2020-02-26
# Notes: Relies on HFR and FY20 MER Targets at the site level

# DEPENDENCIES -------------------------------------------------------
library(tidylog)
library(tidyverse)
library(purrr)
library(scales)
library(tidytext)
library(janitor)
library(readxl)
library(vroom)
library(Wavelength)


# GLOBALS -----------------------------------------------------------

datain <- "Data"
dataout <- "Dataout"

  prinf <- function(df) {
    print(df, n = Inf)
  }

# Target threshold for important flag cut off, using 81%
  thresh_tgt <- 0.81

# Move facets to the left and set theme to be minimal
  left_facets <- function() {
    ggplot2::theme_minimal() +
      theme(strip.text = element_text(hjust = 0, size = 10))
  }


# Validator to check that shares do not add to more than 1
  check_shares <- function(df, stub = "_sh") {
    share_max <- df %>%
      select(contains(stub)) %>%
      summarise(max(.)) %>%
      unlist()
    cat("\nMax share value is:", share_max)
  }


# IMPORT ------------------------------------------------------------

  mer_path <- file.path(datain, "FY20Q0")
  ou_list <- dir(file.path(mer_path), pattern = "*.csv", full.names = TRUE)
  
  df_mer <- map_dfr(
    .x = ou_list,
    .f = ~ read.csv(.x)
  )

# For referencing the join fields between the targerts and HFR
  hfr_path <- "/Users/tim/Downloads/HFR_2020_inprocess_20200211.csv"
  hfr <- vroom(hfr_path)
  
  # Pulling out start date to expand MER base data to size of HFR -- want to track missing reporting periods
  start_date <- hfr %>%
    select(date) %>%
    summarise(start_date = min(date)) %>% 
    pull()
  
  weeks <- hfr %>% distinct(date) %>% add_tally() %>% summarise(weeks = max(n)) %>% pull()
  dates <- lubridate::as_date(start_date) %>% seq(by = 7, length.out = weeks)

# MUNGE AND GENERATE IMPORTANCE FLAGS ------------------------------------

# Pick an ou, what sites consistute 80% of total target
# ou, country  
# Grouping on the orgunituid to determine level of granularity
  df_tarwt <-
    df_mer %>%
  
    # Create indicator target counts by orgunitid -- these used to create shares
    group_by(operatingunit, indicator, orgunituid, mech_code, mech_name) %>%
    summarise(org_indic_targets = sum(mer_targets, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(operatingunit, indicator) %>%
    mutate(
      ou_indic_targets = sum(org_indic_targets, na.rm = TRUE),
      org_indic_tgtsh = org_indic_targets / ou_indic_targets
    ) %>%
    add_tally(name = "site_count") %>% 
    ungroup() %>% 

    # Arrange the resulting share calculations in descending order so we can flag those
    # forming the 80% of targets; use thresh_tgt variable to adjust threshold
    arrange(operatingunit, indicator, desc(org_indic_tgtsh)) %>%
    group_by(operatingunit, indicator) %>%
    mutate(
      run_sum = cumsum(org_indic_tgtsh),
      impflag = if_else(run_sum <= thresh_tgt, 1, 0),
      impflag_count = sum(impflag),
      impflag_sh = impflag_count / site_count
    ) %>%
    ungroup() %>% 
    group_by(operatingunit, orgunituid, indicator) %>% 
    add_tally(name = "mech_count_by_site") %>% 
    ungroup()
  
 # Summarise the site that have multiple mechanisms per indicator
  df_tarwt %>% 
    filter(mech_count_by_site >1) %>% 
    arrange(operatingunit, indicator, orgunituid) %>% 
    write_csv(., file.path(dataout, "HFR_FY20Q0_DATIM_20191207_orgunituid_multiple_mechs.csv"))
  
    # create a date field to aling MER data to HFR --- based on HFR dates
    df_tarwt_base <- purrr::map_dfr(.x = dates,
      .f = ~dplyr::mutate(df_tarwt, date = .x)) 
    
    
    # Verify that the shares do not add to more than 1
    check_shares(df_tarwt_base, stub = "run_sum")


# Check that targets look reasonable
    df_tarwt_base %>%
    group_by(operatingunit, indicator) %>%
    summarise_at(c("impflag_sh", "impflag_count", "ou_indic_targets"), mean) %>%
    ungroup() %>% 
      mutate(ou_sort = reorder_within(operatingunit, impflag_count, indicator),
        indic_sort = fct_reorder(indicator, impflag_count, .desc = TRUE)) %>% 
    ggplot(aes(y = impflag_count, x = ou_sort, fill = indicator)) +
      geom_col() + coord_flip() +
      facet_wrap(~indicator, scales = "free_y") +
      scale_x_reordered() +
      theme_minimal() +
      left_facets() +
      scale_fill_ordinal() +
      theme(panel.grid.major = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.position = "none") +
      labs(y = "Number of high target sites", x = "", 
        title = "Number of high target sites needed to reach 80% of OU targets")



# PLOT TARGET WEIGHTS -----------------------------------------------------

# Take a look at results across ous and indicators
    df_tarwt_base %>%
      group_by(operatingunit, indicator) %>%
      summarise(
        impflag_sh = mean(impflag_sh),
        impflag_count = mean(impflag_count)
      ) %>%
      ungroup() %>%
      mutate(ou_sort = reorder_within(operatingunit, impflag_sh, indicator)) %>%
      ggplot(aes(x = ou_sort, y = impflag_sh)) +
      geom_col() +
      coord_flip() +
      geom_text(aes(label = ifelse(impflag_count > 0, str_c("  ", impflag_count, " "), NA)),
        hjust = 0, size = 3, color = llamar::grey75K
      ) +
      scale_x_reordered() +
      scale_y_continuous(label = percent_format(accuracy = 1)) +
      facet_wrap(~indicator, scales = "free_y") +
      left_facets() +
      labs(
        x = "", y = "",
        title = "How many sites does it take to represent 80% of targets across OUs."
      )



# MERGE WITH HFR ----------------------------------------------------------
# Check overlap in the names between two datasets
  intersect(names(hfr), names(df_tarwt_base))

  hfr_wts <- 
    df_tarwt_base %>%
    left_join(., hfr, by = c("orgunituid", "indicator", "operatingunit", "date")) %>% 
    mutate(mech_flag = if_else(mech_code.x != mech_code.y, 1, 0),
      val_flag = if_else(!is.na(val), 1, 0))
  
  hfr_wts %>% 
    count(mech_flag, val_flag)
  
  # Subsetting those records that do not have matching mechanism ids and and are reporting
  mech_miss_df <- 
    hfr_wts %>% 
    filter(mech_flag == 1 & val_flag == 1) %>% 
    select(operatingunit, date, indicator, orgunituid, 
      mech_code.x, mech_code.y, mech_name.x, mech_name.y,
      val, sex, agecoarse, impflag, impflag_count, impflag_sh,
      everything())
  
  # Collapse HFR down to the orgunituid level to check completeness
  hfr_wts_compress <- 
    hfr_wts %>% 
    mutate(na_flag = if_else(is.na(val), 1, 0)) %>% 
    group_by(operatingunit, indicator, orgunituid, 
      mech_code.x, org_indic_targets, org_indic_tgtsh,
      site_count, run_sum, impflag, impflag_count,
      date, fy, hfr_pd, countryname, orgunit, psnu, snu1) %>% 
    summarise(val_sum = sum(val, na.rm = TRUE),
      na_sum = sum(na_flag),
      na_denom = n(),
      sum_mer_targets = sum(mer_targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(not_reporting = if_else(na_denom == na_sum, 1, 0),
      val_censored = if_else(val_sum >= 0 & not_reporting != 1, val_sum, NA_real_)) 
  
  # What metric can you create at the OU level that will summarise for each indicator, the percent
  # high volume sites reporting?
  # Calculate the number of sites that are imporatnat and not_reporting !=1 and divide by the site count
  tmp <- hfr_wts_compress %>% 
    filter(operatingunit == "Eswatini" & indicator == "HTS_TST") %>% 
    group_by(operatingunit, indicator, date) %>% 
    mutate(reporting_sum = ifelse(not_reporting == 0 & impflag == 1, n(), NA)) 
  
  
  
  # Need a fill that reflect the number reported relative to each indicator  
  hfr_wts_compress %>% 
    filter(operatingunit == "Eswatini" & indicator == "HTS_TST_POS") %>% 
    mutate(org_sort = tidytext::reorder_within(orgunituid, org_indic_targets, indicator)) %>% 
    ggplot(., aes(x = date, y = org_sort, fill = val_censored, colour = factor(impflag))) +
        geom_tile() +
    geom_text(aes(label = val_censored))+
    facet_wrap(~indicator, scales = "free_y") +
    scale_y_reordered() +
        scale_color_manual(values = c("1" = "#808080", "0" = "NA")) +
    theme_minimal() +
    scale_fill_viridis_c(na.value = "#F0F0F0", alpha = 0.85, direction = -1, option = "A")
  
  
# By operating unit 
  
  
  
# Now we need to answer, for those sites that reported. How many of the important sites
# reported within a given week?
# Need a flag for those that are important and didn't report, those that are important
# and did report, those that are not-important and reported, and those that are 
# not-important and did not report.

# Couple things of note
# there are a few orgunituid that are missing
# each date + orgunituid should have two entries -- one male, one female


# First, let's try plotting this to get a sense of who did report and who didnt
  tw_hfr_bts <- 
    tw_hfr %>% 
    filter(operatingunit == "Namibia") %>% 
    group_by(orgunituid, date, indicator, agecoarse) %>% 
    add_tally(name = "date_count") %>% 
    arrange(indicator, orgunituid, date) %>% 
    select(orgunituid, indicator, date, date_count, val, sex, agecoarse, otherdisaggregate,
      mech_code.y, everything()) %>% 
    mutate(imp_cat = case_when(
      impflag == 1 & is.na(val) ~ 2,
      impflag == 1 & !is.na(val) ~ 3,
      impflag == 0 & is.na(val) ~ 1,
      impflag == 0 & !is.na(val) ~ 0,
    )) 
  
  # Need to roll everthing up to site level aggregates before 
  
    
   
    tw_hfr_bts %>% 
      filter(imp_cat != 0 & sex == "Male", mech_flag == 0) %>% 
    mutate(org_sort = fct_reorder(orgunituid, org_indic_targets,)) %>% 
    ggplot(aes(x = date, y = org_sort, fill = factor(imp_cat))) + 
    geom_tile(colour = "white") +
      facet_wrap(~indicator, scales = "free_y")
  



