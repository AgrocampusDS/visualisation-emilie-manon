library(dplyr)
library(tidyr)

# WGI calculation 
data <- read.csv("./data/wgidataset.csv")
d <- data[,c("countryname", "year", "vae","pve","gee","rqe","rle","cce")]
d <- gather(d, "indicator", "value", vae:cce)

## each country has 20 years of data
d_gap_fill  <- d %>%
  group_by(countryname, year) %>%
  mutate(NA_count_c_y = sum(is.na(value))) %>% # gf record: NA values within a region/year prior to gapfilling, max value is 6 (meaning that a country has no data)
  ungroup() %>%
  group_by(countryname) %>% # gapfill missing data with mean of values across years within the same region/indicator
  mutate(ind_mean_c_i = mean(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(value = ifelse(is.na(value), ind_mean_c_i, value)) %>%
  group_by(countryname, year) %>% 
  mutate(NA_count_post_gf1 = sum(is.na(value))) # gf record: NA values within a region/year after within region/indicator gapfilling (i.e. indicator is gapfilled by other years of data), used to cut regions <4 indicators (below)

# Cut countries with less than 4 indicators
countries_no_data <- d_gap_fill %>%
  filter(NA_count_post_gf1 > 3)

countries_no_data <- unique(countries_no_data$countryname)
countries_no_data

## In this case, the countries with minimal data (< 4 indicators ever calculated) are deleted.  
## These will be gap-filled later on if they are deleted now.
d_gap_fill <- d_gap_fill %>%
  filter(!(countryname %in% countries_no_data))


# Calculate overall WGI for each country 
d_calcs <- d_gap_fill %>%
  group_by(countryname, year) %>%
  summarize(score_wgi_scale = mean(value, na.rm=T),
            NA_start = mean(NA_count_c_y), # initial mean number of NA across indicators, pre-gapfill 
            NA_post_gf_1 = mean(NA_count_post_gf1)) %>% # number of NA across indicators, post-gapfill across year gapfill within region/indicator
  ungroup() 