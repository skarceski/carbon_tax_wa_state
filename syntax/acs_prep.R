
library(tidyverse) 
library(tidycensus) 

# 2018 ACS five year 

codebook <- load_variables(2018, "acs5", cache = T) 
race_vars <- codebook |> 
  filter(concept == "RACE") |> 
  pull(name)
ed_vars <- codebook |> 
  filter(str_detect(concept, 
                    "^EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS")) |> 
  pull(name) 

acs_county_2018 <- get_acs(
  geography = "county",
  year = 2018,
  variables = c(
    race_vars,
    ed_vars,
    "B21004_001", # median income
    "B19013_001", # median HH inc
    "B01002_001", # median age
    "B25077_001", # median home value
    "B08136_002", # aggregate travel time to work by car 
    "B01001I_001", # Hispanic or Latino 
    "B25018_001", # median number of rooms per household
    "C24050_001", # employment-industry baseline 
    "C24050_002", # agricultural employment 
    "C24050_003", # construction employment 
    "C24050_004", # manufacturing employment 
    "C24050_007", # transportation employment 
    "C24050_010", # professional employment 
    "C24050_005", # wholesale trade employment 
    "C24050_006" # retail trade employment 
    )) |>
  select(-moe) |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  mutate(r_white = B02001_002/B02001_001,
         r_af_am = B02001_003/B02001_001,
         r_am_ind = B02001_004/B02001_001,
         r_asian = B02001_005/B02001_001,
         r_nh_pi = B02001_006/B02001_001,
         r_other = B02001_007/B02001_001,
         r_multiple = B02001_008/B02001_001,
         r_hisp = B01001I_001/B02001_001,
         ind_construct = C24050_003/C24050_001, 
         ind_transport = C24050_007/C24050_001, 
         ind_ag = C24050_002/C24050_001, 
         ind_pmc = C24050_010/C24050_001, 
         ind_trade = (C24050_005 + C24050_006)/C24050_001, 
         ed_hs_less = (B15003_002 + B15003_003 + B15003_004 + B15003_005 +
                         B15003_006 + B15003_007 + B15003_008 + B15003_009 +
                         B15003_010 + B15003_011 + B15003_012 + B15003_013 +
                         B15003_015 + B15003_016)/B15003_001,
         ed_hs = (B15003_017 + B15003_018 + B15003_019 + B15003_020)/B15003_001,
         ed_associates = B15003_021/B15003_001,
         ed_bachelors = B15003_022/B15003_001,
         ed_masters = B15003_023/B15003_001,
         ed_professional = B15003_024/B15003_001,
         ed_doctorate = B15003_025/B15003_001,
         year = 2018 ) |>
  select(year, county = NAME, county_fips = GEOID, median_income = B21004_001,
         med_hh_income = B19013_001, median_age = B01002_001,
         med_home_val = B25077_001, r_white:ed_doctorate)

rm(codebook, ed_vars, race_vars) 

save(acs_county_2018, file = "data/acs_county_2018.RData")
