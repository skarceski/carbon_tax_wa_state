
# yale program on climate change communication data

library(tidyverse) 

# 2016 codebook: https://climatecommunication.yale.edu/visualizations-data/ycom-us-2016/ 
# 2018 codebook: https://climatecommunication.yale.edu/visualizations-data/ycom-us-2018/ 

yale <- read_csv("data/YCOM_2016_Data.01.csv") |> 
  mutate(year = 2016) |> 
  rename(county_fips = GEOID)

county_match <- yale |> 
  filter(GeoType == "County") |> 
  distinct(GeoName, county_fips) 

yale <- yale |> 
  bind_rows(read_csv("data/YCOM_2018_Data.csv") |> 
              left_join(county_match) |> 
              filter(GeoType == "County") |> 
              mutate(year = 2018)) |> 
  filter(GeoType %in% c("County", "State")) |> 
  mutate(row_id = row_number(),
         GeoName = if_else(row_id == 4996, 
                           "Dona Ana County, New Mexico", GeoName),
         state = case_when(GeoType == "State" ~ GeoName,
                           GeoType == "County" ~ sub(".*, ", "", GeoName),
                           T ~ NA_character_),
         county_fips = str_pad(county_fips, width = 5, 
                               side = "left", pad = "0")) 

county_match <- yale |> 
  filter(GeoType == "County") |> 
  distinct(county_fips, state)
