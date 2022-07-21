
# yale data

library(tidyverse) 


# library(googledrive)

# drive_find(n = 10) 

# drive_download(file = as_id("1-bRDuN8yUV8r0ptBYmB2aeMs8IKO8UWh"),
#                type = "csv", overwrite = T) 
# 
# drive_download(file = as_id("1XkNqMEVIr_ee4uyQXeCnEp8yq1Qru7VD"),
#                type = "csv", overwrite = T) 

# codebook: https://climatecommunication.yale.edu/visualizations-data/ycom-us-2016/ 
ycom_2016 <- read_csv("YCOM_2016_Data.01.csv") %>% mutate(year = 2016)

# codebook: https://climatecommunication.yale.edu/visualizations-data/ycom-us-2018/ 
ycom_2018 <- read_csv("YCOM_2018_Data.csv") %>% mutate(year = 2018) 

vars <- tibble(vars = names(ycom_2016), year_2016 = 1) %>% 
  full_join(tibble(vars = names(ycom_2018), year_2018 = 1), by = "vars") %>% 
  mutate(both = year_2016 + year_2018) %>% 
  filter(both == 2) %>% 
  pull(vars)

# states <- unique(ycom_2016 %>% filter(GeoType == "State") %>% pull(GeoName))

yale <- ycom_2016 %>% 
  select(all_of(vars)) %>% 
  mutate(reducetax = NA_real_, 
         reducetaxOppose = NA_real_) %>% 
  bind_rows(ycom_2018 %>% 
              select(all_of(vars), reducetax, reducetaxOppose)) %>% 
  mutate(state = case_when(GeoType == "State" ~ GeoName,
                           GeoType == "County" ~ sub(".*, ", "", GeoName),
                           T ~ NA_character_)) 



rm(ycom_2016, ycom_2018, vars) 
