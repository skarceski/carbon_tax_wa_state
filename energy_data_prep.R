
# eia data prep
# data source: https://www.eia.gov/electricity/data/state/ 

library(tidyverse)

eia_consume <- read_csv("annual_generation_state_eia.csv") %>% 
  select(1:5) %>% 
  filter(year == 2018, 
         type_of_producer == "Total Electric Power Industry") %>% 
  select(-type_of_producer) %>% 
  mutate(energy_source = case_when(energy_source == "Hydroelectric Conventional" ~ "hydro",
                                   energy_source == "Natural Gas" ~ "nat_gas",
                                   energy_source == "Other Biomass" ~ "other_biomass",
                                   energy_source == "Other Gases" ~ "other_gas",
                                   energy_source == "Pumped Storage" ~ "pumped_storage",
                                   energy_source == "Solar Thermal and Photovoltaic" ~ "solar",
                                   energy_source == "Wood and Wood Derived Fuels" ~ "wood_fuel",
                                   T ~ energy_source),
         energy_source = tolower(energy_source)) %>% 
  pivot_wider(names_from = energy_source, values_from = gen_mwatthrs) %>% 
  mutate(across(coal:geothermal,
                ~ ifelse(is.na(.), 0, .))) %>% 
  mutate(non_co2_electricity = (nuclear + hydro + wind + solar + geothermal)/total,
         non_co2_w_bio = (nuclear + hydro + wind + solar + geothermal + wood_fuel + 
                            other_biomass)/total) 

