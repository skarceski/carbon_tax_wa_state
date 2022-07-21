
source("yale_pccc_prep.R") 
source("energy_data_prep.R") 

z_score <- function(x){
 
  (x - mean(x, na.rm = T))/sd(x, na.rm = T)
   
}

yale <- yale %>% 
  left_join(read_csv("state_level_taxes.csv") %>% 
              filter(year == 2018) %>% 
              mutate(policy = case_when(
                policy == "Average gasoline price for the state" ~ "gas_price",
                policy == "Combined state and local sales taxes" ~ "sl_sales",
                policy == "State gasoline tax" ~ "gas_tax",
                policy == "State beer excise tax" ~ "beer_tax",
                policy == "State cigarette excise tax" ~ "cigs_tax",
                policy == "State distilled liquor tax" ~ "liq_tax",
                policy == "State wine excise tax" ~ "wine_tax",
                T ~ NA_character_)) %>% 
              select(state, policy, tax_amount) %>% 
              group_by(policy) %>% 
              mutate(tax_amount = ifelse(is.na(tax_amount), 0, tax_amount),
                     tax_amount = z_score(tax_amount),
                     state = ifelse(state == "D.C.", 
                                    "District of Columbia", state)) %>% 
              ungroup() %>% 
              filter(!is.na(policy)) %>%
              pivot_wider(names_from = policy, values_from = tax_amount), 
            by = "state") %>% 
  mutate(sin_tax = (beer_tax + cigs_tax + liq_tax + wine_tax)/4)

###############################################################################

# acs and election data 

source("acs_prep.R")

acs_pres <- acs_county_2018 %>% 
  left_join(read_csv("countypres_2000-2020.csv") %>% 
              filter(year == 2016, party %in% c("DEMOCRAT", "GREEN")) %>% 
              group_by(county_fips) %>% 
              summarize(dem_green = sum(candidatevotes)/mean(totalvotes)) %>% 
              select(county_fips, dem_green), 
            by = "county_fips") %>% 
  rename(county = NAME)

read_csv("state_1976-200_president_mitdl.csv") %>% 
  filter(year == 2016, writein == FALSE, 
         party_detailed %in% c("DEMOCRAT", "GREEN")) %>% 
  group_by(state, state_po) %>% 
  summarize(dem_green = sum(candidatevotes)/mean(totalvotes)) %>% 
  ungroup() %>% 
  select(st = state_po, dem_green)

# merge it all together 

states <- tibble(state_abb = c(state.abb, "DC"),
                 state = c(state.name, "District of Columbia")) 

county_data <- acs_pres %>% 
  left_join(yale %>% 
              filter(year == 2018, GeoType == "County") %>% 
              rename(county = GeoName) %>% 
              mutate(county = ifelse(str_detect(county, "Do_a Ana County"),
                                                "Doña Ana County, New Mexico",
                                                county))) %>% 
  mutate(med_inc_k = med_income/1000) %>% 
  filter(!str_detect(county, "Puerto Rico")) %>% 
  left_join(eia_consume %>% 
              left_join(states, by = c("state" = "state_abb")) %>% 
              rename(state_abb = state, state = state.y)) 

rm(v18, acs_county_2018, acs_pres, yale, eia_consume, states)

