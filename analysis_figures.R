
source("yale_pccc_prep.R")

z_score <- function(x){
  
  (x - mean(x, na.rm = T))/sd(x, na.rm = T)
  
} 

################################################################################
# FIGURE 1
################################################################################

library(tidycensus)

# state level data for figure 1 

state_data <- yale %>% 
  filter(year == 2016,
         GeoType == "State") %>% 
  select(state = GeoName, pop = TotalPop, happening, worried, regulate) %>% 
  left_join(tibble(st = c(state.abb, "DC"),
                   state = c(state.name, "District of Columbia"))) %>% 
  left_join(read_csv("state_1976-200_president_mitdl.csv") %>% 
              filter(year == 2016, writein == FALSE, 
                     party_detailed %in% c("DEMOCRAT", "GREEN")) %>% 
              group_by(state, state_po) %>% 
              summarize(dem_green = sum(candidatevotes)/mean(totalvotes)) %>% 
              ungroup() %>% 
              select(st = state_po, dem_green)) %>% 
  select(state, st, pop, happening, worried, regulate, dem_green) %>% 
  left_join(read_csv("annual_generation_state_eia.csv") %>% 
              select(1:5) %>% 
              filter(year == 2016, 
                     type_of_producer == "Total Electric Power Industry") %>% 
              select(-type_of_producer) %>% 
              mutate(energy_source = case_when(
                energy_source == "Hydroelectric Conventional" ~ "hydro",
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
              mutate(con_non_co2 = (nuclear + hydro + wind + solar + geothermal)/total,
                     non_co2_w_bio = (nuclear + hydro + wind + solar + geothermal + wood_fuel + 
                                        other_biomass)/total) %>% 
              rename(st = state) %>% 
              select(-year)) %>% 
  mutate(con_ff_pc = (coal + nat_gas + petroleum + other_biomass + wood_fuel + other_gas)/pop,
         con_non_co2_pc = (hydro + solar + wind + nuclear + geothermal)/pop) %>% 
  select(1:7, con_non_co2, con_ff_pc, con_non_co2_pc) %>% 
  left_join(read_csv("eia_state_production.csv") %>% 
              pivot_longer(4:64, names_to = "year", values_to = "btu_est") %>% 
              # mutate(MSN = tolower(MSN)) %>% 
              filter(year == 2016, str_sub(MSN, -1L, -1L) == "B") %>%
              select(-c(year, Data_Status), st = StateCode, MSN, btu_est) %>% 
              pivot_wider(names_from = MSN, values_from = btu_est) %>% 
              select(st, tot_pes = TEPRB, coal = CLPRB, biomass = BFFDB, 
                     non_co2 = NCPRB, nat_gas = NGMPB, crude = PAPRB, 
                     renew = REPRB, wood_waste = WWPRB, nuclear = NUETB) %>%
              mutate(p_ff = coal + nat_gas + crude,
                     p_non_co2 = non_co2 + nuclear) %>% 
              select(st, p_ff, p_non_co2)) %>% 
  left_join(read_csv("state_level_taxes.csv") %>% 
              filter(year == 2016) %>% 
              mutate(policy = case_when(
                policy == "Combined state and local sales taxes" ~ "sl_sales",
                policy == "State gasoline tax" ~ "gas_tax",
                policy == "State beer excise tax" ~ "beer_tax",
                policy == "State cigarette excise tax" ~ "cigs_tax",
                policy == "State distilled liquor tax" ~ "liq_tax",
                policy == "State wine excise tax" ~ "wine_tax",
                T ~ NA_character_),
                tax_amount = as.numeric(tax_amount)) %>% 
              select(state, policy, tax_amount) %>% 
              group_by(policy) %>% 
              mutate(tax_amount = ifelse(is.na(tax_amount), 0, tax_amount),
                     tax_amount = z_score(tax_amount),
                     state = ifelse(state == "D.C.", 
                                    "District of Columbia", state)) %>% 
              ungroup() %>% 
              filter(!is.na(policy)) %>%
              pivot_wider(names_from = policy, values_from = tax_amount) %>% 
              mutate(sin_tax = (liq_tax + wine_tax + beer_tax + cigs_tax)/4) %>% 
              select(state, gas_tax, sin_tax, sl_sales)) %>% 
  mutate(ff_pc = p_ff/pop, 
         non_co2_pc = p_non_co2/pop)

# I want the non_co2 (geothermal, hydro, wind, solar) and nuclear AND 
# the ff production coal, nat_gas, and crude 

# state_data

# a single combined graphic 

# object containing the names of the RGGI states 
RGGI <- c("Connecticut", "Delaware", "Maine", "New Hampshire", 
          "New York", "Vermont", "Massachusetts", 
          "Maryland", "Rhode Island") 

state_data %>% 
  filter(state != "District of Columbia") %>% 
  select(state, con_non_co2, 
         # p_ff, p_non_co2,
         ff_pc, non_co2_pc,
         sl_sales, sin_tax, gas_tax, 
         happening, regulate, dem_green) %>% 
  pivot_longer(2:10, names_to = "type", values_to = "value") %>% 
  group_by(type) %>% 
  mutate(z_score = z_score(value)) %>% 
  ungroup() %>% 
  mutate(cat = case_when(str_detect(type, "tax|sales") ~ "Indirect taxation",
                         str_detect(type, "ff|non_co2") ~ "Energy production/consumption",
                         str_detect(type, "hap|reg|green") ~ "Citizen demand"),
         type = case_when(type == "con_non_co2" ~ "Non-CO2-emitting consumption",
                          type == "regulate" ~ "Support for regulating CO2",
                          type == "happening" ~ "Belief that CC is happening",
                          type == "non_co2_pc" ~ "Non-CO2-emitting prod. (per capita)",
                          type == "ff_pc" ~ "Fossil fuel production (per capita)",
                          type == "dem_green" ~ "Democratic + Green vote share",
                          type == "sl_sales" ~ "State & local sales tax",
                          type == "gas_tax" ~ "Gasoline tax",
                          type == "sin_tax" ~ "Sin tax index"),
         cat = factor(cat, levels = c("Citizen demand", 
                                      "Energy production/consumption", 
                                      "Indirect taxation")),
         st_cat = case_when(state == "Washington" ~ "WA",
                            state == "California" ~ "CA",
                            state %in% RGGI ~ "RGGI",
                            T ~ "Other"),
         st_cat = factor(st_cat, levels = c("WA", "CA", "RGGI", "Other"))) %>% 
  ggplot(aes(x = z_score, y = type, 
             shape = st_cat, color = st_cat, size = st_cat)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("darkgreen", "darkblue", "maroon", "black")) +
  # scale_color_manual(values = c(rep("black", 4))) +
  scale_size_manual(values = c(4, 4, 2, 1)) +
  scale_shape_manual(values = c(16, 18, 15, 20)) +
  facet_wrap(~cat, ncol = 1, scales = "free") +
  labs(y = NULL, x = NULL, shape = NULL, color = NULL, size = NULL) +
  theme_bw() +
  theme(legend.position = "top") 

ggsave("tables_figures/figure_1.tiff", height = 5, width = 7)

################################################################################
# FIGURE 2
################################################################################

library(lme4)
library(sjPlot)
library(broomExtra)

# this handles all the data prep 
source("data_prep.R") 

model_vars <- tibble(term = c("happening", "regulate", "dem_green", 
                              "gas_tax",  "sin_tax", 
                              "sl_sales", "gas_price",
                              "log_pop", 
                              # "r_white", 
                              "r_af_am", "r_am_ind", 
                              "r_asian", "r_nh_pi", "r_other", 
                              "r_multiple", "r_hisp", "med_age", 
                              "med_inc_k", "avg_travel", "med_rooms",
                              "ind_construct", "ind_transport", "ind_ag",
                              "ind_pmc", "ind_trade", "ed_ls_hs", 
                              # "ed_hs", 
                              "ed_associates",
                              "ed_bachelors", "ed_advanced", 
                              "non_co2_electricity"),
                     long = c("CC is happening",
                              "Regulate C02",
                              "'16 Democratic vote %",
                              "Gas tax (ST)",
                              "Sin tax index (ST)",
                              "Avg. S&L sales tax (ST)",
                              "Average gas price (ST)",
                              "log(Population)", 
                              # "R% White",
                              "R% African American",
                              "R% American Indian",
                              "R% Asian",
                              "R% Nat. Hawaiian/PI",
                              "R% Other", 
                              "R% Multiple races",
                              "R% Hispanic/Latinx",
                              "Median age",
                              "Median income",
                              "Average driving time",
                              "Median no. HH rooms",
                              "Emp% Construction",
                              "Emp% Transportation",
                              "Emp% Agriculture",
                              "Emp% Profession/mgmt",
                              "Emp% Trade",
                              "Ed% Less than HS", 
                              # "Ed% High school",
                              "Ed% Associates degree",
                              "Ed% Bachelor degree",
                              "Ed% Advanced degree",
                              "Non-CO2-emitting elect. (ST)"),
                     cat = c(rep("Citizen demand", 3),
                             rep("Indirect taxation", 3),
                             "Energy/Fuel",
                             "Other demographic context",
                             rep("Race/Ethnicity", 7),
                             rep("Other demographic context", 4),
                             rep("Employment", 5),
                             rep("Education", 4),
                             "Energy/Fuel")) 

# standardize all the predictor variables
county_data_z <- county_data %>% 
  mutate(across(all_of(model_vars$term), 
                ~ z_score(.)))

# fit multilevel model 
model <- lmer(reducetax ~ happening + regulate +
                  # tax vars 
                  gas_tax + gas_price + sin_tax + sl_sales + 
                  # add pres vote 
                  dem_green + log_pop + 
                  # add demographics 
                  r_af_am + r_am_ind + r_asian + r_nh_pi + r_other + 
                  r_multiple + r_hisp + med_age +
                  # add economic variables 
                  med_inc_k + avg_travel + med_rooms + 
                  # add industry vars 
                  ind_construct + ind_transport + ind_ag +
                  ind_pmc + ind_trade + 
                  # add education 
                  ed_ls_hs + ed_associates + ed_bachelors + 
                  ed_advanced + non_co2_electricity + 
                  (1|state),
                data = county_data_z) 

# use broomExtra to 'tidy' the results 
output <- tidy(model) %>% 
  filter(effect == "fixed") %>% 
  select(term, estimate, std.error, statistic) %>% 
  left_join(model_vars) %>% 
  mutate(upper = estimate + 1.96*std.error,
         lower = estimate - 1.96*std.error,
         long = fct_reorder(long, estimate))

output %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = estimate, y = long)) +
  geom_vline(xintercept = 0, color = "gray70", linetype = "dashed") +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  size = 0.25) +
  theme_minimal() +
  labs(x = "Standardized effects", 
       y = NULL) +
  scale_y_discrete(position = "left")

ggsave("tables_figures/figure_2.tiff", height = 6, width = 7)

### table for supplementary data 

# create vector with nice names for the table 
var_names = c("(Intercept)",
              "CC is happening",
              "Regulate C02",
              "Gas tax (ST)",
              "Average gas price (ST)",
              "Sin tax index (ST)",
              "Avg. S&L sales tax (ST)",
              "'16 Dem. & Green vote share", 
              "log(Population)", 
              # "R% White",
              "R% African American",
              "R% American Indian",
              "R% Asian",
              "R% Nat. Hawaiian/PI",
              "R% Other", 
              "R% Multiple races",
              "R% Hispanic/Latinx",
              "Median age",
              "Median income",
              "Average driving time",
              "Median no. HH rooms",
              "Emp% Construction",
              "Emp% Transportation",
              "Emp% Agriculture",
              "Emp% Profession/mgmt",
              "Emp% Trade",
              "Ed% Less than HS", 
              # "Ed% High school",
              "Ed% Associates degree",
              "Ed% Bachelor degree",
              "Ed% Advanced degree",
              "Non-CO2-emitting elect. (ST)")

# create table with sjPlot 
tab_model(model,
          show.ci = F, show.se = T, p.style = "stars",
          pred.labels = var_names,
          dv.labels = c("Support for carbon tax"), 
          title = "Support for carbon tax on FF companies", 
          file = "tables_figures/full_model_output.html") 
