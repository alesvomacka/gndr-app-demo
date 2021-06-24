library(tidyverse)
library(eurostat)

# Share of female researchers ---------------------------------------------

reseach <- search_eurostat("research")
rd_p_femres <- get_eurostat("rd_p_femres")
rd_p_femres <- label_eurostat(rd_p_femres)

rd_p_femres <- 
  rd_p_femres %>%
  filter(!str_detect(geo, "European Union|Euro")) %>% 
  mutate(time     = str_extract(time, "^[:digit:]{4}"),
         selected = ifelse(geo == "Czechia", "Czechia", "Other Eurostat countries"))

write_rds(rd_p_femres, "data/rd_p_femres.rds")

# Obtained research funding -----------------------------------------------
earn_gr_hgpg <- get_eurostat("earn_gr_hgpg")

earn_gr_hgpg <- earn_gr_hgpg %>% 
  mutate(time = str_extract(time, "^[:digit:]{4}")) %>% 
  filter(time == 2006) %>% 
  select(-unit) %>% 
  rename("pay_gap" = "values")

rd_p_femres <- get_eurostat("rd_p_femres")

rd_p_femres <- rd_p_femres %>%
  mutate(time = str_extract(time, "^[:digit:]{4}")) %>% 
  filter(time == 2006) %>% 
  select(-unit) %>% 
  rename("female_share" = "values")

rd_p_femres <- left_join(rd_p_femres, earn_gr_hgpg)
rd_p_femres <- 
  rd_p_femres %>% 
  mutate(sectperf = case_when(sectperf == "BES" ~ "Business enterprise sector",
                              sectperf == "GOV" ~ "Government sector",
                              sectperf == "HES" ~ "Higher education sector",
                              sectperf == "PNP" ~ "Private non-profit sector",
                              sectperf == "TOTAL" ~ "All sectors")) %>% 
  group_by(geo, sectperf) %>% 
  slice_max(female_share)

write_rds(rd_p_femres, "data/pay_gap.rds")
