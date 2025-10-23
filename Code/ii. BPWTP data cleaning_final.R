###############################################################################
####### cH.1 GAMs -- data processing ##########
####### Danielle Spence ##########
####### Created 3/30/2021 ########
###############################################################################
### Clear memory

rm(list = ls())

library(pacman)
p_load(tidyverse, reshape2, epidm, dplyr, tidyr, readxl, janitor,
       install = TRUE)
library(dplyr)

setwd("C:/Users/danis/OneDrive/R/BPWTP Chla GAMs")

## Read in data 

df <- read_csv("Data/Raw data/bpwtp_labdat_db.csv")
## pull out response variable and potential explanatory variables 

## select only raw data

df <- df%>%
  dplyr::select(tbl_date_ymd, tbl_parameter, tbl_result, tbl_station, tbl_sheet_year)

df<- df%>%
  filter(tbl_station == "Raw",
         tbl_sheet_year %in% c(1984:2022))%>%
  rename(year = tbl_sheet_year)%>%
  rename_with(~ sub("^tbl_", "", .), starts_with("tbl_"))



df<- df%>%
  filter(parameter %in% c("Chlorophyll a","Nitrate","Ammonia N","Phosphate (ortho)","Phosphate (total)","Temperature","Organic N"))

df<- df%>% dplyr::select(-station)

parameter_summary <- df %>%
      dplyr::filter(parameter == "Chlorophyll a") %>%
  dplyr::summarise(
            mean = mean(result, na.rm = TRUE),
            median = median(result, na.rm = TRUE),
            sd = sd(result, na.rm = TRUE),
            min = min(result, na.rm = TRUE),
            max = max(result, na.rm = TRUE),
            count = n()
        )

 print(parameter_summary)

 # # A tibble: 1 × 6
 # mean median    sd   min   max count
 # <dbl>  <dbl> <dbl> <dbl> <dbl> <int>
 #   1  20.8     14  21.3     0   167  1219

 # add missing NH3 data


missing_2001 <- read_excel("Data/Raw data/ROUTINE LAB DATA 2001(v2).xlsx",
                           sheet = "Weekly Data", col_names = TRUE, range = "A8:BD67") %>% 
  filter(Parameters %in% c("Nitrate","Ammonia N","Phosphate (ortho)","Phosphate (total)","Temperature","Organic N", "Chlorophyll a")) %>% 
  select(-c("...3":"...4")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>% 
  mutate_at(c("date_ymd", "result"), as.numeric) %>% 
  mutate(date_ymd = excel_numeric_to_date(date_ymd),
         year = year(date_ymd)) %>% 
  select(parameter = Parameters, date_ymd, year, result) 

bp_longterm_sans2001 <- df %>% filter(!year == 2001) 



bp_longterm_infill <- bind_rows(bp_longterm_sans2001, missing_2001) %>% 
  arrange(date_ymd)



## Pivot wider to make parameters into columns


d2<- bp_longterm_infill %>%
  tidyr::pivot_wider(names_from = "parameter", values_from = "result",
                     values_fn = mean)



d2 <- d2%>%
  dplyr::rename(
    Date = "date_ymd",
    Chla_ug.L = "Chlorophyll a",
    NO3_mg.L = "Nitrate",
    NH3_mg.L = "Ammonia N",
    SRP_ug.L = "Phosphate (ortho)",
    TP_ug.L = "Phosphate (total)",
    Temp_C = "Temperature",
    Org_N_mg.L = "Organic N",
    )

 parameter_summary <- d2 %>%
   dplyr::summarise(
     mean = mean(Chla_ug.L, na.rm = TRUE),
     median = median(Chla_ug.L, na.rm = TRUE),
     sd = sd(Chla_ug.L, na.rm = TRUE),
     min = min(Chla_ug.L, na.rm = TRUE),
     max = max(Chla_ug.L, na.rm = TRUE),
     count = n()
   )
 
 print(parameter_summary)
# 
# # A tibble: 1 × 6
# mean median    sd   min   max count
# <dbl>  <dbl> <dbl> <dbl> <dbl> <int>
#  20.9     14  21.3     0   167  2025


# Remove weeks with incomplete data
d2 <- d2[rowSums(is.na(d2)) <= 5, ]  


## Add flow data

flow <- read_csv("Data/daily flow.csv")

flow<- flow %>%
  select(date, combined_05JG004.cms, SK05JG006.cms, RC_IC_cms)%>%
  dplyr::rename("Date" = date)


full_df<- merge(d2, flow, by="Date", all=TRUE)


full_df <- full_df[rowSums(is.na(full_df)) <= 7, ]       # Apply rowSums & is.na

full_df <- mutate(full_df,
             DOY = as.numeric(format(Date,'%j')),
             nMonth = as.numeric(format(Date,'%m')))


 parameter_summary <- full_df %>%
   dplyr::summarise(
     mean = mean(Chla_ug.L, na.rm = TRUE),
     median = median(Chla_ug.L, na.rm = TRUE),
     sd = sd(Chla_ug.L, na.rm = TRUE),
     min = min(Chla_ug.L, na.rm = TRUE),
     max = max(Chla_ug.L, na.rm = TRUE),
     count = n()
   )
 print(parameter_summary)

# mean      median       sd  min max count
# 20.9       14         21.3  0   167  1267


write.csv(full_df, file="data/bpgamdataCLEAN_flow_noimput.csv", row.names=F)

