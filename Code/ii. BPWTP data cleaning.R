###############################################################################
####### cH.1 GAMs -- data processing ##########
####### Danielle Spence ##########
####### Created 3/30/2021 ########
###############################################################################
### Clear memory

rm(list = ls())

library(pacman)
p_load(tidyverse, reshape2, epidm, dplyr, tidyr,
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
         tbl_sheet_year %in% c(1984:2022))


df<- df%>%
  filter(tbl_parameter %in% c("Chlorophyll a","Nitrate","Ammonia N","Phosphate (ortho)","Phosphate (total)","Temperature","Organic N"))

df<- df%>% dplyr::select(-tbl_station)


## Pivot wider to make parameters into columns

d2<- df %>%
  tidyr::pivot_wider(names_from = "tbl_parameter", values_from = "tbl_result",
                     values_fn = mean)


#d2<- df %>%
#  dplyr::mutate(row = row_number()) %>%
#  tidyr::pivot_wider(names_from = "tbl_parameter", values_from = "tbl_result") %>%
#  dplyr::select(-row)



d2 <- d2%>%
  dplyr::rename(
    Date = "tbl_date_ymd",
    year = "tbl_sheet_year",
    Chla_ug.L = "Chlorophyll a",
    NO3_mg.L = "Nitrate",
    NH3_mg.L = "Ammonia N",
    SRP_ug.L = "Phosphate (ortho)",
    TP_ug.L = "Phosphate (total)",
    Temp_C = "Temperature",
    Org_N_mg.L = "Organic N",
    )


## load in missing data 2001–2004

miss <- read_csv("Data/Raw data/MASTERFILEWTPStandardMeasurementsWORKINGApr26_2018.csv")

miss<- miss %>%
  dplyr::filter(year >= 2001 & year <= 2004)%>%
  dplyr::select(SampleDateTime, year, temp, ammn, chla, no3, orgn, orthop, tp)%>%
  dplyr::rename(Date = "SampleDateTime",
         Chla_ug.L = "chla",
         NO3_mg.L = "no3",
         NH3_mg.L = "ammn",
         SRP_ug.L = "orthop",
         TP_ug.L = "tp",
         Temp_C = "temp",
         Org_N_mg.L = "orgn")

miss$Date <- as.Date(miss$Date, "%m/%d/%Y")

full_df<- rbind(d2, miss)
full_df <- full_df[with(full_df, order(Date)),]

full_df <- full_df[!duplicated(full_df[c('Date')]),]


## Add flow data

flow <- read_csv("Data/daily flow.csv")

flow<- flow %>%
  select(date, combined_05JG004.cms, SK05JG006.cms, RC_IC_cms)%>%
  dplyr::rename("Date" = date)


full_df<- merge(full_df, flow, by="Date", all=TRUE)


full_df <- full_df[rowSums(is.na(full_df)) <= 9, ]       # Apply rowSums & is.na


write.csv(full_df, file="data/bpgamdataCLEAN_flow.csv", row.names=F)


my_summary_data <- full_df %>%
  group_by(year) %>%
  summarise(Count = n())   

