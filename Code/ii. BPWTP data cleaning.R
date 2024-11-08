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

df<- df%>% dplyr::select(-tbl_station)



## Pivot wider to make parameters into columns



d2<- df %>%
  dplyr::mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = "tbl_parameter", values_from = "tbl_result") %>%
  dplyr::select(-row)



d2 <- d2%>%
  dplyr::rename(
    Date = "tbl_date_ymd",
    Chla_ug.L = "Chlorophyll a",
    Odour_TON = "Odour",
    Turb_NTU = "Turbidity",
    NO3_mg.L = "Nitrate",
    NH3_mg.L = "Ammonia N",
    SRP_ug.L = "Phosphate (ortho)",
    TP_ug.L = "Phosphate (total)",
    Temp_C = "Temperature",
    Cond_uS.cm = "Conductivity", 
    SO4_mg.L = "Sulphate", 
    Org_N_mg.L = "Organic N",
    )

# select parameters of interest

df <- d2%>%
  select(Date, Chla_ug.L, Odour_TON, Turb_NTU, NO3_mg.L, NH3_mg.L, SRP_ug.L, TP_ug.L, Temp_C, Cond_uS.cm, SO4_mg.L, Org_N_mg.L, pH, DOC, TDS)


chla  <- df%>%
  select(Date, Chla_ug.L)%>%
  na.omit()

TnO <- df %>%
  select(Date, Odour_TON)%>%
  na.omit()

turb <- df %>%
  select(Date, Turb_NTU)%>%
  na.omit()

nitrate <- df %>%
  select(Date, NO3_mg.L)%>%
  na.omit()

SRP <- df %>%
  select(Date, SRP_ug.L)%>%
  na.omit()

TP <- df %>%
  select(Date, TP_ug.L)%>%
  na.omit()

Temp <- df %>%
  select(Date, Temp_C)%>%
  na.omit()

cond <- df %>%
  select(Date, Cond_uS.cm)%>%
  na.omit()

amm <- df%>%
  select(Date, NH3_mg.L)%>%
  na.omit()

sulph<- df%>%
  select(Date, SO4_mg.L)%>%
  na.omit()

orgN<- df%>%
  select(Date, Org_N_mg.L)%>%
  na.omit()

pH<- df%>%
  select(Date, pH)%>%
  na.omit()

doc<- df%>%
  select(Date, DOC)%>%
  na.omit()

#tds<- df%>%
#  select(Date, TDS)%>%
#  na.omit()


# Form new dataframe


full_df<-merge(amm, chla, by="Date", all=TRUE)

full_df<- merge(full_df, cond, by="Date", all=TRUE)
full_df<- merge(full_df, nitrate, by="Date", all=TRUE)
full_df<- merge(full_df, SRP, by="Date", all=TRUE)
full_df<- merge(full_df, sulph, by="Date", all=TRUE)
full_df<- merge(full_df, Temp, by="Date", all=TRUE)
full_df<- merge(full_df, TnO, by="Date", all=TRUE)
full_df<- merge(full_df, TP, by="Date", all=TRUE)
full_df<- merge(full_df, turb, by="Date", all=TRUE)
full_df<- merge(full_df, orgN, by="Date", all=TRUE)
full_df<- merge(full_df, pH, by="Date", all=TRUE)
full_df<- merge(full_df, doc, by="Date", all=TRUE)
#full_df<- merge(full_df, tds, by="Date", all=TRUE)

## load in missing 2001 data

miss <- read_csv("Data/Raw data/MASTERFILEWTPStandardMeasurementsWORKINGApr26_2018.csv")

miss<- miss %>%
  dplyr::filter(year == '2001')%>%
  dplyr::select(SampleDateTime, temp, ammn, chla, no3, orgn, orthop, tp, odour, turb, sulfate, cond, ph, rawdoc)%>%
  dplyr::rename(Date = "SampleDateTime",
         Chla_ug.L = "chla",
         Odour_TON = "odour",
         Turb_NTU = "turb",
         NO3_mg.L = "no3",
         NH3_mg.L = "ammn",
         SRP_ug.L = "orthop",
         TP_ug.L = "tp",
         Temp_C = "temp",
         Cond_uS.cm = "cond", 
         SO4_mg.L = "sulfate", 
         Org_N_mg.L = "orgn",
         pH = "ph",
         DOC = "rawdoc")

#miss$TDS<-NA
miss$Date <- as.Date(miss$Date, "%m/%d/%Y")

full_df<- rbind(full_df, miss)
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

#full_df <- mutate(full_df,
#             year = as.numeric(format(Date,'%Y')),
#             DOY = as.numeric(format(Date,'%j')),
#             nMonth = as.numeric(format(Date,'%m')))


#my_summary_data <- full_df %>%
 # group_by(year) %>%
 # summarise(Count = n())   

