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


full_df <- full_df[rowSums(is.na(full_df)) <= 7, ]       # Apply rowSums & is.na

full_df <- mutate(full_df,
             DOY = as.numeric(format(Date,'%j')),
             nMonth = as.numeric(format(Date,'%m')))


# 2001 nitrate was 0.00 from sept-dec

# Replace NA values with 0 for the year 2020 
full_df$NO3_mg.L[full_df$year == 2001 & is.na(full_df$NO3_mg.L)] <- 0

# add anthony imputations from 2003-2004 for SRP and ammonia

#impute from nearest neighbours

# ammonia
full_df %>% filter(year == 2002) # c(0.0767, 0.0775) for month 3
full_df %>% filter(year == 2003) # c(0.0667, 0) for month 1,2
full_df %>% filter(year == 2003) # c(0.1, 0.02) for month 11,12
full_df %>% filter(year == 2004) # c(0.1, 0.02) for month 1,2,3

full_df <- full_df %>% 
  mutate(NH3_mg.L = case_when(
    is.na(NH3_mg.L) & year == c(2002) & nMonth %in% c(3)~ mean(c(0.0767, 0.0775)),
    is.na(NH3_mg.L) & year == c(2003) & nMonth %in% c(1, 2) ~ mean(c(0.0667, 0)),
    is.na(NH3_mg.L) & year == c(2003) & nMonth %in% c(11, 12) ~ mean(c(0.1, 0.02)),
    is.na(NH3_mg.L) & year == c(2004) & nMonth %in% c(1, 2, 3)~ mean(c(0.1, 0.02)),
    TRUE ~ as.numeric(NH3_mg.L)
  ))


## SRP

full_df %>% filter(year %in% 2004) # c(10, 5.82)
full_df %>% filter(year %in% 2015) # c(55, 0)
full_df %>% filter(year %in% 2018) # c(13, 3)


full_df <- full_df %>% 
  mutate(SRP_ug.L = case_when(
    is.na(SRP_ug.L) & year %in% c(2003:2004) ~ mean(c(10, 5.82)),
    TRUE ~ as.numeric(SRP_ug.L)
  ))

## include anthony imputations for chl.a

# Fill in missing Chl a in longterm df via:

# 2002-03-01 impute w nearest neighbour == 7.79
# 2003-01-01 impute w nearest neighbour == 20.0
# 2003-02-01 impute w nearest neighbour == 20.0
# 2007-12-01 impute w nearest neighbour == 24.9
# 2017-12-01 impute w nearest neighbour == 24.5


full_df$Chla_ug.L <- ifelse(is.na(full_df$Chla_ug.L) & full_df$Date == "2001-12-01", 12.3, full_df$Chla_ug.L)
full_df$Chla_ug.L <- ifelse(is.na(full_df$Chla_ug.L) & full_df$Date == "2002-03-18", 7.79, full_df$Chla_ug.L)
full_df$Chla_ug.L <- ifelse(is.na(full_df$Chla_ug.L) & full_df$Date == "2003-01-06", 20.0, full_df$Chla_ug.L)
full_df$Chla_ug.L <- ifelse(is.na(full_df$Chla_ug.L) & full_df$Date == "2003-02-10", 20.0, full_df$Chla_ug.L)


## change 0s for N and P to limits of detection

full_df$NO3_mg.L[full_df$NO3_mg.L == 0] <- (0.057/2)
full_df$NH3_mg.L[full_df$NH3_mg.L == 0] <- (0.086/2)
full_df$SRP_ug.L[full_df$SRP_ug.L == 0] <- (3/2)

write.csv(full_df, file="data/bpgamdataCLEAN_flow.csv", row.names=F)

my_summary_data <- full_df %>%
  group_by(year) %>%
  summarise(Count = n())   

