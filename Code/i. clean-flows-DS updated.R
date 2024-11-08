##############################################################################
####### Processing flow data BP ##########
####### Created by Anthony Baron ##########
####### Modified by Danielle Spence 11/1/2023 ##########
###############################################################################
### Clear memory

rm(list = ls())
library(pacman)
p_load(dplyr, tidyr, readr, lubridate,
       install = TRUE)


# Station flows for Diefenbaker, Ridge Creek, Iskwao Creek, Qu'Appelle River
# above Buffalo Pound, and the Ungauged portion of the catchment infilled with 
# modeled data (Nazemi's work) 

station_flow_raw <- read_csv("Data/Raw data/BP flow infilled with proportions (1972-2022).csv")

station_flow_raw$date <- as.Date(station_flow_raw$date, "%m/%d/%Y")
  
station_flow_raw <-  mutate(station_flow_raw,
       year = as.numeric(format(date,'%Y')),
       DOY = as.numeric(format(date,'%j')))%>% 
  filter(year %in% c(1984:2022))

  
  daily <- station_flow_raw %>% 
    filter(year %in% c(1984:2022)) %>%
    mutate(RC_IC_cms = SK05JG013.cms + combined_05JG014.cms) %>% 
    #mutate(Season = as.factor(Season)) %>% 
    mutate_if(is.character, as.numeric) %>%  
    select(year,  DOY, date, everything()) %>% 
    # Add additional proportional flows...
    mutate(RC_IC_percent = (combined_05JG014.cms + SK05JG013.cms) / combined_05JG004.cms * 100,
           RC_IC_U_percent = (combined_05JG014.cms + SK05JG013.cms + predicted_Ungauged.cms) / combined_05JG004.cms * 100)
  

write.csv(daily, file="Data/daily flow.csv", row.names=F)

