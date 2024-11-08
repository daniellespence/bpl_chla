###############################################################################
####### SOI and PDO Data  ##########
####### Danielle Spence ##########
####### Created 6/5/2024 ########
###############################################################################

rm(list = ls())

library(pacman)
p_load(tidyverse,  readr,  dplyr,  rsoi, plyr, install = TRUE)


setwd("C:/Users/danis/OneDrive/R/BPWTP Chla GAMs")

## Load in PDO, downloaded from: https://www.ncei.noaa.gov/access/monitoring/pdo/

pdo <- read.csv("data/ersst.v5.pdo.dat.txt", sep="")

pdo<- pdo%>%
  pivot_longer(c(Jan:Dec), names_to = "month", values_to = "PDO")

pdo$month<- mapvalues(pdo$month,from = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                      to = c(1,2,3,4,5,6,7,8,9,10,11,12))

pdo$Date = as.Date(paste0(pdo$Year, "-", pdo$month, "-01"))

# calculate 3-month average for PDO

pdo$PDO_3MON_AVG <- stats::filter(pdo$PDO, rep(1 / 3, 3), sides = 2)

#pdo$date_lag<- pdo$Date %m-% months(6)


pdo1<- pdo%>%
  select(Date, PDO, PDO_3MON_AVG)


pdo2<- do.call("rbind", lapply(1:nrow(pdo), function(i) 
  data.frame(date = seq(pdo1$Date[i], 
                        (seq(pdo1$Date[i],length=2,by="months") - 1)[2], by = "1 days"), 
             PDO_3MON_AVG = pdo1$PDO_3MON_AVG[i],
             pdo = pdo1$PDO[i])))


##----- add in SOI (source is also NOAA), lag 6 months

soi<- download_soi()

#soi$date_lag<- soi$Date %m-% months(6)

soi<- soi%>%
  select(Date, Year, SOI, SOI_3MON_AVG)

soi1<- do.call("rbind", lapply(1:nrow(soi), function(i) 
  data.frame(date = seq(soi$Date[i], 
                        (seq(soi$Date[i],length=2,by="months") - 1)[2], by = "1 days"), 
             SOI_3MON_AVG = soi$SOI_3MON_AVG[i],
             soi = soi$SOI[i])))


clim<- merge( pdo2,soi1, by="date" )

write.csv(clim, file="data/SOI_PDO data no lag.csv", row.names=F)


## comparing to climate trends....

clim1 <- mutate(clim,
             year = as.numeric(format(date,'%Y')))%>% 
  filter(year %in% c(1984:2022))

annP <-aggregate(clim1$PDO_3MON_AVG,  
               by=list(clim1$year),  
                FUN=mean) 
annP<- annP %>% dplyr::rename(PDO = x)

annS <-aggregate(clim1$SOI_3MON_AVG,  
                 by=list(clim1$year),  
                 FUN=mean) 
annS<- annS %>% dplyr::rename(SOI = x)

clim2<- merge(annP, annS, by="Group.1")
clim2<- clim2%>%
  dplyr::rename(Year = 'Group.1')


clim2<- clim2%>% 
  mutate(PDO_desc = ifelse(clim2$PDO < 0, "Wet", "Dry"),
         SOI_desc = ifelse(clim2$SOI > 0, "Cool_Wet", "Warm_Dry"))

clim2<- clim2%>% 
  mutate(Desc = case_when(
    PDO < 0 & SOI > 0 ~ "Cool_wet",  # Condition 1
    PDO > 0 & SOI < 0 ~ "Warm_dry",  # Condition 2
    TRUE ~ "Other"                         # Default case
  ))

write.csv(clim2, file="data/annual SOI PDO no lag.csv", row.names=F)
