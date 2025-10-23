 ###############################################################################
####### BPWTP Data -- GAMs ##########
####### Danielle Spence ##########
####### Created 5/4/2023 ########
###############################################################################
### Clear memory
rm(list = ls())

library(pacman)
p_load(tidyverse, ggplot2, mgcv, gratia, readr, GGally, dplyr,  mgcViz,  lubridate, 
        cowplot, tibble, viridis, marginaleffects,  install = TRUE)

setwd("C:/Users/danis/OneDrive/R/BPWTP Chla GAMs") 

##----------------------------------------------------------------------------##
## 1. Read in data 
##----------------------------------------------------------------------------##

df <- read_csv("data/bpgamdataCLEAN_flow_noimput.csv")
# 1345 obs

df$Date<- as.Date(df$Date, "%m/%d/%Y")

## order the data
df <- df[with(df, order(Date)),]


# rename for ease of use
df <- df%>%
  dplyr::rename(
    date = "Date",
    chla = "Chla_ug.L",
    NO3 = "NO3_mg.L",
    NH3 = "NH3_mg.L",
    SRP = "SRP_ug.L",
    W_temp = "Temp_C",
    QLD = "SK05JG006.cms",
    QWS = "RC_IC_cms"
    )


#Remove rows without Chla data
df <- df[complete.cases(df$chla),]
# 1236 obs


df$NO3[df$NO3 == 0] <- (0.057/2)
df$NH3[df$NH3 == 0] <- (0.086/2)
df$SRP[df$SRP == 0] <- (3/2)

df$DIN <- rowSums(df[,c("NO3", "NH3")], na.rm=TRUE)

## add in PDO and SOI 

clim <- read_csv("data/SOI_PDO data no lag.csv")

df1<- merge(df, clim, by="date")


##----------------------------------------------------------------------------##
## 3. Check trends, distributions, correlations
##----------------------------------------------------------------------------##

# #check distributions
# 
# df %>%
#   dplyr::select(chla, SRP,  DIN) %>%
#   gather() %>%
#   ggplot(aes(value)) +
#   facet_wrap(~ key, scales = "free") +
#   geom_histogram()
# 
# # check correlations
# dfx<-df1%>%
#   select(chla, SRP, DIN, QLD, W_temp, PDO_3MON_AVG, SOI_3MON_AVG)
# ## visualize correlations
# p<- ggpairs(dfx[])  # can see that chla and turbidity, temperature, Org_N and TP are correlated. Ammonia and conductivity are not. (NH3, NO3, and TP correlated; TP and temp). Org N and TP are similarly correlated with chla (0.640 and 0.667, respectively)
# p 
# 
# 
# ggsave('output/Correlations.png', p, height = 6, width  = 8)


df_clean <- df1 %>%
  dplyr::ungroup() %>%                    # Remove any previous grouping
  select(year, chla)



temporal_ranges = df_clean %>%
  group_by(year) %>%
  summarise(mean_year = mean(chla, na.rm = TRUE),
            seasonal_range = max(chla, na.rm = TRUE) - min(chla, na.rm = TRUE)) %>%
  summarise(
    between_year_range = max(mean_year, na.rm = TRUE) - min(mean_year, na.rm = TRUE),
    extreme_range = max(mean_year, na.rm = TRUE)/ min(mean_year, na.rm = TRUE),
    avg_year = mean(mean_year, na.rm = TRUE),
    extreme_factor = (max(mean_year, na.rm = TRUE)/ avg_year),
    avg_within_year_range = mean(seasonal_range, na.rm = TRUE),
    max_within_year_range = max(seasonal_range, na.rm = TRUE)
  )

#between_year_range_percent = (between_year_range / mean_all_years) * 100

library(dplyr)


##----------------------------------------------------------------------------##
## 5. Run the full model (check model fitting script)
##----------------------------------------------------------------------------##

m <- gam(chla ~ s(SRP, k=10) +  
              s(DIN, k=10)+
              s(QLD, k=7)+
              te(PDO_3MON_AVG,SOI_3MON_AVG)+
              te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
            knots=list(DOY=c(0, 366.5)),
            select = TRUE,
            data = df1, method = "REML", family = tw(link="log"))

#saveRDS(m, file = "modelchla_SOInolag.rds")
# sink("model_summarychla.txt")
# summary(m)
# sink()  

# plot_predictions(m, condition = c('SRP', 'DIN'),
#                  type = 'response', points = 0.5,
#                  rug = TRUE) 

summary(m) # Deviance explained = 60%, REML = 4063, r2 = 0.501, n=1084

k.check(m)# k-index looks good 

p1<- draw(m,  residuals = TRUE)& theme_bw() 
p1

#ggsave('output/Chla GAM basic TW.png', p1, height = 10, width  = 10)



##----------------------------------------------------------------------------##
## 6. Assess model fit & autocorrelation
##----------------------------------------------------------------------------##

## i. Appraise model fit
r<- appraise(m, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  theme(plot.tag = element_text(face = 'bold')) & theme_bw()
r

ggsave('output/Chla GAM RESIDUALS TW_noimputs.png', r, height = 6, width  = 8)

# ii. Check for autocorrelation

layout(matrix(1:2, ncol = 2))
acf(resid(m), lag.max = 36, main = "ACF") 
pacf(resid(m), lag.max = 36, main = "pACF")
layout(1)





##--------------------------------------------------##
## References
# Simpson, 2014: https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# Hayes et al, 2020: https://doi.org/10.1002/lol2.10164
