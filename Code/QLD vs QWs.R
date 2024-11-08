###############################################################################
####### BPWTP Data -- GAMs ##########
####### Danielle Spence ##########
####### Created 8/26/2024 ########
###############################################################################
### Clear memory
rm(list = ls())

library(pacman)
p_load(tidyverse, ggplot2, readr, GGally, dplyr, install = TRUE)

library(dplyr)

setwd("C:/Users/danis/OneDrive/R/DW indicators")


##----------------------------------------------------------------------------##
## 1. Read in data 
##----------------------------------------------------------------------------##

df <- read_csv("data/bpgamdataCLEAN_flow.csv")

df$Date<- as.Date(df$Date, "%m/%d/%Y")

# add in Year and nMonth for numeric month and a proper Date class

df <- mutate(df,
             year = as.numeric(format(Date,'%Y')),
             DOY = as.numeric(format(Date,'%j')),
             nMonth = as.numeric(format(Date,'%m')))%>% 
  filter(year %in% c(1984:2022))


## order the data
df <- df[with(df, order(Date)),]


# rename for ease of use
df <- df %>%
  dplyr::rename(
    date = "Date",
    chla = "Chla_ug.L",
    TON = "Odour_TON",
    turb = "Turb_NTU",
    NO3 = "NO3_mg.L",
    NH3 = "NH3_mg.L",
    SRP = "SRP_ug.L",
    TP = "TP_ug.L",
    W_temp = "Temp_C",
    cond = "Cond_uS.cm",
    SO4 = "SO4_mg.L",
    #A_temp = "mean_temp",
    QBP = "combined_05JG004.cms",
    QLD = "SK05JG006.cms",
    QWS = "RC_IC_cms",
    orgN = "Org_N_mg.L",
    # DOC = "DOC_mg.L"
  )



#Remove rows without TON data
df <- df[complete.cases(df$QWS),]
df <- df[complete.cases(df$QLD),]

# Fit a linear regression model
m <- lm(QLD ~ QWS, data = df)

# Display the summary of the model
summary(m)

# Add the regression line to a scatter plot
p<-ggplot(df, aes(x=QWS, y=QLD, colour=year))+
  geom_point(size=2)+
  scale_colour_viridis_c(option = "mako", direction = 1, name = "Year") +
  xlab(expression(paste("Flow from watershed sources (",m^3, "/", s,")")))+ ylab(expression(paste("Flow from Lake Diefenbaker ( ",m^3, "/", s,")")))+
  theme_bw(base_size = 14)
p
ggsave('output/scatterplot QWS QLD.png', p, height = 6, width  = 8)


## 
dfx<-df%>%
  select(QWS, QLD)
## visualize correlations
p<- ggpairs(dfx[]) + theme_bw()  # can see that DOC and turbidity, temperature, Org_N and TP are correlated. Ammonia and conductivity are not. (NH3, NO3, and TP correlated; TP and temp). Org N and TP are similarly correlated with DOC (0.640 and 0.667, respectively)
p 


ggsave('output/QWS QLD Correlations.png', p, height = 8, width  = 10)

summary(df$QWS)

# Convert yQWS# Convert y to a factor to indicate zero vs non-zero
df$QWS_group <- factor(ifelse(df$QWS >= 0.2069, "Above/Equal", "Below"))


p <-ggplot(df, aes(x = QLD, fill=QWS_group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Above/Equal" = "#165460A1", "Below" = "#165430A1"), name="Average QWS")+
  ylab(expression(paste("Conditional density")))+ xlab(expression(paste("Flow from Lake Diefenbaker ( ",m^3, "/", s,")")))+
  theme_bw(base_size = 14)
p

ggsave('output/conditional density QWS QLD.png', p, height = 8, width  = 10)
