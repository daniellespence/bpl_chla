 ###############################################################################
####### BPWTP Data -- GAMs ##########
####### Danielle Spence ##########
####### Created 5/4/2023 ########
###############################################################################
### Clear memory
rm(list = ls())

library(pacman)
p_load(tidyverse, ggplot2, mgcv, gratia, readr, GGally, dplyr,  mgcViz, car, lubridate,cowplot,plyr, rsoi, rpdo, tibble, viridis, install = TRUE)

setwd("C:/Users/danis/OneDrive/R/BPWTP Chla GAMs")

##----------------------------------------------------------------------------##
## 1. Read in data 
##----------------------------------------------------------------------------##

df <- read_csv("data/bpgamdataCLEAN_flow.csv")

problems(df)

df$Date<- as.Date(df$Date, "%m/%d/%Y")

# add in Year and nMonth for numeric month and a proper Date class

df <- mutate(df,
             year = as.numeric(format(Date,'%Y')),
             DOY = as.numeric(format(Date,'%j')),
             nMonth = as.numeric(format(Date,'%m')),
             week = as.numeric(format(Date, '%W')))%>% 
  filter(year %in% c(1984:2022))


## order the data
df <- df[with(df, order(Date)),]


# rename for ease of use
df <- df%>%
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
    QWS = "RC_IC_cms"
    #orgN = "Org_N_mg.L",
    # DOC = "DOC_mg.L"
    )


#Remove rows without Chla data
df <- df[complete.cases(df$chla),]
# 1238 obs

df$DIN <- rowSums(df[,c("NO3", "NH3")], na.rm=TRUE)

## add in PDO, downloaded from: https://www.ncei.noaa.gov/access/monitoring/pdo/

pdo <- read.csv("data/ersst.v5.pdo.dat.txt", sep="")

pdo<- pdo%>%
  pivot_longer(c(Jan:Dec), names_to = "month", values_to = "PDO")


pdo$month<- mapvalues(pdo$month,from = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                      to = c(1,2,3,4,5,6,7,8,9,10,11,12))

pdo$Date = as.Date(paste0(pdo$Year, "-", pdo$month, "-01"))

# calculate 3-month average for PDO

pdo$PDO_3MON_AVG <- stats::filter(pdo$PDO, rep(1 / 3, 3), sides = 2)

pdo$date_lag<- pdo$Date %m-% months(6)


pdo<- pdo%>%
  select(date_lag, PDO, PDO_3MON_AVG)


pdo1<- do.call("rbind", lapply(1:nrow(pdo), function(i) 
  data.frame(date = seq(pdo$date_lag[i], 
                        (seq(pdo$date_lag[i],length=2,by="months") - 1)[2], by = "1 days"), 
             PDO_3MON_AVG = pdo$PDO_3MON_AVG[i],
             pdo = pdo$PDO[i])))

## add in SOI, lag 6 months

soi<- download_soi()

soi$date_lag<- soi$Date %m-% months(6)

soi<- soi%>%
  select(date_lag, SOI, SOI_3MON_AVG)

soi1<- do.call("rbind", lapply(1:nrow(soi), function(i) 
  data.frame(date = seq(soi$date_lag[i], 
                        (seq(soi$date_lag[i],length=2,by="months") - 1)[2], by = "1 days"), 
             SOI_3MON_AVG = soi$SOI_3MON_AVG[i],
             soi = soi$SOI[i])))


clim<- merge( pdo1,soi1, by="date" )

df1<- merge(df, clim, by="date")


## remove 0s/missing values from analysis....

df1 <- filter(df1, chla != 0) # 1206 obs
df1 <- filter(df1, SRP != 0) # 889
df1 <- filter(df1, DIN != 0) # 774

##----------------------------------------------------------------------------##
## 3. Check trends, distributions, correlations
##----------------------------------------------------------------------------##

# basic plot

df %>%
  ggplot(aes(x = year, y = chla)) +
  geom_line()+
  ggtitle("chla")

df %>%
  dplyr::select(chla, SRP,  W_temp,  DIN, QLD) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


## transform positively skewed data (chla, SRP, DIN)

df1$chla_sqrt<- sqrt(df1$chla)
df1$srp_sqrt<- sqrt(df1$SRP)
df1$din_sqrt<- sqrt(df1$DIN)
df1$QLD_sqrt<- sqrt(df1$QLD)

df1 %>%
  dplyr::select(chla_sqrt, srp_sqrt,  W_temp,  din_sqrt, QLD_sqrt, SOI_3MON_AVG, PDO_3MON_AVG) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()



dfx<-df1%>%
  select(chla, SRP, DIN, QLD, W_temp, PDO_3MON_AVG, SOI_3MON_AVG)
## visualize correlations
p<- ggpairs(dfx[])  
p 

scatterplotMatrix(dfx,pch=19,cex=.5,reg.line=F, 
                  spread=F,ellipse=F, col=c('gray60','#2957FF','#FF8000'),
                  col.axis='gray50')

scatterplotMatrix(scale(dfx),pch=19,cex=.5,reg.line=F, 
                  spread=F,ellipse=F, col=c('gray60','#2957FF','#FF8000'),
                  col.axis='gray50')

# water temp is correlated with nutrients and QLD.
# DIn and SRP are correlated


##----------------------------------------------------------------------------##
## 5. Check which fit is best for time
##----------------------------------------------------------------------------##


df1$fYear<- as.factor(df1$year)

# year not added

m1 <- gam(sqrt(chla) ~ s(sqrt(SRP)) +  
            s(sqrt(DIN))+
            #s(W_temp)+
            s(QLD)+
            te(PDO_3MON_AVG,SOI_3MON_AVG),
          select = TRUE,
          data = df1, method = "ML", family = tw())

gam.check(m1)


# year as tensor
m2 <- gam(sqrt(chla) ~ s(sqrt(SRP)) +  
            s(sqrt(DIN))+ 
            # s(W_temp)+
            s(QLD)+
            te(PDO_3MON_AVG,SOI_3MON_AVG)+
            te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
          select = TRUE,
          na.action = na.exclude,
          data = df1, method = "REML", family = tw())

gam.check(m2)
summary(m2) #48% explained, r sq 0.513
draw(m2, residuals = TRUE)

# year as random effect


m3 <- gam(sqrt(chla) ~ s(sqrt(SRP)) +  
            s(sqrt(DIN))+
            s(W_temp)+
            s(QLD)+
            te(PDO_3MON_AVG,SOI_3MON_AVG)+
            s(fYear, bs="re"),
          select = TRUE,
          data = df1, method = "ML", family = tw())

gam.check(m3)
summary(m3)

draw(m3, residuals = TRUE)

AIC(m1, m2, m3)
BIC(m1, m2, m3)
# model 2 is a better fit in terms of AIC and BIC.

##----------------------------------------------------------------------------##
## 5. Check for best fit family
##----------------------------------------------------------------------------##

#df1$fYear<- as.factor(df1$year)


ma <- gam(chla ~ s(SRP) +  
            s(DIN)+
            s(W_temp)+
            s(QLD)+
            te(PDO_3MON_AVG,SOI_3MON_AVG)+
            te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
            select = TRUE,
          data = df1, method = "ML", family = scat())

mb <- gam(sqrt(chla) ~ s(sqrt(SRP)) +  
            s(sqrt(DIN))+
            s(W_temp)+
            s(QLD)+
            te(PDO_3MON_AVG,SOI_3MON_AVG)+
            te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
            select = TRUE,
          data = df1, method = "ML", family = gaussian)

mc <- gam(chla ~ s(SRP) +  
            s(DIN)+
            s(W_temp)+
            s(QLD)+
            #te(PDO_3MON_AVG,SOI_3MON_AVG)+
            te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
            select = TRUE,
          data = df1, method = "ML", family = tw())


md <- gam(sqrt(chla) ~ s(sqrt(SRP)) +  
            s(sqrt(DIN))+
            s(W_temp)+
            s(QLD)+
            te(PDO_3MON_AVG,SOI_3MON_AVG)+
            te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
          knots=list(DOY=c(0, 366.5)),
          select = TRUE,
          data = df1, method = "ML", family = Gamma(link = "log"))

me <- gam(sqrt(chla) ~ s(sqrt(SRP)) +  
            s(sqrt(DIN))+
            s(W_temp)+
            s(QLD)+
            te(PDO_3MON_AVG,SOI_3MON_AVG)+
            te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
          select = TRUE,
          data = df1, method = "ML",  family = inverse.gaussian(link = "log"))

# Compare AIC values
AIC(mc,ma)
summary(mc)
draw(mc)

##--- check which variables should stay/be removed

nullvars <- c('chla', "SRP", "DIN", "QLD", "W_temp", "PDO_3MON_AVG", "SOI_3MON_AVG", "year", "DOY")
mumdat <- df1[complete.cases(df1[,nullvars]),] # want to AIC compare, and NA diff
#   between vars
mummod <- update(mc, data=mumdat, na.action="na.fail")
mummoddin <- update(mummod, .~.-s(sqrt(DIN)))
mummodsrp <- update(mummod, .~.-s(sqrt(SRP)))
mummodwtemp <- update(mummod, .~.-s(W_temp))
mummodQLD <- update(mummod, .~.-s(QLD))
mummodTE <- update(mummod, .~.-te(PDO_3MON_AVG,SOI_3MON_AVG))

AIC(mummod, mummodwtemp,  mummodTE) # 

summary(mummodwtemp)

# AIC is somewhat better when water temp is removed, however not by much. reduciton in model fit when other predictors removed.


##----------------------------------------------------------------------------##
## 5. check whether SOI and PDO should be included as individual or tensor
##----------------------------------------------------------------------------##

# year not added

mi <- gam(sqrt(chla) ~ s(sqrt(SRP)) +  
            s(sqrt(DIN))+
            #s(W_temp)+
            s(QLD)+
            s(PDO_3MON_AVG)+
            s(SOI_3MON_AVG)+
            te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
          #s(fYear, bs="re"),
          select = TRUE,
          data = df1, method = "REML", family = tw())

summary(mi)
gam.check(mi)
draw(mi, all=TRUE)


# year as tensor
mI <- gam(sqrt(chla) ~ s(sqrt(SRP)) +  
            s(sqrt(DIN))+ 
            # s(W_temp)+
            s(QLD)+
            ti(PDO_3MON_AVG) +
            ti(SOI_3MON_AVG)+
            ti(PDO_3MON_AVG,SOI_3MON_AVG)+
            te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
          select = TRUE,
          data = df1, method = "REML", family = tw())

anova(mi, mI, test = "LRT") #tensor interaction is better.
draw(mI)


##----------------------------------------------------------------------------##
## 5. check whether sqrt transform improves model fit
##----------------------------------------------------------------------------##

m_t <- gam(chla ~ s(SRP, k=10) +  
             s(DIN, k=10)+
             s(QLD, k=5)+ 
             # s(W_temp)+
             te(PDO_3MON_AVG,SOI_3MON_AVG)+
             te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
           knots=list(DOY=c(0, 366.5)),
           select = TRUE,
           data = df1, method = "REML", family = tw(link="log"))

m_tSQ <- gam(sqrt(chla) ~ s(sqrt(SRP), k=10) +  
             s(sqrt(DIN), k=10)+
             s(QLD, k=5)+ 
             # s(W_temp)+
             te(PDO_3MON_AVG,SOI_3MON_AVG)+
             te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
           knots=list(DOY=c(0, 366.5)),
           select = TRUE,
           data = df1, method = "REML", family = tw(link="log"))

m_sq <- gam(chla ~ s(sqrt(SRP), k=10) +  
             s(sqrt(DIN), k=10)+
             s(QLD, k=5)+ 
             # s(W_temp)+
             te(PDO_3MON_AVG,SOI_3MON_AVG)+
             te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
           knots=list(DOY=c(0, 366.5)),
           select = TRUE,
           data = df1, method = "REML", family = tw(link="log"))

AIC(m_t, m_SQ, m_sq)


## i. Appraise model fits
r1<- appraise(m_sq, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  theme(plot.tag = element_text(face = 'bold')) & theme_bw()
r1

r1<- appraise(m_SQ, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  theme(plot.tag = element_text(face = 'bold')) & theme_bw()
r1


##----------------------------------------------------------------------------##
## 6. Control for autocorrelation
##----------------------------------------------------------------------------##


ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

m<- gamm(chla ~ s(SRP, k=5) +  
           s(DIN, k=6)+
           s(QLD, k=5)+ 
           te(PDO_3MON_AVG,SOI_3MON_AVG)+
           te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
         # select = TRUE,
         data = df1, method = "REML", 
         family = Gamma(link = "log"))

## AR(1)
m1 <- gamm(chla ~ s(SRP, k=10) +  
             s(DIN, k=10)+
             s(QLD, k=5)+ 
             te(PDO_3MON_AVG,SOI_3MON_AVG)+
             te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
           # select = TRUE,
           data = df1, family = Gamma(link = "log"),
           correlation = corARMA(form = ~ 1|year, p = 1),
           control = ctrl, method = "REML")
## AR(2)
m2 <- gamm(chla ~ s(SRP, k=5) +  
             s(DIN, k=6)+
             s(QLD, k=5)+ 
             te(PDO_3MON_AVG,SOI_3MON_AVG)+
             te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
           #select = TRUE,
           family = Gamma(link = "log"),
           data = df1, correlation = corARMA(form = ~ 1|year, p = 2),
           control = ctrl, method = "REML")

## AR(3)
m3 <- gamm(chla ~ s(SRP, k=5) +  
             s(DIN, k=6)+
             s(QLD, k=5)+ 
             te(PDO_3MON_AVG,SOI_3MON_AVG)+
             te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
           
           data = df1, family = Gamma(link = "log"),
           correlation = corARMA(form = ~ 1|year, p = 3),
           control = ctrl, method = "REML")

anova(m$lme, m1$lme, m2$lme, m3$lme) ## shows that m1 is best


layout(matrix(1:2, ncol = 2))
res <- resid(m1$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(1) errors")
acf(res, lag.max = 36, main = "pACF- AR(1) errors")
layout(1) # this is better

summary(m1$gam) 

k.check(m1$gam) 

## i. Appraise model fit
r1<- appraise(m1$gam, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  theme(plot.tag = element_text(face = 'bold')) & theme_bw()
r1

ggsave('output/Chla GAM_preds RESIDUALS AUTOCORR GAMMA.png', r1, height = 6, width  = 8)

p1<- draw(m1$gam, residuals = TRUE)& theme_bw()
p1

#ggsave('output/Chla GAM_ autocorrelation no interaction SOI.png', p1, height = 10, width  = 10)


##--------------------------------------------------##
## References
# Simpson, 2014: https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# Hayes et al, 2020: https://doi.org/10.1002/lol2.10164
# Simpson 2020 : https://stats.stackexchange.com/questions/471267/plotting-gams-on-response-scale-with-multiple-smooth-and-linear-terms
# Wilk et al., 2018: https://doi.org/10.1029/2018JG004506 (plotting response values)
