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

df <- read_csv("data/bpgamdataCLEAN_flow.csv")


df$Date<- as.Date(df$Date, "%m/%d/%Y")

# add in Year and nMonth for numeric month and a proper Date class

df <- mutate(df,
             year = as.numeric(format(Date,'%Y')),
             nMonth = as.numeric(format(Date,'%m')),
             DOY = as.numeric(format(Date,'%j')))%>% 
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
    QBP = "combined_05JG004.cms",
    QLD = "SK05JG006.cms",
    QWS = "RC_IC_cms"
    )


#Remove rows without Chla data
df <- df[complete.cases(df$chla),]
# 1255 obs

# change 0 values to LOD 
df$NO3[df$NO3 == 0] <- 0.057
df$NH3[df$NH3 == 0] <- 0.086

df$DIN <- rowSums(df[,c("NO3", "NH3")], na.rm=TRUE)

## add in PDO and SOI 

clim <- read_csv("data/SOI_PDO data.csv")

df1<- merge(df, clim, by="date")

## remove 0s/missing values from analysis....

df1 <- filter(df1, chla != 0) # 1255 obs
#df1 <- filter(df1, SRP != 0) # 903
#df1 <- filter(df1, DIN != 0) # 774

##----------------------------------------------------------------------------##
## 3. Check trends, distributions, correlations
##----------------------------------------------------------------------------##

#check distributions

df %>%
  dplyr::select(chla, SRP,  DIN) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# check correlations
dfx<-df1%>%
  select(chla, SRP, DIN, QLD, W_temp, PDO_3MON_AVG, SOI_3MON_AVG)
## visualize correlations
p<- ggpairs(dfx[])  # can see that chla and turbidity, temperature, Org_N and TP are correlated. Ammonia and conductivity are not. (NH3, NO3, and TP correlated; TP and temp). Org N and TP are similarly correlated with chla (0.640 and 0.667, respectively)
p 


ggsave('output/Correlations.png', p, height = 6, width  = 8)


##----------------------------------------------------------------------------##
## 5. Run the full model (check model fitting script)
##----------------------------------------------------------------------------##

m <- gam(chla ~ s(SRP, k=10) +  
              s(DIN, k=10)+
              s(QLD, k=5)+ 
              # s(W_temp)+
              te(PDO_3MON_AVG,SOI_3MON_AVG)+
              te(year, DOY, bs = c("cr", "cc"), k=c(10,12)),
            knots=list(DOY=c(0, 366.5)),
            select = TRUE,
            data = df1, method = "REML", family = tw(link="log"))

#plot_predictions(m, condition = c('SRP', 'DIN'),
#                 type = 'response', points = 0.5,
#                 rug = TRUE) 

summary(m) # Deviance explained = 60%, REML = 3894, r2 = 0.499

k.check(m)# k-index looks good 

p1<- draw(m,  residuals = TRUE)& theme_bw() 
p1

ggsave('output/Chla GAM basic TW.png', p1, height = 10, width  = 10)



##----------------------------------------------------------------------------##
## 6. Assess model fit & autocorrelation
##----------------------------------------------------------------------------##

## i. Appraise model fit
r<- appraise(m, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  theme(plot.tag = element_text(face = 'bold')) & theme_bw()
r

ggsave('output/Chla GAM RESIDUALS TW .png', r, height = 6, width  = 8)

# ii. Check for autocorrelation

layout(matrix(1:2, ncol = 2))
acf(resid(m), lag.max = 36, main = "ACF") 
pacf(resid(m), lag.max = 36, main = "pACF")
layout(1)




#----------------------------------------------------------------------------##
## 7. Plotting on the response scale/fitted values
##----------------------------------------------------------------------------##


## ---------- DIN -------------------##

new_data_DIN <- with(df1, expand.grid(DIN = seq(min(DIN), max(DIN),length = 200),
                                     SRP = median(SRP, na.rm=TRUE),
                                 W_temp = median(W_temp, na.rm=TRUE),
                                 QLD = median(QLD),
                                 SOI_3MON_AVG = median(SOI_3MON_AVG, na.rm=TRUE),
                                 PDO_3MON_AVG = median(PDO_3MON_AVG, na.rm=TRUE),
                                 year = median(year),
                                 DOY = median(DOY)))

DIN.pred <- predict(m, newdata = new_data_DIN, type = "terms", se.fit = TRUE)

whichCols <- grep("DIN", colnames(DIN.pred$fit))
whichColsSE <- grep("DIN", colnames(DIN.pred$se.fit))
new_data_DIN <- cbind(new_data_DIN, Fitted = DIN.pred$fit[, whichCols], 
                      se.Fitted = DIN.pred$se.fit[, whichColsSE])
limits <- aes(ymax = Fitted + se.Fitted, ymin= Fitted - se.Fitted)

## make into original limits
new_data_DIN <- with(new_data_DIN, transform(new_data_DIN, Fittedplus = Fitted + se.Fitted))
new_data_DIN <- with(new_data_DIN, transform(new_data_DIN, Fittedminus = Fitted - se.Fitted))

shiftDIN <- attr(predict(m, newdata = new_data_DIN, type = "iterms"), "constant")
DIN.pdatnorm <- new_data_DIN
DIN.pdatnorm <- with(DIN.pdatnorm, transform(DIN.pdatnorm, Fitted = Fitted + shiftDIN, 
                                             Fittedplus = Fittedplus + shiftDIN, 
                                             Fittedminus = Fittedminus + shiftDIN))

# backtransform from tweedie distribution
DIN.pdatnorm$Fitted<-exp(DIN.pdatnorm$Fitted)
DIN.pdatnorm$Fittedplus<-exp(DIN.pdatnorm$Fittedplus)
DIN.pdatnorm$Fittedminus<-exp(DIN.pdatnorm$Fittedminus)

DINquants <- quantile(df1$DIN, c(.05,.95), na.rm = TRUE)

DINplot <- ggplot(DIN.pdatnorm, aes(x = DIN, y = Fitted)) +
  theme_bw(base_size = 14)+
  annotate("rect", xmin=DINquants[1], xmax=DINquants[2], ymin=-Inf, ymax=Inf, alpha = 0.1, fill='gray60') +
  geom_line() +
  geom_ribbon(aes(ymin = Fittedminus, ymax = Fittedplus), 
              alpha = 0.25, fill = '#165459B2') +  
  geom_rug(aes(x=DIN), data = df1, stat = "identity", position = "identity", 
           sides = "b", na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, alpha=0.3) +
  xlab(expression(paste("DIN"~"("*"mg"~L^{-1}*")"))) + ylab(expression(paste("Chlorophyll ", italic("a"), " (",mu,"g L"^-1*")")))

DINplot


## ---------- SRP -------------------##

new_data_SRP <- with(df1, expand.grid(SRP = seq(min(SRP, na.rm = TRUE), max(SRP, na.rm = TRUE), length = 200),
                                     DIN = median(DIN),
                                     W_temp = median(W_temp, na.rm=TRUE),
                                     QLD = median(QLD),
                                     SOI_3MON_AVG = median(W_temp, na.rm=TRUE), 
                                     PDO_3MON_AVG = median(PDO_3MON_AVG, na.rm=TRUE),
                                     year = median(year),
                                     DOY = median(DOY)))


SRP.pred <- predict(m, newdata = new_data_SRP, type = "terms", se.fit = TRUE)


whichCols <- grep("SRP", colnames(SRP.pred$fit))
whichColsSE <- grep("SRP", colnames(SRP.pred$se.fit))
new_data_SRP <- cbind(new_data_SRP, Fitted = SRP.pred$fit[, whichCols], 
                      se.Fitted = SRP.pred$se.fit[, whichColsSE])

limits <- aes(ymax = Fitted + se.Fitted, ymin= Fitted - se.Fitted)

## make into original limits
new_data_SRP <- with(new_data_SRP, transform(new_data_SRP, Fittedplus = Fitted + se.Fitted))
new_data_SRP <- with(new_data_SRP, transform(new_data_SRP, Fittedminus = Fitted - se.Fitted))

shiftSRP <- attr(predict(m, newdata = new_data_SRP, type = "iterms"), "constant")
SRP.pdatnorm <- new_data_SRP
SRP.pdatnorm <- with(SRP.pdatnorm, transform(SRP.pdatnorm, Fitted = Fitted + shiftSRP, 
                                             Fittedplus = Fittedplus + shiftSRP, 
                                             Fittedminus = Fittedminus + shiftSRP))

# backtransform from tweedie distribution
SRP.pdatnorm$Fitted<-exp(SRP.pdatnorm$Fitted)
SRP.pdatnorm$Fittedplus<-exp(SRP.pdatnorm$Fittedplus)
SRP.pdatnorm$Fittedminus<-exp(SRP.pdatnorm$Fittedminus)


SRPquants <- quantile(df1$SRP, c(.05,.95), na.rm = TRUE)


SRPplot <- ggplot(SRP.pdatnorm, aes(x = SRP, y = Fitted)) +
  theme_bw(base_size = 14)+
  scale_y_continuous(limits=c(0, 25))+
 annotate("rect", xmin=SRPquants[1], xmax=SRPquants[2], ymin=-Inf, ymax=Inf, alpha = 0.1, fill='gray60') +
  geom_line() +
  geom_ribbon(aes(ymin = Fittedminus, ymax = Fittedplus), 
              alpha = 0.25, fill = '#165459B2') +  
   geom_rug(aes(x=SRP), data = df1, stat = "identity", position = "identity", 
           sides = "b", na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, alpha=0.3) +
  xlab(expression(paste("SRP ", " (",mu,"g L"^-1*")"))) + ylab(expression(paste("Chlorophyll ", italic("a"), " (", mu,"g L"^-1*")")))

SRPplot


## ---------- QLD -------------------##

new_data_QLD <- with(df1, expand.grid(QLD = seq(min(QLD), max(QLD), length = 200),
                                      DIN = median(DIN),
                                      SRP = median(SRP, na.rm=TRUE),
                                      W_temp = median(W_temp, na.rm=TRUE),
                                      SOI_3MON_AVG = median(W_temp, na.rm=TRUE), 
                                      PDO_3MON_AVG = median(PDO_3MON_AVG, na.rm=TRUE),
                                      year = median(year),
                                      DOY = median(DOY)))

QLD.pred <- predict(m, newdata = new_data_QLD, type = "terms", se.fit = TRUE)

whichCols <- grep("QLD", colnames(QLD.pred$fit))
whichColsSE <- grep("QLD", colnames(QLD.pred$se.fit))
new_data_QLD <- cbind(new_data_QLD, Fitted = QLD.pred$fit[, whichCols], 
                       se.Fitted = QLD.pred$se.fit[, whichColsSE])
limits <- aes(ymax = Fitted + se.Fitted, ymin= Fitted - se.Fitted)

## make into original limits
new_data_QLD <- with(new_data_QLD, transform(new_data_QLD, Fittedplus = Fitted + se.Fitted))
new_data_QLD <- with(new_data_QLD, transform(new_data_QLD, Fittedminus = Fitted - se.Fitted))

shiftQLD <- attr(predict(m, newdata = new_data_QLD, type = "iterms"), "constant")
QLD.pdatnorm <- new_data_QLD
QLD.pdatnorm <- with(QLD.pdatnorm, transform(QLD.pdatnorm, Fitted = Fitted + shiftQLD, 
                                               Fittedplus = Fittedplus + shiftQLD, 
                                               Fittedminus = Fittedminus + shiftQLD))

# backtransform from tweedie distribution
QLD.pdatnorm$Fitted<-exp(QLD.pdatnorm$Fitted)
QLD.pdatnorm$Fittedplus<-exp(QLD.pdatnorm$Fittedplus)
QLD.pdatnorm$Fittedminus<-exp(QLD.pdatnorm$Fittedminus)


QLDquants <- quantile(df1$QLD, c(.05,.95), na.rm = TRUE)

QLDplot <- ggplot(QLD.pdatnorm, aes(x = QLD, y = Fitted)) +
  theme_bw(base_size = 14)+
  scale_y_continuous(limits=c(5, 22))+
  annotate("rect", xmin=QLDquants[1], xmax=QLDquants[2], ymin=-Inf, ymax=Inf, alpha = 0.1, fill='gray60') +
  geom_line() +
  geom_ribbon(aes(ymin = Fittedminus, ymax = Fittedplus), 
              alpha = 0.25, fill = '#165459B2') +  
 geom_rug(aes(x=QLD), data = df1, stat = "identity", position = "identity", 
           sides = "b", na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, alpha=0.3) +
  xlab(expression(paste("QLD (",m^3, "/", s,")"))) + ylab(expression(paste("Chlorophyll ", italic("a"), " (", mu, "g L"^-1*")")))

QLDplot




p_all<- plot_grid(SRPplot, DINplot, QLDplot,  ncol = 2,  labels = c('A.', 'B.', 'C.'))
p_all


ggsave('output/Chla GAM_Response scale NO lag.png', p_all, height = 10, width  = 10)


## ---------- SOI * PDO -------------------##

new_data_SOI_3MON_AVG <- with(df1, expand.grid(SOI_3MON_AVG = seq(min(SOI_3MON_AVG), max(SOI_3MON_AVG), length = 200),
                                               PDO_3MON_AVG = seq(min(PDO_3MON_AVG), max(PDO_3MON_AVG), length = 200),
                                               SRP = median(SRP, na.rm=TRUE),
                                               DIN = median(DIN, na.rm=TRUE),
                                               W_temp = median(W_temp, na.rm=TRUE),
                                               QLD = median(QLD, na.rm=TRUE),
                                               year = median(year),
                                               DOY = median(DOY)))


SOI_3MON_AVG.pred <- predict(m, newdata = new_data_SOI_3MON_AVG, type = "terms")

whichCols <- grep("PDO_3MON_AVG,SOI_3MON_AVG", colnames(SOI_3MON_AVG.pred))

new_data_SOI_3MON_AVG <- cbind(new_data_SOI_3MON_AVG, Fitted = SOI_3MON_AVG.pred[, whichCols])

shiftcomb <- attr(SOI_3MON_AVG.pred, "constant")
SOI_3MON_AVG.pdatnorm <- new_data_SOI_3MON_AVG
SOI_3MON_AVG.pdatnorm <- with(SOI_3MON_AVG.pdatnorm, transform(SOI_3MON_AVG.pdatnorm, Fitted = Fitted + shiftcomb))

toofar <- exclude.too.far(SOI_3MON_AVG.pdatnorm$PDO_3MON_AVG, SOI_3MON_AVG.pdatnorm$SOI_3MON_AVG, df1$PDO_3MON_AVG, df1$SOI_3MON_AVG, dist=0.1)

SOI_3MON_AVG.pdatnorm$chla <- SOI_3MON_AVG.pdatnorm$Fitted
SOI_3MON_AVG.pdatnorm$chla[toofar] <- NA

# backtransform from tweedie distribution
SOI_3MON_AVG.pdatnorm$chla<- exp(SOI_3MON_AVG.pdatnorm$chla)


names(new_data_SOI_3MON_AVG)[which(names(new_data_SOI_3MON_AVG)=='SOI_3MON_AVG.pred')] <- 'Chla'

comboplot <- ggplot(SOI_3MON_AVG.pdatnorm, aes(x = PDO_3MON_AVG, y = SOI_3MON_AVG, z=chla)) + 
  theme_bw(base_size=14, base_family = 'Arial') +
  theme(legend.position='top') +
  geom_raster(aes(fill=chla)) + # change to turn grey background into nothing
  scale_fill_distiller(palette = "Spectral", direction = -1, na.value='transparent') +
  geom_point(data=df1, aes(x=PDO_3MON_AVG, y=SOI_3MON_AVG, z=NULL)) +
  geom_contour(colour = "black", binwidth = 5) +
  theme(legend.key.width=unit(1.5,"cm"))+
  xlab("PDO") + ylab("SOI") +
  labs(fill=expression(paste("Chlorophyll ", italic("a"), " (", mu ,"g L"^-1*")  ")))+
  theme(legend.position = "top")


comboplot


ggsave('output/SOI PDO INTERACTION Response scale NO LAG.png', comboplot, height = 6, width  = 8)


## ==================================================================================
## Looking at effect by time SOURCE CODE (Wilk et al. 2018)
## ==================================================================================


df2<- df1%>%
  select(date, year, nMonth, DOY, chla, SRP, DIN, QLD,  SOI_3MON_AVG, PDO_3MON_AVG)
df2<- na.exclude(df2)

df2$date <- as.Date(df2$date)

testing1 <- predict(m, type = 'terms')
testing <- as.data.frame(testing1)

tosum <- grep("DIN", colnames(testing))
DINeffect <- rowSums(testing[tosum], na.rm = TRUE)
testing <- testing[,-tosum]
testing$DIN <- DINeffect


names(testing) <- c("SRP", "QLD", "SOI_PDO", "time", "DIN")
testing$Year <- df1$year
testing$Month <- df1$nMonth


testing$Month<- as.factor(testing$Month)
testing$Year<- as.factor(testing$Year)

peD <- ggplot(testing, aes(x = Year, y = DIN)) +
  annotate("rect", ymin = -0.1, ymax = 0.1,
           xmin = -Inf, xmax = Inf, alpha = 0.3, fill = '#165459B2') +
  geom_boxplot() +
  scale_x_discrete(breaks = seq(1985, 2020, by = 5))+
  theme_bw(base_size=12) +
  theme(axis.text.x = element_text(angle=70, hjust=1,  face = "bold")) +
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.x = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold")) + 
  ylab("DIN effect") + xlab("Year")
peD


##--------SRP--------------##


peS <- ggplot(testing, aes(x = Year, y = SRP)) +
  annotate("rect", ymin = -0.1, ymax = 0.1,
           xmin = -Inf, xmax = Inf, alpha = 0.3, fill = '#165459B2') +
  geom_boxplot() +
  scale_x_discrete(breaks = seq(1985, 2020, by = 5))+
  theme_bw(base_size=14) +
  theme_bw(base_size=12) +
  theme(axis.text.x = element_text(angle=70, hjust=1,  face = "bold")) +
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.x = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold")) + 
 ylab("SRP Effect") + xlab("Year")
peS


##--------QLD--------------##

peQ <- ggplot(testing, aes(x = Year, y = QLD)) +
  annotate("rect", ymin = -0.1, ymax = 0.1,
             xmin = -Inf, xmax = Inf, alpha = 0.3, fill = '#165459B2') +
  geom_boxplot() +
  scale_x_discrete(breaks = seq(1985, 2020, by = 5))+
  theme_bw(base_size=12) +
  theme(axis.text.x = element_text(angle=70, hjust=1,  face = "bold")) +
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.x = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold")) + 
  ylab("QLD Effect") + xlab("Year")

peQ


##--------SOI*PDO--------------##

pesoi <- ggplot(testing, aes(x = Year, y = SOI_PDO)) +
  annotate("rect", ymin = -0.1, ymax = 0.1,
           xmin = -Inf, xmax = Inf, alpha = 0.3, fill = '#165459B2') +
  geom_boxplot() +
  scale_x_discrete(breaks = seq(1985, 2020, by = 5))+
  theme_bw(base_size=12) +
  theme(axis.text.x = element_text(angle=70, hjust=1,  face = "bold")) +
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.x = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold")) + 
  ylab("SOI*PDO Effect") + xlab("Year")

pesoi


p_allEFF<- plot_grid(peD, peS,  peQ, pesoi, ncol = 2, align = "hv")
p_allEFF

ggsave('output/Chla GAM_partial Long-term.png', p_allEFF, height = 8, width  = 10)


##--------------------------------------------------##
## References
# Simpson, 2014: https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# Hayes et al, 2020: https://doi.org/10.1002/lol2.10164
# Simpson 2020 : https://stats.stackexchange.com/questions/471267/plotting-gams-on-response-scale-with-multiple-smooth-and-linear-terms
# Wilk et al., 2018: https://doi.org/10.1029/2018JG004506 (plotting response values)
