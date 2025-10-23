###############################################################################
####### BPWTP Data -- data analysis ##########
####### Danielle Spence ##########
####### Created 5/4/2023 ########
###############################################################################
### Clear memory
rm(list = ls())

library(pacman)
p_load(tidyverse, ggplot2, mgcv, reshape2, gratia, readr,  GGally, ggeffects, dplyr, cowplot,lehuynh,
       install = TRUE)

setwd("C:/Users/danis/OneDrive/R/BPWTP Chla GAMs")

##----------------------------------------------------------------------------##
## 1. Read in data 
##----------------------------------------------------------------------------##

df <- read_csv("data/bpgamdataCLEAN_chla.csv")
df$Date<- as.Date(df$Date, "%m/%d/%Y")
# 1244 obs

# add in Year and nMonth for numeric month and a proper Date class

## order the data
df <- df[with(df, order(Date)),]


# rename for ease of use
df <- df%>%
  dplyr::rename(
    date = "Date",
    chla = "Chla_ug.L")


#Remove rows without Chla data
df <- df[complete.cases(df$chla),]
# 1238 obs


df <- filter(df, chla != 0)
# 1206 obs


##----------------------------------------------------------------------------##
## 2. Check trends, distributions
##----------------------------------------------------------------------------##
# 
# df %>%
#   ggplot(aes(x = year, y = chla)) +
#   geom_point()+
#   geom_line()+
#   ggtitle("")
# 
# # check distributions
# 
# df %>%
#   select(chla) %>%
#   gather() %>%
#   ggplot(aes(value)) +
#   facet_wrap(~ key, scales = "free") +
#   geom_histogram() # chla has a strong positive skew, suggest using gamma or tweedie dist
# 

##----------------------------------------------------------------------------##
## 3. Modelling chlorophyll a over the Years
##----------------------------------------------------------------------------##

m<- gam(chla ~ 
            s(year) +
            s(DOY, bs = "cc", k=12)+
            ti(DOY,year, bs = c("cc", "tp")),
          knots=list(DOY=c(0, 366.5)),
          data = df, method = "REML", family = Gamma(link = "log"))


summary(m) # 42% of deviance explained, REML 4568, r sq = 0.395
draw(m, residuals =TRUE)

appraise(m, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  theme(plot.tag = element_text(face = 'bold')) & theme_bw()

k.check(m)

##----------------------------------------------------------------------------##
## 4. Checking & controlling for autocorrelation
##----------------------------------------------------------------------------##

layout(matrix(1:2, ncol = 2))
acf(resid(m), lag.max = 36, main = "ACF") 
pacf(resid(m), lag.max = 36, main = "pACF")
layout(1)

## Controlling for autocorrelation
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

m<- gamm(chla ~ s(DOY, bs = "cc", k=12) +
           s(year, bs="cr", k=20)+
           ti(DOY,year, bs=c("cc", "cr")), 
         knots=list(DOY=c(0, 366.5)),
         data = df, method = "REML", family=Gamma(link = "log"))

## AR(1)

m1 <- gamm(chla ~ s(DOY, bs = "cc", k=12) +
              s(year, bs="cr", k=20)+
              ti(DOY,year, bs=c("cc", "cr")), 
             knots=list(DOY=c(0, 366.5)),
            data = df, correlation = corARMA(form = ~ 1|year, p = 1),
            control = ctrl, method = "REML", family=Gamma(link = "log"))


## AR(2)
m2 <- gamm(chla ~ s(DOY, bs = "cc", k=12) +
             s(year, bs="cr", k=20)+
             ti(DOY,year, bs=c("cc", "cr")), 
           knots=list(DOY=c(0, 366.5)),
            data = df, correlation = corARMA(form = ~ 1|year, p = 2),
            control = ctrl, method = "REML", family=Gamma(link = "log"))

## AR(3)
m3 <- gamm(chla ~ s(DOY, bs = "cc", k=12) +
             s(year, bs="cr", k=20)+
             ti(DOY,year, bs=c("cc", "cr")), 
           knots=list(DOY=c(0, 366.5)),
            data = df, correlation = corARMA(form = ~ 1|year, p = 3),
            control = ctrl, method = "REML", family=Gamma(link = "log"))

AIC(m$lme, m1$lme, m2$lme, m3$lme) ## shows that m1 is best


layout(matrix(1:2, ncol = 2))
res <- resid(m1$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(1) errors")
acf(res, lag.max = 36, main = "pACF- AR(1) errors")
layout(1) # this is better

summary(m1$gam) # r squared = 0.4


k.check(m1$gam) 

draw(m1$gam, residuals = TRUE)


p<-appraise(m1$gam, point_col = 'steelblue', point_alpha = 0.5, n_bins = 'fd') & 
  theme(plot.tag = element_text(face = 'bold')) & theme_bw()
p

ggsave('output/chla time series residuals.png', p, height = 6, width  = 8)

##----------------------------------------------------------------------------##
## 7. Plotting on the response scale/fitted values and periods of significant change
##----------------------------------------------------------------------------##

#Derivatives 
d_chla<- derivatives(m1, select = "s(year)", type = "central", n=200)

colnames(d_chla) <- sub("^\\.", "", colnames(d_chla))

#does it change?
d_chla<- d_chla %>% mutate(Null_test = 0 >= lower_ci & 0 <= upper_ci)

# select change column...
d_chla <- d_chla %>%
  select( Null_test)


#new data to be used with predictors (length = output of derivatives f(x))
pdat <- with(df,
             data.frame(year = seq(min(year), max(year),
                                   length = 200),
                        DOY = median(DOY)))

#extract predictor and standard errors
p_chla <- predict(m1$gam, newdata = pdat, se.fit = TRUE)


#calculate confidence intervals for predictor
p_chla <- as.data.frame(p_chla)
p_chla <- p_chla %>% mutate("lower.p" = (fit-(se.fit*1.96)),
                        "upper.p" = (fit+(se.fit*1.96))) 

# bind to derivatives data frame
d_chla <- cbind(d_chla, p_chla, pdat)

# create continuous variable for Null_test (for figure)
d_chla$sig = if_else(d_chla$Null_test == "FALSE", 1, 2) 

#backtransform 
d_chla$fit<-exp(d_chla$fit)
d_chla$lower.p<-exp(d_chla$lower.p)
d_chla$upper.p<-exp(d_chla$upper.p)


#figure
p <- ggplot(d_chla, aes(year, fit)) +
  xlab("Year") + ylab(expression(paste("Chlorophyll ", italic("a"), " (", mu, "g/L)"))) +
  geom_ribbon(aes(ymin = lower.p, ymax = upper.p), fill = "#165459B2",  alpha = 0.2) +
  geom_point(data = df, aes(year,chla), alpha = 0.1) +
  geom_line(data = d_chla, aes(x = year, y = fit, colour=sig), alpha = 0.8, linewidth=1.5) +
  scale_colour_gradient(low = "violetred", high = "black", guide = NULL) +
  geom_line(data = d_chla, aes(x = year, y = lower.p), colour = "#165459B2") +
  geom_line(data = d_chla, aes(x = year, y = upper.p), colour = "#165459B2") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +  # Remove padding on x-axis
  theme_bw( base_family = 'Arial')+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1)) +
  theme(plot.title = element_text(vjust = -6, hjust = 0.05))
p

#ggsave('output/chla periods of change updated.png', p, height = 8, width  = 10)


#make a new variable for change periods 
new_chla <- d_chla %>% select(year, sig) 

## determine exact periods of change

library(lubridate)
new_chla$date <- format(date_decimal(new_chla$year), "%d-%m-%Y")

# 08-01-1992 to 01-10-1997

saveRDS(p, "DOY_chla.rds")

ggsave_elsevier("output/Chla_year.jpeg", plot = p, width = "one_half_column", height =  140)


##---------------- plot DOY

#Derivatives 
d_chla1<- derivatives(m1, select = "s(DOY)", type = "central", n=200)

colnames(d_chla1) <- sub("^\\.", "", colnames(d_chla1))

#does it change?
d_chla1<- d_chla1 %>% mutate(Null_test = 0 >= lower_ci & 0 <= upper_ci)

# select change column...
d_chla1 <- d_chla1 %>%
  select( Null_test)


#new data to be used with predictors (length = output of derivatives f(x))
pdat1 <- with(df,
             data.frame(DOY = seq(min(DOY), max(DOY),
                                   length = 200),
                        year = median(year)))

#extract predictor and standard errors
p_chla1 <- predict(m1$gam, newdata = pdat1, se.fit = TRUE)


#calculate confidence intervals for predictor
p_chla1 <- as.data.frame(p_chla1)
p_chla1 <- p_chla1 %>% mutate("lower.p" = (fit-(se.fit*1.96)),
                            "upper.p" = (fit+(se.fit*1.96))) 

# bind to derivatives data frame
d_chla1 <- cbind(d_chla1, p_chla1, pdat1)

# create continuous variable for Null_test (for figure)
d_chla1$sig = if_else(d_chla1$Null_test == "FALSE", 1, 2) 

#backtransform 
d_chla1$fit<-exp(d_chla1$fit)
d_chla1$lower.p<-exp(d_chla1$lower.p)
d_chla1$upper.p<-exp(d_chla1$upper.p)


#figure
p1 <- ggplot(d_chla1, aes(DOY, fit)) +
  xlab("Month") + ylab(expression(paste("Chlorophyll ", italic("a"), " (", mu, "g/L)"))) +
  geom_ribbon(aes(ymin = lower.p, ymax = upper.p), fill = "#165459B2",  alpha = 0.2) +
  geom_point(data = df, aes(DOY,chla), alpha = 0.1) +
  geom_line(data = d_chla1, aes(x = DOY, y = fit, colour=sig), alpha = 0.8, linewidth=1.5) +
  scale_colour_gradient(low = "violetred", high = "black", guide = NULL) +
  geom_line(data = d_chla1, aes(x = DOY, y = lower.p), colour = "#165459B2") +
  geom_line(data = d_chla1, aes(x = DOY, y = upper.p), colour = "#165459B2") +
  scale_x_continuous(expand = c(0, 0), breaks = c(5, 60,  121, 182, 244, 305, 350), labels = c("Jan", "Mar", "May", "Jul", "Sept", "Nov", "Dec")) +
  scale_y_continuous(expand = c(0, 0)) +  # Remove padding on x-axis
  theme_bw( base_family = 'Arial')+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1)) +
  theme(plot.title = element_text(vjust = -6, hjust = 0.05))
p1



#ggsave('output/chla DOY sig change pink.png', p, height = 8, width  = 10)
saveRDS(p1, "year_chla.rds")

p_all<- plot_grid(p, p1, align="hv", labels = c('A.', 'B.'))
p_all

#ggsave('output/chla DOY sig change both format.png', p_all, height = 6, width  = 8)
ggsave_elsevier("output/Figure_2.jpeg", plot = p_all, width = "one_half_column", height =  140)

saveRDS(p_all, "time_chla.rds")
#make a new variable for change periods 
new_chla1 <- d_chla1 %>% select(DOY, sig) 


##----------------------------------------------------------------------------##
## 9. Comparing chla trends over the years (code from Simpson, 2015)
##----------------------------------------------------------------------------###

pdat <- with(df,
                  data.frame(year = rep(1984:2022, each = 365),
                             DOY = rep(1:365, times = 39)))

pred <- predict(m1$gam, newdata = pdat, se.fit = TRUE)
crit <- qt(0.975, df = df.residual(m1$gam)) # ~95% interval critical t
pdat <- transform(pdat, fitted = pred$fit, se = pred$se.fit, fYear = as.factor(year))
pdat <- transform(pdat,
                  upper = fitted + (crit * se),
                  lower = fitted - (crit * se))

pdat$fitted<-exp(pdat$fitted)
pdat$se<-exp(pdat$se)
pdat$upper<-exp(pdat$upper)
pdat$lower<-exp(pdat$lower)

p1 <- ggplot(pdat, aes(x = DOY, y = fitted, group = fYear)) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper,
                            fill = year), alpha = 0.2) + # confidence band
  geom_line(aes(colour = year)) +    # predicted temperatures
  theme_bw(base_family = 'Arial') +                        # minimal theme
  theme(legend.position = "top") +    # push legend to the top
  scale_x_continuous(expand = c(0, 0), breaks = c(5, 60,  121, 182, 244, 305, 350), labels = c("Jan", "Mar", "May", "Jul", "Sept", "Nov", "Dec")) +
  scale_y_continuous(expand = c(0, 0)) +  # Remove padding on x-axis
  labs(y = expression(paste("Chlorophyll ", italic("a"), " (µg/L)")), x = "Month") +
  scale_fill_viridis_c(option = "plasma", direction = 1, name = "Year") +
  scale_colour_viridis_c(option = "plasma", direction = 1, name = "Year") 
  #scale_x_continuous(breaks = 1:12,   # tweak where the x-axis ticks are
                #     labels = month.abb, # & with what labels
                  #   minor_breaks = NULL)
p1


#ggsave('output/chla over time GAMMA .png', p1, height = 6, width  = 8)
ggsave_elsevier("output/Figure_3.jpeg", plot = p1, width = "one_half_column", height =  140)


##----------------------------------------------------------------------------##
## 7. Plotting the interaction on response scale/fitted values
##----------------------------------------------------------------------------##


new_data_year <- with(df, expand.grid(DOY = seq(min(DOY), max(DOY),length = 200),
                                      year = seq(min(year), max(year),length = 200)))

year.pred <- predict(m1$gam, newdata = new_data_year, type = "terms")

whichCols <- grep("DOY,year", colnames(year.pred))
#whichColsSE <- grep("year", colnames(year.pred$se.fit))

new_data_year <- cbind(new_data_year, Fitted = year.pred[, whichCols])

shiftcomb <- attr(year.pred, "constant")
year.pdatnorm <- new_data_year
year.pdatnorm <- with(year.pdatnorm, transform(year.pdatnorm, Fitted = Fitted + shiftcomb))

toofar <- exclude.too.far(year.pdatnorm$DOY, year.pdatnorm$year, df$DOY, df$year, dist=0.1)
year.pdatnorm$chla <- year.pdatnorm$Fitted
year.pdatnorm$chla[toofar] <- NA


year.pdatnorm$chla<-exp(year.pdatnorm$chla)


names(new_data_year)[which(names(new_data_year)=='year.pred')] <- 'Chla'

comboplot <- ggplot(year.pdatnorm, aes(x = year, y = DOY, z=chla)) + 
  theme_bw(base_size=14, base_family = 'Arial') +
  theme(legend.position='top') +
  geom_raster(aes(fill=chla)) + # change to turn grey background into nothing
  scale_fill_distiller(palette = "Spectral", direction = -1, na.value='transparent') +
  geom_point(data=df, aes(x=year, y=DOY, z=NULL)) +
  geom_contour(colour = "black", binwidth = 2) +
  theme(legend.key.width=unit(1.5,"cm")) +
  xlab("Year") + ylab("DOY") +
  labs(fill=expression(paste("Chlorophyll ", italic("a"), " (", mu, "g L"^-1*")   ")))+
  theme(legend.position = "top")


  #ggtitle(expression(paste("Chlorophyll ", italic("a"), " (", mu, "g L"^-1*")")))


comboplot

ggsave('output/Chla time series response values Interaction GAMMA.png', comboplot, height = 6, width  = 8)


##----------------------------------------------------------------------------##
## 9. Comparing predicted to observed (Code from Baron, 2023)
##----------------------------------------------------------------------------###


# i. predict Chla concentrations from GAM 
predM <- predict(m1$gam, type = 'response', se.fit = TRUE)

predM_df <- as_tibble(predM) %>% dplyr::rename(chlapred = fit, chlapred_se = se.fit)

# ii. bind to original dataframe
gambp_pred <- qpcR:::cbind.na(df, predM_df)

gambp_pred <- as_tibble(gambp_pred)

# iii. select date and observed vs. predicted

gambp_pred_long <- dplyr::select(gambp_pred, c(date, chla, chlapred)) 

gambp_pred_long <- gambp_pred_long %>%
  dplyr::rename(Observed = chla, Predicted = chlapred) %>% 
  pivot_longer(cols = -date,
               names_to = "fit",
               values_to = "Chla") 

gambp_pred_long %>%
  ggplot(aes(date, Chla, col = fit)) +
  geom_line(size = 1) +
  labs(y = 'Chla', col = NULL, x = 'Year')

# plot prediction with ribbon 
p_predYEAR <- gambp_pred %>% 
  ggplot(aes(date, chla)) +
  geom_point(alpha = 7/8, size = 1.5) + 
  geom_line(data = gambp_pred, aes(date, chlapred), size = 1, col = "steelblue3") + 
  geom_ribbon(data = gambp_pred,
              aes(ymin = chlapred - 1.96 * chlapred_se, 
                  ymax = chlapred + 1.96 * chlapred_se),
              fill = 'steelblue3', alpha = 1/2, size = 4) +
  labs(x = 'Year', y = expression(paste("Log(Chlorophyll ", italic("a"), " (?g L"^-1*"))")) ) 

p_predYEAR

#ggsave('output/predicted vs observed year.png', p_predYEAR, height = 8, width  = 10)


## decent fit, doesn't model extremes very well

# plot prediction with ribbon and faceted by year 
p_predDOY <- gambp_pred %>% 
  ggplot(aes(yday(date), chla_sqrt)) +
  facet_wrap(~ year, ncol = 10) +
  geom_point(alpha = 7/8, size = 1.5) + 
  geom_line(data = gambp_pred, aes(yday(date), chlapred, group = year), size = 1, col = "#165459B2") +
  geom_ribbon(data = gambp_pred,
              aes(ymin = chlapred - 1.96 * chlapred_se, 
                  ymax = chlapred + 1.96 * chlapred_se),
              fill = '#165459B2', alpha = 1/2) +
  theme_bw()+
  labs(x = 'Day of year', y = expression(paste("Chlorophyll ", italic("a"))))

p_predDOY 

ggsave('output/Chla predicted vs observed DOY NO 0s balanced sampling 2003.png', p_predDOY, height = 8, width  = 10)


##--------------------------------------------------##
## References
# Simpson, 2014: https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
#SImpson, 2015: https://fromthebottomoftheheap.net/2015/11/21/climate-change-and-spline-interactions/
# Painter et al., 2023: https://doi.org/10.1002/ecs2.4472
# Code for response scale plots: Wiik et al., 2023: 
# selection of tensor prods: https://pub.towardsai.net/gams-and-smoothing-splines-part-2-tensor-product-splines-97928f226a2c
# plotting predicted vs. observed: Baron, 2023: https://harvest.usask.ca/handle/10388/14623
