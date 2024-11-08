###############################################################################
####### CH.1 BPWTP -- data processing ##########
####### Danielle Spence ##########
####### Created 3/30/2021 ########
###############################################################################
### Clear memory
rm(list = ls())

library(pacman)
install.packages("weathercan", 
                 repos = c("https://ropensci.r-universe.dev", 
                           "https://cloud.r-project.org"))
p_load(tidyverse,  weathercan, dplyr, tidyr, geomtextpath, ggsci, cowplot,
       install = TRUE)

setwd("C:/Users/danis/OneDrive/R/BPWTP Chla GAMs")

##----------------------------------------------------------------------------##
## 1. load in Environment Canada weather data for Moose Jaw (more reliable)
##----------------------------------------------------------------------------##

stations_search(name = "MOOSE JAW") # 1998 and beyod

stations_search(name = "MOOSE JAW CS") # 1998 and beyod
stations_search(name = "MOOSE JAW A") # 1984 to 1998

EC1 <- weather_dl(station_ids = 2967, interval = "day", start = "1984-01-01", end = "1998-05-31")
EC2 <- weather_dl(station_ids = 27476, interval = "day", start = "1984-01-01", end = "2022-12-31")


EC <- rbind(EC1, EC2)

write.csv(EC, file="data/Moose Jaw Climate data.csv", row.names=F)


EC <- EC%>%
  dplyr::select(date, mean_temp, max_temp, total_precip, total_rain, total_snow)%>%
  dplyr::rename(Date = date)

EC<-mutate(EC,
           year = as.numeric(format(Date,'%Y')),
           DOY = as.numeric(format(Date,'%j')),
           month = as.numeric(format(Date,'%m')))


# mean temps

ann <-aggregate(EC$mean_temp,  
                by=list(EC$year),  
                FUN=mean,
                na.rm=TRUE) 
ann<- ann %>% dplyr::rename(mean = x)


ann1 <-aggregate(EC$mean_temp,  
                 by=list(EC$year),  
                 FUN = max,
                 na.rm=TRUE)
ann1 <- ann1 %>% dplyr::rename(max = x)


ann2 <-aggregate(EC$mean_temp,  
                 by=list(EC$year),  
                 FUN = min,
                 na.rm=TRUE)
ann2 <- ann2 %>% dplyr::rename(min = x)

annT<- left_join(ann, ann2,  by= "Group.1")
annT<- left_join(annT, ann1, by= "Group.1")


## total precip
annP <-aggregate(EC$total_precip,  
                by=list(EC$year),  
                FUN=sum,
                na.rm=TRUE) 

##----------------------------------------------------------------------------##
## 2. Explore time series for each variable... starting with precip
##----------------------------------------------------------------------------##

# basic plot

prec <- EC %>% 
  ggplot(aes(yday(Date), total_precip)) +
  facet_wrap(~ year, ncol = 10) +
  geom_point(alpha = 7/8, size = 1.5) + 
  #geom_smooth(data = EC, aes(yday(Date), total_precip, group = year), size = 1, col = "steelblue3") +
  labs(x = 'Day of year', y = "Total daily precipitation (mm)")+
  theme_bw(base_size = 14)

prec 

#ggsave('output/annual daily precip.png', prec, height = 8, width  = 10)


#precip_annual <-aggregate(EC$total_precip,  
#                          by=list(EC$year),  
#                          FUN=sum,   
#                          na.rm=TRUE) 
#mean(precip_annual$x)

precPlot <- ggplot(data=EC,  # the data frame
                          aes(year, total_precip)) +   # the variables of interest
  geom_bar(stat="identity") +   # create a bar graph
  xlab("Year") + ylab("Annual Precipitation (mm)") +  # label the x & y axes
  geom_hline(yintercept = 313, linetype="dashed")+
  scale_x_continuous(breaks = round(seq(min(EC$year), max(EC$year), by = 2)))+
  #ggtitle("Annual Precipitation (mm)\n 1984-2022")+
  theme_bw(base_size = 14) 

precPlot


#ggsave('output/annual total precip w avg.png', precPlot, height = 6, width  = 10)

EC$Date <- as.POSIXct(EC$Date)

p<- EC %>%
  group_by(year) %>%
  summarize(total = sum(total_precip, na.rm = TRUE), Date = median(Date)) %>% {
    ggplot(., aes(Date, total)) +
      geom_line(color = 'gray') +
      geom_point(color = 'gray75') +
      geom_textsegment(aes(x = as.POSIXct('1984-01-01'), 
                           xend = as.POSIXct('1989-12-31'),
                           y = mean(total), yend = mean(total), color = '1980s',
                           label = '1980s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('1990-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('1990-01-01'), 
                           xend = as.POSIXct('1999-12-31'),
                           y = mean(total), yend = mean(total), color = '1990s',
                           label = '1990s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2000-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2000-01-01'), 
                           xend = as.POSIXct('2009-12-31'),
                           y = mean(total), yend = mean(total), color = '2000s',
                           label = '2000s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2010-01-01') &
                                  .$Date > as.POSIXct('1999-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2010-01-01'), 
                           xend = as.POSIXct('2019-12-31'),
                           y = mean(total), yend = mean(total), color = '2010s',
                           label = '2010s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2020-01-01') &
                                  .$Date > as.POSIXct('2009-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2020-01-01'), 
                           xend = as.POSIXct('2022-12-31'),
                           y = mean(total), yend = mean(total), color = '2020s',
                           label = '2020s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2022-01-01') &
                                  .$Date > as.POSIXct('2019-12-31'),], linetype = 2) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'none') +
      labs(title = 'Annual Precipitation', x = 'Year', y = expression(paste("Annual precipitation (mm)")))+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())
  }

p<- p + scale_color_cosmic("hallmarks_dark")
p

ggsave('output/annual precip.png', p, height = 8, width  = 10)

## now look at rainfall...

p_r<- EC %>%
  group_by(year) %>%
  summarize(total = sum(total_rain, na.rm = TRUE), Date = median(Date)) %>% {
    ggplot(., aes(Date, total)) +
      geom_line(color = 'gray') +
      geom_point(color = 'gray75') +
      geom_textsegment(aes(x = as.POSIXct('1984-01-01'), 
                           xend = as.POSIXct('1989-12-31'),
                           y = mean(total), yend = mean(total), color = '1980s',
                           label = '1980s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('1990-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('1990-01-01'), 
                           xend = as.POSIXct('1999-12-31'),
                           y = mean(total), yend = mean(total), color = '1990s',
                           label = '1990s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2000-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2000-01-01'), 
                           xend = as.POSIXct('2009-12-31'),
                           y = mean(total), yend = mean(total), color = '2000s',
                           label = '2000s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2010-01-01') &
                                  .$Date > as.POSIXct('1999-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2010-01-01'), 
                           xend = as.POSIXct('2019-12-31'),
                           y = mean(total), yend = mean(total), color = '2010s',
                           label = '2010s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2020-01-01') &
                                  .$Date > as.POSIXct('2009-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2020-01-01'), 
                           xend = as.POSIXct('2022-12-31'),
                           y = mean(total), yend = mean(total), color = '2020s',
                           label = '2020s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2022-01-01') &
                                  .$Date > as.POSIXct('2019-12-31'),], linetype = 2) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'none') +
      labs(x = 'Year', y = expression(paste("Annual rainfall (mm)")))+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())
  }

p_r<- p_r + scale_color_cosmic("hallmarks_dark")
p_r

ggsave('output/annual rainfall.png', p_r, height = 8, width  = 10)

## and snowfall....

p_s<- EC %>%
  group_by(year) %>%
  summarize(total = sum(total_snow, na.rm = TRUE), Date = median(Date)) %>% {
    ggplot(., aes(Date, total)) +
      geom_line(color = 'gray') +
      geom_point(color = 'gray75') +
      geom_textsegment(aes(x = as.POSIXct('1984-01-01'), 
                           xend = as.POSIXct('1989-12-31'),
                           y = mean(total), yend = mean(total), color = '1980s',
                           label = '1980s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('1990-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('1990-01-01'), 
                           xend = as.POSIXct('1999-12-31'),
                           y = mean(total), yend = mean(total), color = '1990s',
                           label = '1990s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2000-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2000-01-01'), 
                           xend = as.POSIXct('2009-12-31'),
                           y = mean(total), yend = mean(total), color = '2000s',
                           label = '2000s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2010-01-01') &
                                  .$Date > as.POSIXct('1999-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2010-01-01'), 
                           xend = as.POSIXct('2019-12-31'),
                           y = mean(total), yend = mean(total), color = '2010s',
                           label = '2010s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2020-01-01') &
                                  .$Date > as.POSIXct('2009-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2020-01-01'), 
                           xend = as.POSIXct('2022-12-31'),
                           y = mean(total), yend = mean(total), color = '2020s',
                           label = '2020s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2022-01-01') &
                                  .$Date > as.POSIXct('2019-12-31'),], linetype = 2) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'none') +
      labs(x = 'Year', y = expression(paste("Annual snowfall (mm)")))
  }

p_s<- p_s + scale_color_cosmic("hallmarks_dark")
p_s

ggsave('output/annual snowfall.png', p_s, height = 8, width  = 10)

p_precip<- plot_grid(p, p_r, p_s,  ncol = 1, nrow = 3)
p_precip

ggsave('output/precipitation all.png', p_precip, height = 8, width  = 10)


##----------------------------------------------------------------------------##
## Air temperature
##----------------------------------------------------------------------------##

#mean(EC$max_temp, na.rm=TRUE) # annual mean max temp is 9.8

# group by year, find annual mean temps
mean_annual <-aggregate(EC$mean_temp,  
                              by=list(EC$year),  
                              FUN=mean,   
                              na.rm=TRUE) 
# basic plot

temp <- EC %>% 
  ggplot(aes(yday(Date), mean_temp)) +
  facet_wrap(~ year, ncol = 10) +
  geom_point(alpha = 7/8, size = 1.5) + 
  #geom_smooth(data = EC, aes(yday(Date), total_precip, group = year), size = 1, col = "steelblue3") +
  labs(x = 'Day of year', y =expression(paste("Mean daily air temperature (", degree*C,")")))+
  theme_bw(base_size = 14)

temp

#ggsave('output/annual mean daily temp.png', temp, height = 8, width  = 10)

## -------------------- Seasonal means.... ----------###

# first create season

seasons = function(x){
  if(x %in% 3:5) return("Spring")
  if(x %in% 6:8) return("Summer")
  if(x %in% 9:11) return("Fall")
  if(x %in% c(12,1,2)) return("Winter")
  
}


EC$Season = sapply(month(EC$Date), seasons)

# now group by seasons, find mean


mean_seasonal_max <-aggregate(EC$max_temp,  
                     by=list(EC$Season),  
                     FUN=mean,   
                     na.rm=TRUE) 
# means: summer = 25, spring & fall = 10, winter = -7

max <- EC %>% 
  ggplot(aes(year, max_temp)) +
  geom_line(aes(group = DOY))+
  facet_wrap(~ Season, ncol = 2) +
  labs(x = 'Year', y = expression(paste("Seasonal max daily air temperature (", degree*C,")")))+
  theme_bw(base_size = 14) 

max

#max + facet_grid(~factor(Season, levels=c('Fall', 'Winter', 'Spring', 'Summer')))

ggsave('output/seasonal max daily temps.png', max, height = 8, width  = 10)


mean_A <- ggplot(EC, aes(year, mean_temp, group = DOY)) +
  geom_boxplot() + 
  facet_wrap(~ Season, ncol = 2) +
  labs(x = 'Year', y = expression(paste("Mean daily air temperature (", degree*C,")")))+
  scale_fill_cosmic(palette="hallmarks_dark")+
  theme_bw(base_size = 16) 

mean_A

ggsave('output/seasonal mean daily temps.png', mean_A, height = 8, width  = 10)


both <- ggplot(EC, aes(year, mean_temp, group = DOY, fill = "Mean")) +
  geom_boxplot() + 
  geom_boxplot(data=EC, aes(year, max_temp, group = DOY, fill = "Max")) + 
  facet_wrap(~ Season, ncol = 2) +
  #geom_smooth(data = EC, aes(yday(Date), max_temp, group = year), size = 1, col = "steelblue3") +
  labs(x = 'Year', y = expression(paste("Air temperature (", degree*C,")")), fill="")+
  scale_fill_cosmic(palette="hallmarks_dark")+
  theme_bw(base_size = 16) 

both

mypal <- pal_cosmic("hallmarks_dark", alpha = 0.7)(9)
mypal


ggsave('output/seasonal mean daily temps combined.png', both, height = 8, width  = 10)

## -------------------- Annual means --------------------###

EC$Date <- as.POSIXct(EC$Date)

p1<- EC %>%
  group_by(year) %>%
  summarize(mean_max = mean(max_temp, na.rm = TRUE), Date = median(Date)) %>% {
    ggplot(., aes(Date, mean_max)) +
      geom_line(color = 'gray') +
      geom_point(color = 'gray75') +
      geom_textsegment(aes(x = as.POSIXct('1984-01-01'), 
                           xend = as.POSIXct('1989-12-31'),
                           y = mean(mean_max), yend = mean(mean_max), color = '1980s',
                           label = '1980s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('1990-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('1990-01-01'), 
                           xend = as.POSIXct('1999-12-31'),
                           y = mean(mean_max), yend = mean(mean_max), color = '1990s',
                           label = '1990s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2000-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2000-01-01'), 
                           xend = as.POSIXct('2009-12-31'),
                           y = mean(mean_max), yend = mean(mean_max), color = '2000s',
                           label = '2000s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2010-01-01') &
                                  .$Date > as.POSIXct('1999-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2010-01-01'), 
                           xend = as.POSIXct('2019-12-31'),
                           y = mean(mean_max), yend = mean(mean_max), color = '2010s',
                           label = '2010s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2020-01-01') &
                                  .$Date > as.POSIXct('2009-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2020-01-01'), 
                           xend = as.POSIXct('2022-12-31'),
                           y = mean(mean_max), yend = mean(mean_max), color = '2020s',
                           label = '2020s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2022-01-01') &
                                  .$Date > as.POSIXct('2019-12-31'),], linetype = 2) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'none') +
      labs(title = 'Max temperature', x = 'Year', y = expression(paste("Annual mean max air temperature ( " ,degree*C,")")))
  }
    
p1<- p1 + scale_color_cosmic("hallmarks_dark")
p1

#ggsave('output/mean max annual temp.png', p1, height = 8, width  = 10)

## annual mean air temp

p2<- EC %>%
  group_by(year) %>%
  summarize(mean = mean(mean_temp, na.rm = TRUE), Date = median(Date)) %>% {
    ggplot(., aes(Date, mean)) +
      geom_line(color = 'gray') +
      geom_point(color = 'gray75') +
      geom_textsegment(aes(x = as.POSIXct('1984-01-01'), 
                           xend = as.POSIXct('1989-12-31'),
                           y = mean(mean), yend = mean(mean), color = '1980s',
                           label = '1980s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('1990-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('1990-01-01'), 
                           xend = as.POSIXct('1999-12-31'),
                           y = mean(mean), yend = mean(mean), color = '1990s',
                           label = '1990s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2000-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2000-01-01'), 
                           xend = as.POSIXct('2009-12-31'),
                           y = mean(mean), yend = mean(mean), color = '2000s',
                           label = '2000s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2010-01-01') &
                                  .$Date > as.POSIXct('1999-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2010-01-01'), 
                           xend = as.POSIXct('2019-12-31'),
                           y = mean(mean), yend = mean(mean), color = '2010s',
                           label = '2010s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2020-01-01') &
                                  .$Date > as.POSIXct('2009-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2020-01-01'), 
                           xend = as.POSIXct('2022-12-31'),
                           y = mean(mean), yend = mean(mean), color = '2020s',
                           label = '2020s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2022-01-01') &
                                  .$Date > as.POSIXct('2019-12-31'),], linetype = 2) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'none') +
      labs(x = 'Year', y = expression(paste("Mean annual air temperature ( " ,        degree*C,")")))
  }

p2<- p2 + scale_color_cosmic("hallmarks_dark")
p2



ggsave('output/mean annual temp.png', p2, height = 6, width  = 10)


p_annual<- plot_grid(p1, p2,  ncol = 2, nrow = 1)
p_annual
ggsave('output/air temp series annual.png', p_annual, height = 6, width  = 10)



# now group by seasons, find autumn mean air temp

df_b<- EC%>%
  group_by(year, Season)%>%
  mutate(med_temp = median(mean_temp, na.rm=TRUE))


dfz<- df_b%>% 
  filter(Season == 'Fall')

dfz$date <- as.POSIXct(dfz$Date)

p1<- dfz %>%
  group_by(year) %>%
  summarize(med_temp = mean(med_temp, na.rm = TRUE), date = median(date)) %>%{
    ggplot(., aes(date, med_temp)) +
      geom_line(color = 'gray') +
      geom_point(color = 'gray75') +
      geom_textsegment(aes(x = as.POSIXct('1984-01-01'), 
                           xend = as.POSIXct('1989-12-31'),
                           y = mean(med_temp), yend = mean(med_temp), color = '1980s',
                           label = '1980s'), vjust = -0.2, size = 6,
                       data = .[.$date < as.POSIXct('1990-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('1990-01-01'), 
                           xend = as.POSIXct('1999-12-31'),
                           y = mean(med_temp), yend = mean(med_temp), color = '1990s',
                           label = '1990s'), vjust = -0.2, size = 6,
                       data = .[.$date < as.POSIXct('2000-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2000-01-01'), 
                           xend = as.POSIXct('2009-12-31'),
                           y = mean(med_temp), yend = mean(med_temp), color = '2000s',
                           label = '2000s'), vjust = -0.2, size = 6,
                       data = .[.$date < as.POSIXct('2010-01-01') &
                                  .$date > as.POSIXct('1999-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2010-01-01'), 
                           xend = as.POSIXct('2019-12-31'),
                           y = mean(med_temp), yend = mean(med_temp), color = '2010s',
                           label = '2010s'), vjust = -0.2, size = 6,
                       data = .[.$date < as.POSIXct('2020-01-01') &
                                  .$date > as.POSIXct('2009-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2020-01-01'), 
                           xend = as.POSIXct('2022-12-31'),
                           y = mean(med_temp), yend = mean(med_temp), color = '2020s',
                           label = '2020s'), vjust = -0.2, size = 6,
                       data = .[.$date < as.POSIXct('2022-01-01') &
                                  .$date > as.POSIXct('2019-12-31'),], linetype = 2) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'none') +
      labs(title = "Median air temperature in Autumn", x = 'Year', y = expression(paste("Median air temperature in Autumn (" , degree*C,")")))
  }

p1<- p1 + scale_color_cosmic("hallmarks_dark")
p1

ggsave('output/Autumn median air temp.png', p1, height = 8, width  = 10)



##----------------------------------------------------------------------------##
## Ice data
##----------------------------------------------------------------------------##


#---load in ice data, attach


ice<- read_csv("data/icecover.csv") 


#df1<- merge(df, ice, by='year', all=TRUE)


#df <- df1[with(df1, order(date)),]

## Create variable for whether the lake is frozen

#df <- df %>%
#  mutate(frozen = if_else(date <= iceON & date > iceOFF, 0, 1))

#df$frozen <-as.factor(df$frozen)

#Remove rows without ice data
#df <- df[complete.cases(df$frozen),]
ice$iceON <- as.POSIXct(ice$iceON)

p_on<- ice %>%
  group_by(year) %>%
  summarize(iceon = mean(iceondoy, na.rm = TRUE), Date = median(iceON)) %>% {
    ggplot(., aes(Date, iceon)) +
      geom_line(color = 'gray') +
      geom_point(color = 'gray75') +
      geom_textsegment(aes(x = as.POSIXct('1990-01-01'), 
                           xend = as.POSIXct('1999-12-31'),
                           y = mean(iceon), yend = mean(iceon), color = '1990s',
                           label = '1990s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2000-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2000-01-01'), 
                           xend = as.POSIXct('2009-12-31'),
                           y = mean(iceon), yend = mean(iceon), color = '2000s',
                           label = '2000s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2010-01-01') &
                                  .$Date > as.POSIXct('1999-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2010-01-01'), 
                           xend = as.POSIXct('2019-12-31'),
                           y = mean(iceon), yend = mean(iceon), color = '2010s',
                           label = '2010s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2020-01-01') &
                                  .$Date > as.POSIXct('2009-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2020-01-01'), 
                           xend = as.POSIXct('2022-12-31'),
                           y = mean(iceon), yend = mean(iceon), color = '2020s',
                           label = '2020s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2022-01-01') &
                                  .$Date > as.POSIXct('2019-12-31'),], linetype = 2) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'none') +
      labs(title = 'Ice On', x = 'Year', y = expression(paste("Day of year of ice on")))
  }

p_on<- p_on + scale_color_cosmic("hallmarks_dark")
p_on

ice$iceOFF <- as.POSIXct(ice$iceOFF)

p_off<- ice %>%
  group_by(year) %>%
  summarize(iceoff = mean(iceoffdoy, na.rm = TRUE), Date = median(iceOFF)) %>% {
    ggplot(., aes(Date, iceoff)) +
      geom_line(color = 'gray') +
      geom_point(color = 'gray75') +
      geom_textsegment(aes(x = as.POSIXct('1990-01-01'), 
                           xend = as.POSIXct('1999-12-31'),
                           y = mean(iceoff), yend = mean(iceoff), color = '1990s',
                           label = '1990s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2000-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2000-01-01'), 
                           xend = as.POSIXct('2009-12-31'),
                           y = mean(iceoff), yend = mean(iceoff), color = '2000s',
                           label = '2000s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2010-01-01') &
                                  .$Date > as.POSIXct('1999-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2010-01-01'), 
                           xend = as.POSIXct('2019-12-31'),
                           y = mean(iceoff), yend = mean(iceoff), color = '2010s',
                           label = '2010s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2020-01-01') &
                                  .$Date > as.POSIXct('2009-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2020-01-01'), 
                           xend = as.POSIXct('2022-12-31'),
                           y = mean(iceoff), yend = mean(iceoff), color = '2020s',
                           label = '2020s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2022-01-01') &
                                  .$Date > as.POSIXct('2019-12-31'),], linetype = 2) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'none') +
      labs(title = 'Ice Off', x = 'Year', y = expression(paste("Day of year of ice off")))
  }
p_off <- p_off + scale_color_cosmic("hallmarks_dark")
p_off


p_ice<-plot_grid(p_on, p_off,  ncol = 2, nrow = 1)
p_ice
ggsave('output/ice dates.png', p_ice, height = 6, width  = 10)
