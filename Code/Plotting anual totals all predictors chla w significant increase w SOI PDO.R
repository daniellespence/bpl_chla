###############################################################################
####### CH.1 BPWTP -- data processing ##########
####### Danielle Spence ##########
####### Created 3/30/2021 ########
###############################################################################
### Clear memory
rm(list = ls())

library(pacman)
p_load(tidyverse, dplyr, tidyr, geomtextpath, ggsci, cowplot, patchwork,lehuynh,
       install = TRUE)


setwd("C:/Users/danis/OneDrive/R/BPWTP Chla GAMs")



#SOI
phases <- data.frame(
  phase = c("Warm", "Cold", "Warm", "Cold", "Warm", "Cold", "Warm", "Cold", "Warm", "Cold"),
  start = c(1990, 1995, 1997, 1998, 2002, 2007, 2014, 2016, 2018, 2020),
  end = c(1995, 1997, 1998, 2002, 2007, 2014, 2016, 2018, 2020, 2022)
)


#PDO
phases_PDO <- data.frame(
  phase = c("Cold","Warm","Cold", "Warm", "Cold", "Warm", "Cold", "Warm", "Cold"),
  start = c(1990, 1992, 1994, 1995, 1998, 2003, 2004, 2014, 2016),
  end = c(1992, 1994, 1995, 1998, 2003, 2004, 2014, 2016, 2022)
)

#In-phase

in_phase <- data.frame(
  phase = c("Warm", "Cold", "Warm","Warm","Cold", "Warm", "Cold", "Warm", "Cold", "Cold"),
  start = c(1986, 1988, 1992, 1997, 1998, 2003, 2007, 2014, 2016, 2020 ),
  end = c(1988, 1990, 1993, 1998, 2001, 2004, 2014, 2016, 2019, 2022 )
)


##----------------------------------------------------------------------------##
## 1. load in flow data
##----------------------------------------------------------------------------##
## Read in data 
df <- read_csv("data/bpgamdataCLEAN_flow.csv")

# add in Year and nMonth for numeric month and a proper Date class


df$Date<- as.Date(df$Date, "%m/%d/%Y")

df <- mutate(df,
             year = as.numeric(format(Date,'%Y')),
             DOY = as.numeric(format(Date,'%j')),
             nMonth = as.numeric(format(Date,'%m')))%>% 
  filter(year %in% c(1984:2022))


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
    TP = "TP_ug.L",
    W_temp = "Temp_C",
    QLD = "SK05JG006.cms",
    QWS = "RC_IC_cms",
    orgN = "Org_N_mg.L")


#SIGNIFICANT INCREASE
sig_increase <- data.frame(
  phase = c("Significant increase"),
  start = c(1992),
  end = c(1997)
)



##----------------------------------------------------------------------------##
## 2. Explore time series for each variable
##----------------------------------------------------------------------------##


#---------------------------------------- plot annual average chla 

#Remove rows without chla data
df_c <- df[complete.cases(df$chla),]

ann <-aggregate(df_c$chla,  
                by=list(df_c$year),  
                FUN=mean,
                na.rm=TRUE) 
ann<- ann %>% dplyr::rename(mean = x)


ann1 <-aggregate(df_c$chla,  
                 by=list(df_c$year),  
                 FUN = max,
                 na.rm=TRUE)
ann1 <- ann1 %>% dplyr::rename(max = x)


ann2 <-aggregate(df_c$chla,  
                 by=list(df_c$year),  
                 FUN = min,
                 na.rm=TRUE)
ann2 <- ann2 %>% dplyr::rename(min = x)

anncl<- left_join(ann, ann1,  by= "Group.1")
anncl<- left_join(anncl, ann2, by= "Group.1")


p_anCL<- anncl %>%
  ggplot(aes(x = Group.1, y = mean)) +
  geom_point()+
  geom_ribbon(aes(ymin = min, ymax = max), fill = '#165459B2', alpha = 1/2) +
  geom_line()+
  ggtitle("Chl.a")+
  xlab("Year") + ylab(expression(paste("Chl. ", italic("a"), " (", mu, "g L"^-1*")")))+
  theme_bw(base_size = 14)+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold")) +
theme(axis.title.x = element_blank(),
      axis.text.x = element_blank()) #+

p_anCL

# p_anCL <- p_anCL +
#   geom_rect(data = sig_increase, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
#             alpha = 0.3,  inherit.aes = FALSE) +                 # Transparency for shading
#   scale_fill_manual(values = ("Signifcant increase" = "violetred"))+ theme(legend.position = "none")
# 
# 
# p_anCL

p_anCL <- p_anCL +
  geom_rect(data = in_phase, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.2,  inherit.aes = FALSE) +                 # Transparency for shading
  scale_fill_manual(values = c("Warm" = "red", "Cold" = "blue")) 
p_anCL 

##---------- plot annual SRP loads


#Remove rows without complete data
df_s <- df[complete.cases(df$SRP),]

ann <-aggregate(df_s$SRP,  
                by=list(df_s$year),  
                FUN=mean,
                na.rm=TRUE) 
ann<- ann %>% dplyr::rename(mean = x)

ann1 <-aggregate(df_s$SRP,  
                 by=list(df_s$year),  
                 FUN = max,
                 na.rm=TRUE)
ann1 <- ann1 %>% dplyr::rename(max = x)

ann2 <-aggregate(df_s$SRP,  
                 by=list(df_s$year),  
                 FUN = min,
                 na.rm=TRUE)
ann2 <- ann2 %>% dplyr::rename(min = x)

annSRP<- left_join(ann, ann1,  by= "Group.1")
annSRP<- left_join(annSRP, ann2, by= "Group.1")

p_anSRP<- annSRP %>%
  ggplot(aes(x = Group.1, y = mean)) +
  geom_point()+
  geom_ribbon(aes(ymin = min, ymax = max), fill = '#165459B2', alpha = 1/2) +
  geom_line()+
  ggtitle("SRP")+
  xlab("Year") + ylab(expression(paste("SRP ", "(", mu, "g L"^-1*")")))+
  theme_bw(base_size = 14)+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
p_anSRP

# p_anSRP<- p_anSRP +
#   geom_rect(data = phases, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
#             alpha = 0.3,  inherit.aes = FALSE) +                 # Transparency for shading
#   scale_fill_manual(values = ("Signifcant increase" = "violetred"))+ theme(legend.position = "none")
# 
# p_anSRP

p_anSRP <- p_anSRP +
  geom_rect(data = in_phase, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.2,  inherit.aes = FALSE) +                 # Transparency for shading
  scale_fill_manual(values = c("Warm" = "red", "Cold" = "blue")) 
p_anSRP 

#---------------------------------------------- plot annual DIN totals



df$DIN <- rowSums(df[,c("NO3", "NH3")], na.rm=TRUE)

df1 <- df[complete.cases(df$DIN),]

ann <-aggregate(df1$DIN,  
                by=list(df1$year),  
                FUN=mean,
                na.rm=TRUE) 
ann<- ann %>% dplyr::rename(mean = x)

ann1 <-aggregate(df1$DIN,  
                 by=list(df1$year),  
                 FUN = max,
                 na.rm=TRUE)
ann1 <- ann1 %>% dplyr::rename(max = x)

ann2 <-aggregate(df1$DIN,  
                 by=list(df1$year),  
                 FUN = min,
                 na.rm=TRUE)
ann2 <- ann2 %>% dplyr::rename(min = x)

annDIN<- left_join(ann, ann1,  by= "Group.1")
annDIN<- left_join(annDIN, ann2, by= "Group.1")


p_anDIN<- annDIN %>%
  ggplot(aes(x = Group.1, y = mean)) +
  geom_point()+
  geom_ribbon(aes(ymin = min, ymax = max), fill = '#165459B2', alpha = 1/2) +
  geom_line()+
  ggtitle("DIN")+
  xlab("Year") + ylab(expression(paste("DIN ", "(mg L"^-1*")")))+
  theme_bw(base_size = 14)+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
p_anDIN

# 
# p_anDIN<- p_anDIN +
#   geom_rect(data = phases, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
#             alpha = 0.3,  inherit.aes = FALSE) +                 # Transparency for shading
#   scale_fill_manual(values = ("Signifcant increase" = "violetred"))+ theme(legend.position = "none")
# 
# p_anDIN

p_anDIN <- p_anDIN +
  geom_rect(data = in_phase, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.2,  inherit.aes = FALSE) +                 # Transparency for shading
  scale_fill_manual(values = c("Warm" = "red", "Cold" = "blue")) 
p_anDIN 


#--------------------------------------------------- plot annual QLD totals

## load flow data separately (to get fuller sense of wet and dry periods)

flow <- read_csv("Data/daily flow.csv")

flow<- flow %>%
  select(date, combined_05JG004.cms, SK05JG006.cms, RC_IC_cms)%>%
  dplyr::rename("Date" = date,
                QLD = "SK05JG006.cms",
                QWS = "RC_IC_cms",)
flow <- mutate(flow,
               year = as.numeric(format(Date,'%Y')),
               DOY = as.numeric(format(Date,'%j')),
               nMonth = as.numeric(format(Date,'%m')))%>% 
  filter(year %in% c(1984:2022))


ann <-aggregate(flow$QLD,  
                by=list(flow$year),  
                FUN=mean,
                na.rm=TRUE) 
ann<- ann %>% dplyr::rename(mean = x)

ann1 <-aggregate(flow$QLD,  
                 by=list(flow$year),  
                 FUN = max,
                 na.rm=TRUE)
ann1 <- ann1 %>% dplyr::rename(max = x)

ann2 <-aggregate(flow$QLD,  
                 by=list(flow$year),  
                 FUN = min,
                 na.rm=TRUE)
ann2 <- ann2 %>% dplyr::rename(min = x)

annLD<- left_join(ann, ann1,  by= "Group.1")
annLD<- left_join(annLD, ann2, by= "Group.1")

p_an<- annLD %>%
  ggplot(aes(x = Group.1, y = mean)) +
  geom_point()+
  geom_ribbon(aes(ymin = min, ymax = max), fill = '#165459B2', alpha = 1/2) +
  geom_line()+
  ggtitle("QLD")+
  xlab("Year") + ylab(expression(paste(
    "QLD (",
    m^3, s^1,
    ")")))+
  theme_bw(base_size = 14)+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold"))+
  theme(axis.text.x = element_text(face = "bold", size=14))+
  theme(axis.title.x = element_text(face = "bold", size=16))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
p_an

# p_an <- p_an  +
#   geom_rect(data = phases, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
#             alpha = 0.3,  inherit.aes = FALSE) +                 # Transparency for shading
#   scale_fill_manual(values = ("Signifcant increase" = "violetred"))+ theme(legend.position = "none")
# 
# p_an 

p_an <- p_an +
  geom_rect(data = in_phase, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.2,  inherit.aes = FALSE) +                 # Transparency for shading
  scale_fill_manual(values = c("Warm" = "red", "Cold" = "blue")) 
p_an 


#------------------------------------------- plot annual total WS contributions

ann <-aggregate(flow$QWS,  
                by=list(flow$year),  
                FUN=mean,
                na.rm=TRUE) 
ann<- ann %>% dplyr::rename(mean = x)

ann1 <-aggregate(flow$QWS,  
                 by=list(flow$year),  
                 FUN = max,
                 na.rm=TRUE)
ann1 <- ann1 %>% dplyr::rename(max = x)

ann2 <-aggregate(flow$QWS,  
                 by=list(flow$year),  
                 FUN = min,
                 na.rm=TRUE)
ann2 <- ann2 %>% dplyr::rename(min = x)

annWS<- left_join(ann, ann1,  by= "Group.1")
annWS<- left_join(annWS, ann2, by= "Group.1")

p_anWS<- annWS %>%
  ggplot(aes(x = Group.1, y = mean)) +
  geom_point()+
  geom_ribbon(aes(ymin = min, ymax = max), fill = '#165459B2', alpha = 1/2) +
  geom_line()+
  ggtitle("QWS")+
  xlab("Year") + ylab(expression(paste(
    "QWS (",
    m^3, s^1,
    ")")))+
  theme_bw(base_size = 14)+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold"))+
  theme(axis.text.x = element_text(face = "bold", size=14))+
  theme(axis.title.x = element_text(face = "bold", size=16)) #+
# theme(axis.title.x = element_blank(),
#       axis.text.x = element_blank())
p_anWS


# p_anWS<- p_anWS +
#   geom_rect(data = phases, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
#             alpha = 0.3,  inherit.aes = FALSE) +                 # Transparency for shading
#   scale_fill_manual(values = ("Signifcant increase" = "violetred"))+ theme(legend.position = "none")
# 
# p_anWS

p_anWS <- p_anWS +
  geom_rect(data = in_phase, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.2,  inherit.aes = FALSE) +                 # Transparency for shading
  scale_fill_manual(values = c("Warm" = "red", "Cold" = "blue")) 
p_anWS 

# ------------------ put it all together -------------------------------#

p_all<- (p_anCL/ p_anSRP/  p_anDIN/ p_an/ p_anWS)+ plot_layout(guides = "collect") 
p_all


ggsave_elsevier("output/Timeline all.jpeg", plot = p_all, width = "full_page", height =  240)


p_all<- (p_anSRP/  p_anDIN/ p_an/ p_anWS)+ plot_layout(guides = "collect") 
p_all

ggsave('output/annual totals predictors sig increase.png', p_all, height = 18, width  = 16, dpi = 320)















###----------------------------------------------------------------------------
### Decadal averages-water temperature

df$date <- as.POSIXct(df$date)

p1<- df %>%
  group_by(year) %>%
  summarize(max = max(W_temp, na.rm = TRUE), Date = median(date)) %>% {
    ggplot(., aes(Date, max)) +
      geom_line(color = 'gray') +
      geom_point(color = 'gray75') +
      geom_textsegment(aes(x = as.POSIXct('1984-01-01'), 
                           xend = as.POSIXct('1989-12-31'),
                           y = mean(max), yend = mean(max), color = '1980s',
                           label = '1980s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('1990-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('1990-01-01'), 
                           xend = as.POSIXct('1999-12-31'),
                           y = mean(max), yend = mean(max), color = '1990s',
                           label = '1990s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2000-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2000-01-01'), 
                           xend = as.POSIXct('2009-12-31'),
                           y = mean(max), yend = mean(max), color = '2000s',
                           label = '2000s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2010-01-01') &
                                  .$Date > as.POSIXct('1999-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2010-01-01'), 
                           xend = as.POSIXct('2019-12-31'),
                           y = mean(max), yend = mean(max), color = '2010s',
                           label = '2010s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2020-01-01') &
                                  .$Date > as.POSIXct('2009-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2020-01-01'), 
                           xend = as.POSIXct('2022-12-31'),
                           y = mean(max), yend = mean(max), color = '2020s',
                           label = '2020s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2022-01-01') &
                                  .$Date > as.POSIXct('2019-12-31'),], linetype = 2) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'none') +
      labs(title = 'Max water temperature', x = 'Year', y = expression(paste("Annual max water temperature (  " , degree*C,")")))
  }

p1<- p1 + scale_color_cosmic("hallmarks_dark")
p1

ggsave('output/max annual water temp.png', p1, height = 8, width  = 10)


p2<- df %>%
  group_by(year) %>%
  summarize(max = mean(W_temp, na.rm = TRUE), Date = median(date)) %>% {
    ggplot(., aes(Date, max)) +
      geom_line(color = 'gray') +
      geom_point(color = 'gray75') +
      geom_textsegment(aes(x = as.POSIXct('1984-01-01'), 
                           xend = as.POSIXct('1989-12-31'),
                           y = mean(max), yend = mean(max), color = '1980s',
                           label = '1980s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('1990-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('1990-01-01'), 
                           xend = as.POSIXct('1999-12-31'),
                           y = mean(max), yend = mean(max), color = '1990s',
                           label = '1990s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2000-01-01'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2000-01-01'), 
                           xend = as.POSIXct('2009-12-31'),
                           y = mean(max), yend = mean(max), color = '2000s',
                           label = '2000s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2010-01-01') &
                                  .$Date > as.POSIXct('1999-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2010-01-01'), 
                           xend = as.POSIXct('2019-12-31'),
                           y = mean(max), yend = mean(max), color = '2010s',
                           label = '2010s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2020-01-01') &
                                  .$Date > as.POSIXct('2009-12-31'),], linetype = 2) +
      geom_textsegment(aes(x = as.POSIXct('2020-01-01'), 
                           xend = as.POSIXct('2022-12-31'),
                           y = mean(max), yend = mean(max), color = '2020s',
                           label = '2020s'), vjust = -0.2, size = 6,
                       data = .[.$Date < as.POSIXct('2022-01-01') &
                                  .$Date > as.POSIXct('2019-12-31'),], linetype = 2) +
      theme_bw(base_size = 14) +
      theme(legend.position = 'none') +
      labs(title = 'Mean water temperature', x = 'Year', y = expression(paste("Annual mean water temperature (  " , degree*C,")")))
  }

p2<- p2 + scale_color_cosmic("hallmarks_dark")
p2

ggsave('output/mean annual water temp.png', p2, height = 8, width  = 10)


mean_annual <-aggregate(df$W_temp,  
                        by=list(df$year),  
                        FUN=mean,   
                        na.rm=TRUE) 

max <-aggregate(df$W_temp,  
                        by=list(df$year),  
                        FUN=max,   
                        na.rm=TRUE) 



#--------------------------------plot annual TN totals

df$TN <- rowSums(df[,c("NO3", "NH3", "orgN")], na.rm=TRUE)

df1 <- df[complete.cases(df$TN),]

ann <-aggregate(df1$TN,  
                by=list(df1$year),  
                FUN=mean,
                na.rm=TRUE) 
ann<- ann %>% dplyr::rename(mean = x)

ann1 <-aggregate(df1$TN,  
                 by=list(df1$year),  
                 FUN = max,
                 na.rm=TRUE)
ann1 <- ann1 %>%dplyr::rename(max = x)

ann2 <-aggregate(df1$TN,  
                 by=list(df1$year),  
                 FUN = min,
                 na.rm=TRUE)
ann2 <- ann2 %>% dplyr::rename(min = x)

annTN<- left_join(ann, ann1,  by= "Group.1")
annTN<- left_join(annTN, ann2, by= "Group.1")

p_anTN<- annTN %>%
  ggplot(aes(x = Group.1, y = mean)) +
  geom_point()+
  geom_ribbon(aes(ymin = min, ymax = max), fill = '#165459B2', alpha = 1/2) +
  geom_line()+
  ggtitle("TN")+
  xlab("Year") + ylab(expression(paste("TN ", "(mg L"^-1*")")))+
  theme_bw(base_size = 14)+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
p_anTN


p_anTN<- p_anTN +
  geom_rect(data = phases, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.3,  inherit.aes = FALSE) +                 # Transparency for shading
  scale_fill_manual(values = ("Signifcant increase" = "violetred"))+ theme(legend.position = "none")

p_anTN
#---------------------------------------------- plot annual water temp

ann <-aggregate(df$W_temp,  
                by=list(df$year),  
                FUN=mean,
                na.rm=TRUE) 
ann<- ann %>% dplyr::rename(mean = x)

ann1 <-aggregate(df$W_temp,  
                 by=list(df$year),  
                 FUN = max,
                 na.rm=TRUE)
ann1 <- ann1 %>% dplyr::rename(max = x)

ann2 <-aggregate(df$W_temp,  
                 by=list(df$year),  
                 FUN = min,
                 na.rm=TRUE)
ann2 <- ann2 %>% dplyr::rename(min = x)

annW<- left_join(ann, ann1,  by= "Group.1")
annW<- left_join(annW, ann2, by= "Group.1")

p_anW<- annW %>%
  ggplot(aes(x = Group.1, y = mean)) +
  geom_point()+
  geom_ribbon(aes(ymin = min, ymax = max), fill = '#165459B2', alpha = 1/2) +
  geom_line()+ 
  theme_bw(base_size = 14)+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold"))+
  theme(axis.text.x = element_text(face = "bold", size=14))+
  theme(axis.title.x = element_text(face = "bold", size=16)) +
  ggtitle("Water Temperature")+
  xlab("Year") + ylab(expression(paste("Water temp ( " , degree*C,")"))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  theme(axis.text.x = element_text(face = "bold", size=14))+
  theme(axis.title.x = element_text(face = "bold", size=16))
p_anW

p_anW<- p_anW +
  geom_rect(data = phases, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.3,  inherit.aes = FALSE) +                 # Transparency for shading
  scale_fill_manual(values = ("Signifcant increase" = "violetred"))+ theme(legend.position = "none")

p_anW

#------------------------------------------- plot annual TP loads

ann <-aggregate(df_s$TP,  
                by=list(df_s$year),  
                FUN=mean,
                na.rm=TRUE) 
ann<- ann %>% dplyr::rename(mean = x)

ann1 <-aggregate(df_s$TP,  
                 by=list(df_s$year),  
                 FUN = max,
                 na.rm=TRUE)
ann1 <- ann1 %>% dplyr::rename(max = x)

ann2 <-aggregate(df_s$TP,  
                 by=list(df_s$year),  
                 FUN = min,
                 na.rm=TRUE)
ann2 <- ann2 %>% dplyr::rename(min = x)

annTP<- left_join(ann, ann1,  by= "Group.1")
annTP<- left_join(annTP, ann2, by= "Group.1")

p_anTP<- annTP %>%
  ggplot(aes(x = Group.1, y = mean)) +
  geom_point()+
  geom_ribbon(aes(ymin = min, ymax = max), fill = '#165459B2', alpha = 1/2) +
  geom_line()+
  theme_bw(base_size = 14)+
  theme(axis.text.y = element_text(face = "bold"))+
  theme(axis.title.y = element_text(face = "bold")) +
  ggtitle("TP")+
  xlab("Year") + ylab(expression(paste("TP  ", "(", mu, "g L"^-1*")")))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
p_anTP

p_anTP<- p_anTP +
  geom_rect(data = phases, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.3,  inherit.aes = FALSE) +                 # Transparency for shading
  scale_fill_manual(values = ("Signifcant increase" = "violetred"))+ theme(legend.position = "none")

p_anTP

#ggsave('output/TP Annual total.png', p_anTP, height = 8, width  = 10)


