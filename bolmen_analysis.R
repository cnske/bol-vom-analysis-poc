setwd("C:/Users/luzie/Lund University/Clemens Klante - Intern - Luzie Rieth/bol-vom-analysis-poc/data")
#setwd("./data")
#getwd()

#install.packages("here")
library(tidyverse) #includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, lubridate
library(doBy)
library(multcomp)
library(EnvStats)
library(readxl)
library(ggtext)
library(readtext)
library(base)
library(nanoparquet)
library(patchwork)
library(scico)
library(arrow)
library(here) #? and plotly?
#library(car)  ?
#library(psych)  ?
#library(lsr)  ?
#library(RColorBrewer)  ?
#library(scales)    ?




############ Bolmen depth measurements ############ 

## open csv ##
skipBolmE <- which((readLines(con="SITES_SONDE-PROF_BOL_BS-E_20220413-20251103_L2_irregular.csv"))=="####")
BolmE <- read.csv("SITES_SONDE-PROF_BOL_BS-E_20220413-20251103_L2_irregular.csv", skip = skipBolmE, sep=",", header = T, dec=".", stringsAsFactors = T)
BolmE <- mutate(BolmE, location = "BOL_BS-E")

skipBolmNE <- which((readLines(con="SITES_SONDE-PROF_BOL_BS-NE_20220510-20251103_L2_irregular.csv"))=="####")
BolmNE<- read.csv("SITES_SONDE-PROF_BOL_BS-NE_20220510-20251103_L2_irregular.csv", skip = skipBolmNE, sep=",", header = T, dec=".", stringsAsFactors = T)
BolmNE <- mutate(BolmNE, location = "BOL_BS-NE")

skipBolmNW <- which((readLines(con="SITES_SONDE-PROF_BOL_BS-NW_20220510-20251103_L2_irregular.csv"))=="####")
BolmNW<- read.csv("SITES_SONDE-PROF_BOL_BS-NW_20220510-20251103_L2_irregular.csv", skip = skipBolmNW, sep=",", header = T, dec=".", stringsAsFactors = T)
BolmNW <- mutate(BolmNW, location = "BOL_BS-NW")

skipBolmS <- which((readLines(con="SITES_SONDE-PROF_BOL_BS-S_20220712-20251103_L2_irregular.csv"))=="####")
BolmS<- read.csv("SITES_SONDE-PROF_BOL_BS-S_20220712-20251103_L2_irregular.csv", skip = skipBolmS, sep=",", header = T, dec=".", stringsAsFactors = T)
BolmS <- mutate(BolmS, location = "BOL_BS-S")

skipBolmW <- which((readLines(con="SITES_SONDE-PROF_BOL_BS-W_20220413-20251103_L2_irregular.csv"))=="####")
BolmW<- read.csv("SITES_SONDE-PROF_BOL_BS-W_20220413-20251103_L2_irregular.csv", skip = skipBolmW, sep=",", header = T, dec=".", stringsAsFactors = T)
BolmW <- mutate(BolmW, location = "BOL_BS-W")

## combine data frames ##
BolmProfile <- rbind(BolmE, BolmNE, BolmNW, BolmS, BolmW) 

BolmProfile <- mutate(BolmProfile, date=substring(BolmProfile$TIMESTAMP, 0, 10) )
BolmProfile <- mutate(BolmProfile, year=substring(BolmProfile$TIMESTAMP, 0, 4) )
BolmProfile <- mutate(BolmProfile, season=substring(BolmProfile$TIMESTAMP, 6, 10) )

BolmProfile$year <- as.factor(BolmProfile$year)
BolmProfile$location <- as.factor(BolmProfile$location)
BolmProfile$date <- as.Date( BolmProfile$date, format="%Y-%m-%d")
BolmProfile$season <- as.Date( BolmProfile$season, format="%m-%d")

str(BolmProfile)
summary(BolmProfile)

## plotting ##
windows()   #each basins throughout the years
ggplot(BolmProfile, aes(date, D_Water, color=O2SAT))+
  geom_point()+
  scale_x_date(date_labels = ("%m/%y"), date_breaks="3 months")+
  scale_y_reverse()+
  facet_grid(location ~.)

windows()   #all years, only one basin
ggplot((subset(BolmProfile, location=="BOL_BS-S")), aes(season, D_Water, color=O2SAT))+
  geom_point()+
  scale_x_date(date_labels = ("%b %y"))+
  scale_y_reverse()+
  facet_grid(year ~.)



############ contour plots with interpolation ####################

#setting Dataset for interpolation (location, time, value)
df <- subset(BolmProfile, location=="BOL_BS-NW" & year=="2024")
df$depth <- c(df$D_Water)
df$value <- c(df$O2SAT)
#just plotting measurements
ggplot(df,aes(x=date, y=depth, color=value))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(midpoint = (max(df$value) - ((max(df$value)-min(df$value))/2)),
    low = scales::muted("blue"),mid="white", high = scales::muted("red"))
#define function to estimate value, at a given date, for any depth
estimate_val_by_date <- function(target_date, target_depth) {
  data_for_date <- df %>%        
    filter(date == target_date) %>%
    arrange(depth)
  approx(data_for_date$depth, data_for_date$value, xout = target_depth, ties = min)$y }
#try estimation for earliest date of df and specific depths
estimate_val_by_date(ymd(min(df$date)), c(0, 1, 5, 10)) 
#creating tibble with calculated values (for each given date and wanted depths)
val_interp_depth <- crossing(
  tibble(date = unique(df$date)), #sampling days
  tibble(depth = seq(1, (floor(max(df$depth))), length.out = 100)) #1m to max depth (rounded down)
 ) %>%
  group_by(date) %>%
  mutate(value = estimate_val_by_date(date[1], depth)) #calculating values
  #is length out fitting?
#define function to estimate value, at a given depth, for any day
estimate_val_by_depth <- function(target_depth, target_date) {
  data_for_depth <- val_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$value, xout = target_date)$y }
#try estimation for the 4 days after sampling start at specific depth (1m)
estimate_val_by_depth(target_depth = 1, 
  target_date = seq( ymd(min(df$date)) , ymd((min(df$date)+4)) , by = 1) )
#creating tibble with calculated values for all combinations of date and depth
val_raster <- crossing(
  tibble(date = seq(ymd(min(df$date)), ymd(max(df$date)), by = 1)),
  tibble(depth = unique(val_interp_depth$depth))
 ) %>%
  group_by(depth) %>%
  mutate(value = estimate_val_by_depth(depth[1], date))
#plotting tibble with equally spaced depth and date
plot1 <- ggplot(val_raster, aes(date, depth, fill = value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = (max(df$value) - ((max(df$value)-min(df$value))/2)), 
    high = scales::muted("red"), low = scales::muted("blue")) +
  coord_cartesian(expand = FALSE)+
  labs(y="depth in m", x="", title="title" )
windows()
plot1




###AVERAGE: combining multiple years in one plot

#setting dataset for interpolation (location and value)
df_all <- subset(BolmProfile, location=="BOL_BS-W")
df_all$depth <- c(df_avg$D_Water)
df_all$value <- c(df_avg$FDOM)
#removing the year of the date
df_avg <- df_all %>%
  mutate(day_in_year = as.numeric(date - floor_date(date, unit = "years"), unit = "days"),
    date_label = ymd("2000-01-01") + day_in_year )
#using 2D smoother loess (values between years smooth?)
smooth_fit <- loess(value ~ day_in_year + depth, data = df_avg, span = 0.2)
  #check if smooth is a good average of the years graphically
  #compare fit to plotted measurements
#create tibble and estimate values for all day and depth combinations with predict
val_raster_smooth <- crossing(
  tibble(date = seq(ymd(min(df_avg$date)), ymd(max(df_avg$date)), by = 1)),
  tibble(depth = seq(1, (floor(max(df_avg$depth))), length.out = 100))
  ) %>%
  mutate(  day_in_year = as.numeric(date - floor_date(date, unit = "years"), unit = "days"),
    value = predict(smooth_fit, newdata = tibble(day_in_year = day_in_year, depth = depth))  )
#plot tibble
firstday <- min(df_avg$day_in_year)  #saved as number from df without na
lastday <- max(df_avg$day_in_year)
ggplot(val_raster_smooth, aes(as.Date(day_in_year), depth, fill = value) ) +
  geom_raster() +
  scale_y_reverse() +
  scale_x_date(date_labels = ("%b"), breaks = "1 month")+
  scale_fill_gradient2(
    midpoint = (max(df_avg$value) - ((max(df_avg$value)-min(df_avg$value))/2)), 
    high = scales::muted("red"), low = scales::muted("blue")) +
  coord_cartesian(expand = FALSE, xlim = c(as.Date(firstday), as.Date(lastday)) )+
  labs(x="", y="depth in m", title = "average depth profile of temperature during the year")
#legend name change
#check if smooth is good
#plot other years and combine yearly plots
#try other values



### Contour plot for each year at one location



#for multiple years after each other: use same midpoint, only one legend, 
#adding labels
#multiple years next to each other?
#only necessary steps
#set colors, legends and labs according to value/location (if?)
#title setting in the beginning
#legend title?

#first creating functions
#then setting dataset for the different years, calculating nd creating plot
#with a loop to repeat for each year?

estimate_val_by_date <- function(target_date, target_depth) {
  data_for_date <- df %>%        
    filter(date == target_date) %>%
    arrange(depth)
  approx(data_for_date$depth, data_for_date$value, xout = target_depth, ties = min)$y }

estimate_val_by_depth <- function(target_depth, target_date) {
  data_for_depth <- val_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(data_for_depth$date, data_for_depth$value, xout = target_date)$y }

df <- subset(BolmProfile, location=="BOL_BS-E" & year=="2025") #set location (and Year? )
df$depth <- c(df$D_Water)
df$value <- c(df$pH)          #set value
Titel <- "pH throughout the year 2025 at Bolmen Basin E"         #set title

#if (mean(df$value) == mean(df$TW))    {color1 <- "red4"; color2 <- "white"; color3 <- "blue4"}
#if statements to help with title, legend, palette?

val_interp_depth <- crossing(
  tibble(date = unique(df$date)), #sampling days
  tibble(depth = seq(1, (floor(max(df$depth))), length.out = 100)) #1m to max depth (rounded down)
) %>%
  group_by(date) %>%
  mutate(value = estimate_val_by_date(date[1], depth)) #calculating values

val_raster <- crossing(
  tibble(date = seq(ymd(min(df$date)), ymd(max(df$date)), by = 1)),
  tibble(depth = unique(val_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(value = estimate_val_by_depth(depth[1], date))

#plotting tibble with equally spaced depth and date
plot1 <- ggplot(val_raster, aes(date, depth, fill = value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_scico(palette = "batlow") + #binning? limits=c()
  coord_cartesian(expand = FALSE)+
  labs(y="depth in m", x="", title=Titel )
windows()
plot1

#next steps:
#farbgradient verwenden
#alle jahre einzeln bzw. nacheinander plotten



############ Discharge measurements ############ 


## open csv ##
skipLIL <- which((readLines(con="SITES_WB-SL-Q_BOL_IN-LIL_20220923-20250127_L2_irregular.csv"))=="####")
LIL <- read.csv("SITES_WB-SL-Q_BOL_IN-LIL_20220923-20250127_L2_irregular.csv", skip = skipLIL, sep=",", header = T, dec=".", stringsAsFactors = T)
LIL <- mutate(LIL, location = "Lillån")

skipONN <- which((readLines(con="SITES_WB-SL-Q_BOL_IN-ONN_20220926-20241030_L2_irregular.csv"))=="####")
ONN <- read.csv("SITES_WB-SL-Q_BOL_IN-ONN_20220926-20241030_L2_irregular.csv", skip = skipONN, sep=",", header = T, dec=".", stringsAsFactors = T)
ONN <- mutate(ONN, location = "Önnekvarn")

skipSTO <- which((readLines(con="SITES_WB-SL-Q_BOL_IN-STO_20220923-20250127_L2_irregular.csv"))=="####")
STO <- read.csv("SITES_WB-SL-Q_BOL_IN-STO_20220923-20250127_L2_irregular.csv", skip = skipSTO, sep=",", header = T, dec=".", stringsAsFactors = T)
STO <- mutate(STO, location = "Storån")

## combine data frames ##
Inlet <- rbind(LIL, ONN, STO)

Inlet <- mutate(Inlet, date=substring(Inlet$TIMESTAMP, 0, 10) )
Inlet <- mutate(Inlet, year=substring(Inlet$TIMESTAMP, 0, 4) )
Inlet <- mutate(Inlet, season=substring(Inlet$TIMESTAMP, 6, 10) )

Inlet$year <- as.factor(Inlet$year)
Inlet$location <- as.factor(Inlet$location)
Inlet$date <- as.Date( Inlet$date, format="%Y-%m-%d")
Inlet$season <- as.Date( Inlet$season, format="%m-%d")

str(Inlet)


## plotting ##
windows()   #inlets seperately
ggplot(Inlet, aes(date, Q, color=location))+
  geom_line()+
  scale_x_date(date_labels = ("%b %y"))+
  facet_grid(location ~.)

   #seperated by year
ggplot(Inlet, aes(season, Q, color=location))+
  geom_line()+
  scale_x_date(date_labels = ("%b"))+
  facet_grid(year ~.)

   #only one location
ggplot((subset(Inlet, location=="Storån")), aes(date, Q, color=location))+
  geom_line()+
  scale_x_date(date_labels = ("%b %y"))

   #only one year
ggplot((subset(Inlet, year=="2023")), aes(date, Q, color=location))+
  geom_line()+
  scale_x_date(date_labels = ("%b %y"))


ggplot(Inlet, aes(date, Q, color=location))+
  geom_line()+ 
  geom_point()+
  scale_x_date(date_labels = ("%b %y"), breaks = ("2 months"))+
  theme_minimal()+
  labs(title="Discharge measurements at Inlets of Lake Bolmen", y = "Discharge in m³/s", x="")





############ meteorological plots ############################

df <- arrow::read_parquet("metobs_bol.parquet", col_select = c(TimeStamp,StationName, ParameterName, Unit, Value))
df$StationName <- as.factor(df$StationName)
df$ParameterName <- as.factor(df$ParameterName)
df$Unit <- as.factor(df$Unit)
df$Date <- as.Date((substring(df$TimeStamp, 0, 10)), format="%Y-%m-%d")
df$Year <- as.factor(substring(df$TimeStamp, 0, 4))

lastYear <- floor_date(max(df$Date), unit="year")
df1y <- subset(df, Date < lastYear & Date >= (lastYear-years(1)) & StationName =="Ljungby A")
df30y <- subset(df, Date < lastYear & Date >= (lastYear-years(30)) & StationName =="Ljungby A")
summary(df1y)



### daily means 30 year ###

daily30 <- list(
  setNames(aggregate(Value~Date,(subset(df30y, ParameterName=="Vindhastighet")), 
                     mean, na.rm=TRUE, na.action=NULL), c("Date", "Windspeed_d")),
  setNames(aggregate(Value~Date,(subset(df30y, ParameterName=="Daggpunktstemperatur")), 
                     mean, na.rm=TRUE, na.action=NULL), c("Date", "Dewpoint_d")),
  setNames(aggregate(Value~Date,(subset(df30y, ParameterName=="Relativ Luftfuktighet")), 
                     mean, na.rm=TRUE, na.action=NULL), c("Date", "Humidity_d")),
  setNames(aggregate(Value~Date,(subset(df30y, ParameterName=="Nederbördsmängd")), 
                     sum, na.rm=TRUE, na.action=NULL), c("Date", "Precipitation_d")),
  setNames(aggregate(Value~Date,(subset(df30y, ParameterName=="Lufttemperatur")), 
                     mean, na.rm=TRUE, na.action=NULL), c("Date", "Temperatur_d"))
)
df30yDaily <- daily30 %>% reduce(full_join, by='Date')
df30yDaily$Year <-  as.factor(substring(df30yDaily$Date, 0, 4))



#PLOTTING as BARGRAPH
windows()
ggplot(df30yDaily, aes(x=Date, y=Precipitation_d))+
 geom_col(fill="blue", na.rm=TRUE)+
  theme_bw()
   #Problem, dass NA wie 0 dargestellt wird

df30yDaily <- df30yDaily %>%
  mutate(Decade = case_when(
    (Date > (max(df$Date)-years(10)) ) & (Date <= max(df$Date) ) ~ "decade3",
    (Date > (max(df$Date)-years(20)) ) & (Date <= (max(df$Date)-years(10)) ) ~ "decade2",
    (Date >= (max(df$Date)-years(30)) ) & (Date <= (max(df$Date)-years(20)) ) ~ "decade1"
  ))
df30yDaily$Decade <- as.factor(df30yDaily$Decade)

pa <- ggplot(subset(df30yDaily, Decade == "decade1"), aes(x=Date, y=Precipitation_d))+
  geom_col(fill="blue", na.rm=TRUE)+
  theme_bw()+
  coord_cartesian(ylim=c(0,70))+
  theme(axis.title.x=element_blank())+
  scale_x_date(date_labels = "%Y", breaks="1 year")

pb <- ggplot(subset(df30yDaily, Decade == "decade2"), aes(x=Date, y=Precipitation_d))+
  geom_col(fill="blue", na.rm=TRUE)+
  theme_bw()+
  coord_cartesian(ylim=c(0,70))+
  theme(axis.title.x=element_blank())+
  scale_x_date(date_labels = "%Y", breaks="1 year")

pc <- ggplot(subset(df30yDaily, Decade == "decade3"), aes(x=Date, y=Precipitation_d))+
  geom_col(fill="blue", na.rm=TRUE)+
  theme_bw()+
  coord_cartesian(ylim=c(0,70))+
  theme(axis.title.x=element_blank())+
  scale_x_date(date_labels = "%Y", breaks="1 year")
pa/pb/pc
ggsave("testplot.png", dpi = "retina")


#Histogram of daily precipitation, distribution of extreme events
#without 0 in Precipitation
#subset for 10 years each
#sum of days with rain
windows()
ggplot(subset(df30yDaily, Precipitation_d>0 & Date >= (max(Date)-years(10)) ), 
       aes(x=Precipitation_d )  )+
  geom_histogram(binwidth=1, fill="steelblue1", center=0.5)+
  theme_bw()+
  labs(y="day count", x="Amount of Precipitation in mm", title="Distribution of daily rain?!")+
  annotate("label", x=Inf,y=Inf, label=paste("some text", max(df30yDaily$Precipitation_d)), vjust=3, hjust=2)+
  stat_count(binwidth = 1, 
             geom = 'text', 
             color = 'white', 
             aes(label = after_stat(count)),
             position = position_stack(vjust = 0.5))

after_stat(count(df30yDaily$Precipitation_d))

test <- subset(df30yDaily, Precipitation_d>0 & Date >= (max(Date)-years(10)) )
?annotate
stat_bin(geom = "text", aes(label = ..count..))
#Histogram for daily mean Temperature, distribution of extreme events
#subset for 10 years each


### average Klimadiagramm 30 years ###

df30yP <- subset(df30y, ParameterName=="Nederbördsmängd") #Precipitation, avg of monthly sum of 30 years
df30yP$Month <- floor_date(df30yP$Date, "month")
df30yPSum <- summarize(df30yP, sum = sum(Value), .by=Month) #monthly sum
df30yPT1 <- aggregate(list(Precipitation_m = df30yPSum$sum), list(Month = month(df30yPSum$Month, label=TRUE)), mean)

df30yT <- subset(df30y, ParameterName=="Lufttemperatur") #Temperature, monthly avg of 30 years
df30yPT2 <- aggregate(list(Temperatur_m = df30yT$Value), list(Month = month(df30yT$Date, label=TRUE)), mean) 

df30yPT <- merge(df30yPT1, df30yPT2, by="Month")
df30yPT$Date <- paste("01", df30yPT$Month, sep="-")
df30yPT$Date <- as.Date(df30yPT$Date, format="%d-%b") 
str(df30yPT)


p30yPT <- ggplot(df30yPT, aes(x= Date)) +
  geom_bar(aes(y= Precipitation_m/2, fill = "Precipitation"), stat="identity")  +
  geom_line(aes(y = Temperatur_m, color = "Temperature"), linewidth=1.2) +
  geom_point(aes(y = Temperatur_m), color="tomato3") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(
    expression("Average Monthly Temperature  (°C)"), 
    sec.axis = sec_axis(~ . * 2, name = "Monthly Precipitation (mm)", breaks=c(0,20, 40, 60, 80, 100)) ) +
  scale_colour_manual("", values = c("Temperature" = "tomato3")) +
  scale_fill_manual("", values = "steelblue1") +
  theme_bw()+
  labs(title="Average Climate at Station Lungby A")+
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=0, hjust=0.5),
        legend.position = "bottom",
        axis.title.x = element_blank()) 

windows()
p30yPT
 #adding values from last year to compare




############ hydrological plots #############################





