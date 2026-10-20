#analysis functions



##### set work space #####

  #set wd with here
  #load packages


###### climate_diagram() ###### 


dfraw <- arrow::read_parquet("metobs_bol.parquet", col_select = c(TimeStamp,StationName, ParameterName, Unit, Value))
dfraw$StationName <- as.factor(dfraw$StationName)
dfraw$ParameterName <- as.factor(dfraw$ParameterName)
dfraw$Unit <- as.factor(dfraw$Unit)
dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")

lastYear <- floor_date(max(dfraw$Date), unit="year")
df30y <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & StationName =="Ljungby A") #!!!


daily <- list(
  setNames(aggregate(Value~Date,(subset(df30y, ParameterName=="Nederbördsmängd")), 
                     sum, na.rm=TRUE, na.action=NULL), c("Date", "Precipitation_d")),
  setNames(aggregate(Value~Date,(subset(df30y, ParameterName=="Lufttemperatur")), 
                     mean, na.rm=TRUE, na.action=NULL), c("Date", "Temperatur_d"))  )
df <- daily %>% reduce(full_join, by='Date')



###### precipitation_distribution() ###### 


#parameters:    a = number of years for each plot (num)
#               filename = name of the raw parquet file (chr)
#               parameter = name of Precipitation column in parquet, e.g. "Nederbördsmängd" (chr)

#raw data only from one station   !!!


Precipitation_distribution <- function(a, filename, parameter)
{
  dfraw <- arrow::read_parquet(filename, col_select = c(TimeStamp,StationName, ParameterName, Unit, Value))
  dfraw$ParameterName <- as.factor(dfraw$ParameterName)
  dfraw$Unit <- as.factor(dfraw$Unit)
  dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")
  
  lastYear <- floor_date(max(dfraw$Date), unit="year")
  df30y <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & StationName =="Ljungby A") #!!!
  StationName <- str_c(df30y[1, "StationName"])
  
  DF <- setNames(aggregate(Value~Date,(subset(df30y, ParameterName==parameter)), 
                           sum, na.rm=TRUE, na.action=NULL), c("Date", "Precipitation_d"))
  
  xmax <- ceiling(max(DF$Precipitation_d, na.rm=TRUE))
  t1 <- 0 #start at year 0
  
  
  while(t1 < 30) {
    df <- subset(DF, Date >= (min(Date)+years(t1))   &   Date < (min(Date)+years(t1+a))  )
    #DF is dataframe for 30 years and df is subset for each window length
    
    
    raindays <- nrow(df[df$Precipitation_d >0,])
    rainsum <- sum(df$Precipitation_d, na.rm=TRUE)
    rainratio <- round(x=((raindays / nrow(df))*100), digits=0)
    rainmax <- max(df$Precipitation_d, na.rm=TRUE)
    
    
    
    ggplot(subset(df, Precipitation_d>0), aes(x=Precipitation_d )  )+
      geom_histogram(binwidth=1, fill="steelblue1", center=0.5)+
      theme_bw()+
      labs(y="day count", x="Amount of Precipitation in mm", 
           title="Distribution of Daily Precipitation", 
           subtitle=paste("between", format(min(df$Date), format="%d.%m.%Y"), 
                          "and", format(max(df$Date), format="%d.%m.%Y"), 
                          "\nat weather station", StationName) )+
      annotate("text_npc", npcx = 0.9, npcy = 0.9, 
               label = paste("total amount of precipitation: ", round(rainsum), 
                             "\ndays with precipitation: ", raindays, 
                             "\nratio of precipitation days:  ",rainratio, "%"))+
      annotate("segment", x = rainmax, xend= rainmax, y = 0, yend = 30, 
               linewidth=0.5, lty=2)+
      annotate("text", x=rainmax , y=30, hjust=1, vjust=-0.5,
               label = paste("max", rainmax, "mm"))+
      theme(plot.title=element_text(face="bold"), aspect.ratio=1)+
      coord_cartesian(xlim=c(0, xmax))+
      scale_y_continuous(trans = scales::pseudo_log_trans(base = 10))
    
    ggsave(filename = str_c("histogram_P_t", t1,"_", format(Sys.time(), "%Y-%m-%d"), ".png"), dpi = "retina")
    
    
    t1 <- t1 + a       #change running index
  }    #end of loop
  
  return(t1)
  
}      #end of function

 #trying the function
a = 12
filename="metobs_bol.parquet"
parameter="Nederbördsmängd"

Precipitation_distribution(12, "metobs_bol.parquet", "Nederbördsmängd")


###### temperature_distribution() ###### 

#parameters:    a = number of years for each plot (num)
#               filename = name of the raw parquet file (chr)
#               parameter = name of Temperature column in parquet, e.g. "Lufttemperatur" (chr)

#raw data only from one station   !!!

Temperature_distribution <- function(a, filename, parameter) 
{
  dfraw <- arrow::read_parquet(filename, col_select = c(TimeStamp,StationName, ParameterName, Unit, Value))
  dfraw$ParameterName <- as.factor(dfraw$ParameterName)
  dfraw$Unit <- as.factor(dfraw$Unit)
  dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")
  
  lastYear <- floor_date(max(dfraw$Date), unit="year")
  df30y <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & StationName =="Ljungby A") #!!!
  StationName <- str_c(df30y[1, "StationName"])
  
  DF <- setNames(aggregate(Value~Date,(subset(df30y, ParameterName==parameter)), 
                           mean, na.rm=TRUE, na.action=NULL), c("Date", "Temperatur_d"))
  t1 <- 0 #start at year 0
  
  
  while(t1 < 30) {
    dfT <- subset(DF, Date >= (min(Date)+years(t1))   &   Date < (min(Date)+years(t1+a))  )
    
    Tmean <- round(mean(dfT$Temperatur_d, na.rm=TRUE), digits=1)
    Tmax <- round(max(dfT$Temperatur_d, na.rm=TRUE), digits=1)
    Tmin <- round(min(dfT$Temperatur_d, na.rm=TRUE), digits=1)
    
    ggplot(dfT, aes(x=Temperatur_d )  )+
      geom_histogram(binwidth=1, fill="grey", center=0)+
      theme_bw()+
      labs(y="day count", x="daily air temperature in °C", 
           title="Distribution of Daily Temperature", 
           subtitle=paste("between", format(min(dfT$Date), format="%d.%m.%Y"), 
                          "and", format(max(dfT$Date), format="%d.%m.%Y"),
                          "\nat weather station", StationName) )+
      geom_vline(xintercept=Tmean, lty=2)+
      annotate("text", x=Tmean, y=230, label=paste("mean:", Tmean, "°C"), 
               hjust=1.1, vjust=-1)+
      geom_vline(xintercept=Tmax, lty=2, color="red")+
      annotate("text", x=Tmax, y=230, label=paste("max:", Tmax, "°C"), color="red", 
               hjust=1.1, vjust=-1)+
      geom_vline(xintercept=Tmin, lty=2, color="blue")+
      annotate("text", x=Tmin, y=230, label=paste("min:", Tmin, "°C"),color="blue", 
               hjust=-0.1, vjust=-1)+
      theme(plot.title=element_text(face="bold"), aspect.ratio=1)+
      coord_cartesian(xlim=c(-23, 30), ylim=c(0, 250))
    
    ggsave(filename = str_c("histogram_T_t", t1,"_", format(Sys.time(), "%Y-%m-%d"), ".png"), dpi = "retina")
    
    t1 <- t1 + a       #change running index
    
  }
  
  return(t1)
  
}

a = 10
filename="metobs_bol.parquet"
parameter="Lufttemperatur"

Temperature_distribution(10, "metobs_bol.parquet", "Lufttemperatur")


###### depth_measurements() ###### 


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
#BolmProfile <- mutate(BolmProfile, year=substring(BolmProfile$TIMESTAMP, 0, 4) )
#BolmProfile$year <- as.factor(BolmProfile$year)
BolmProfile$location <- as.factor(BolmProfile$location)
BolmProfile$date <- as.Date( BolmProfile$date, format="%Y-%m-%d")
lastYear <- floor_date(max(dfraw$Date), unit="year")


#DF big dataframe with all locations and years
# build DF from list with individual names

List <- list(
  "SITES_SONDE-PROF_BOL_BS-E_20220413-20251103_L2_irregular.csv",
  "SITES_SONDE-PROF_BOL_BS-NE_20220510-20251103_L2_irregular.csv",
  "SITES_SONDE-PROF_BOL_BS-NW_20220510-20251103_L2_irregular.csv",
  "SITES_SONDE-PROF_BOL_BS-S_20220712-20251103_L2_irregular.csv",
  "SITES_SONDE-PROF_BOL_BS-W_20220413-20251103_L2_irregular.csv"
)
my_list <- list()


for (element in List) {
  skip <- which((readLines(con=element))=="####")
  df <- read.csv(element, skip = skipBolmW, sep=",", header = T, dec=".", stringsAsFactors = T)
  Location <- substring(element, 17, 26)
  df <- mutate(df, location = Location)
  
  my_list <- append(my_list, list(df))
  assign(paste0("df_", Location), df)
}

DF <- bind_rows(my_list, .id="ID")

#function with filename, parameter

#set data
df <- subset(BolmProfile, location=="BOL_BS-E" & year=="2025") #set location (and Year? )
df$depth <- c(df$D_Water)
df$value <- c(df$pH)          #set value



#create functions for interpolation

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

# LOOP: i for location factor, repeat for all locations

#calculate values
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



#plotting
windows()
ggplot(val_raster, aes(date, depth, fill = value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_scico(palette = "batlow") +        #binning? limits=c()
  coord_cartesian(expand = FALSE)+
  labs(y="depth in m", x="", title= "Depth Profile of pH at Location BOL_BS-E in 2025")

ggsave(filename = "WQ_BOL_profile_pH_BS-E.eps", dpi = "retina", path ="C:/Users/luzie/Lund University/Clemens Klante - Intern - Luzie Rieth/bol-vom-analysis-poc/fig" )



str_c("histogram_T_t", t1,"_", format(Sys.time(), "%Y-%m-%d")
paste("Depth Profile of", parameter, "at Location", location, year)



###### discharge() ###### 
 








