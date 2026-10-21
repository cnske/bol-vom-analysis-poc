#analysis functions
library(here)
here::i_am("analysis_functions.R")
setwd(here())

pathdata <- paste0(here(), "/data/")
pathfig <- paste0(here(), "/fig")

testdf <- read.csv(paste0(pathdata, "SITES_SONDE-PROF_BOL_BS-E_20220413-20251103_L2_irregular.csv"))

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
?read_parquet

Precipitation_distribution <- function(a, filename, parameter)
{
  dfraw <- arrow::read_parquet(filename, col_select = c(TimeStamp,StationName, ParameterName, Unit, Value))
  dfraw$ParameterName <- as.factor(dfraw$ParameterName)
  dfraw$Unit <- as.factor(dfraw$Unit)
  dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")
  
  lastYear <- floor_date(max(dfraw$Date), unit="year")
  df30y <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & StationName =="Ljungby A") #!!!
  StationName <- str_c(df30y[1, "StationName"])
  
  if (StationName == "Ljungby A") {
    SiteName <- "bol"
  } else {
    if (StationName == "SMHI station?") {
      SiteName <- "vom"
    } else {
      SiteName <- "rin"
    }
  }
  
  
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
    
    ggsave(filename = str_c("meteo_hist_P_", SiteName, "_a", a, "_t", t1, ".svg"), dpi = "retina")
    
    
    t1 <- t1 + a       #change running index
  }    #end of loop
  
  return(t1)
  
}      #end of function

#parameters:    a = number of years for each plot (num)
#               filename = name of the raw parquet file (chr)
#               parameter = name of Precipitation column in parquet, e.g. "Nederbördsmängd" (chr)

#raw data only from one station   !!!


Precipitation_distribution(15, "metobs_bol.parquet", "Nederbördsmängd")


###### temperature_distribution() ###### 


Temperature_distribution <- function(a, filename, parameter) 
{
  dfraw <- arrow::read_parquet(filename, col_select = c(TimeStamp,StationName, ParameterName, Unit, Value))
  dfraw$ParameterName <- as.factor(dfraw$ParameterName)
  dfraw$Unit <- as.factor(dfraw$Unit)
  dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")
  
  lastYear <- floor_date(max(dfraw$Date), unit="year")
  df30y <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & StationName =="Ljungby A") #!!!
  StationName <- str_c(df30y[1, "StationName"])
  
  if (StationName == "Ljungby A") {
    SiteName <- "bol"
  } else {
    if (StationName == "SMHI station?") {
      SiteName <- "vom"
    } else {
      SiteName <- "rin"
    }
  }
  
  DF <- setNames(aggregate(Value~Date,(subset(df30y, ParameterName==parameter)), 
                           mean, na.rm=TRUE, na.action=NULL), c("Date", "Temperatur_d"))
  t1 <- 0 
  
  
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
    
    ggsave(filename = str_c("meteo_hist_T_", SiteName, "_a", a, "_t", t1, ".svg"), dpi = "retina")
    
    t1 <- t1 + a  
    
  }
  
  return(t1)
  
}

#parameters:    a = number of years for each plot (num)
#               filename = name of the raw parquet file (chr)
#               parameter = name of Temperature column in parquet, e.g. "Lufttemperatur" (chr)

#raw data only from one station   !!!

Temperature_distribution(10, "metobs_bol.parquet", "Lufttemperatur")


###### depth_measurements() ###### 


depth_profile <- function(List, parameter)
{
  df_list <- list()
  location_list <- list()
  
  for (element in List) {
    skip <- which((readLines(con= paste0(pathdata, element)))=="####")
    df <- read.csv(paste0(pathdata, element), skip = skip, sep=",", header = T, dec=".", stringsAsFactors = T)
    LocationName <- str_sub(element, start = 18, end=-36)
    location_list <- append(location_list, list(LocationName))
    df <- mutate(df, location = LocationName)
    df_list <- append(df_list, list(df))
    assign(paste("df_", LocationName, sep=""), df)
  }
  
  DF <- bind_rows(df_list)
  DF <- mutate(DF, date=substring(DF$TIMESTAMP, 0, 10) )
  DF$location <- as.factor(DF$location)
  DF$date <- as.Date( DF$date, format="%Y-%m-%d")
  lastYear <- floor_date(max(DF$date), unit="year")
  
  
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
  
  #for loop with parameter from list
  for (element in location_list)
  {
    df <- subset(DF, location==element & date >= (lastYear-years(1))  & date < lastYear)
    df$depth <- c(df$D_Water)
    df$value <- c(df$parameter)
    interp_depth <- max( df$depth[df$depth!=max(df$depth)] )
    
    val_interp_depth <- crossing(
      tibble(date = unique(df$date)), 
      tibble(depth = seq(1, interp_depth, length.out = 100)) 
    ) %>%
      group_by(date) %>%
      mutate(value = estimate_val_by_date(date[1], depth)) 
    
    val_raster <- crossing(
      tibble(date = seq(ymd(min(df$date)), ymd(max(df$date)), by = 1)),
      tibble(depth = unique(val_interp_depth$depth))
    ) %>%
      group_by(depth) %>%
      mutate(value = estimate_val_by_depth(depth[1], date))
    
    
    ggplot(val_raster, aes(date, depth, fill = value)) +
      geom_raster() +
      scale_y_reverse() +
      scale_x_date(date_breaks="1 month", date_labels="%b")+
      scale_fill_scico(palette = "batlow") +        #binning? limits=c()
      coord_cartesian(expand = FALSE)+
      labs(y = "depth in m", 
           title = paste0("Depth Profile of ", parameter, " at ", element, " in ", format.Date(lastYear-1, format="%Y")    ) )+
      theme(plot.title=element_text(face="bold"), aspect.ratio=1)
    
    ggsave(filename = paste0("WQ_profile_", element, "_", parameter, ".svg" ), 
           path=pathfig, dpi = "retina" )
    

  }
  #end of loop with parameter from list
  
  return(location_list)
}

#parameters:   List = a list with all the csv files 
#              parameter = a column name of csv file

#              maybe a loop so it repeats for every parameter in list?

List <- list(
  "SITES_SONDE-PROF_BOL_BS-E_20220413-20251103_L2_irregular.csv" ,
  "SITES_SONDE-PROF_BOL_BS-NE_20220510-20251103_L2_irregular.csv" ,
  "SITES_SONDE-PROF_BOL_BS-NW_20220510-20251103_L2_irregular.csv" ,
  "SITES_SONDE-PROF_BOL_BS-S_20220712-20251103_L2_irregular.csv" ,
  "SITES_SONDE-PROF_BOL_BS-W_20220413-20251103_L2_irregular.csv" )

parameter = "pH"

depth_profile(List, "FDOM")


###### discharge() ###### 
 


?ggsave





