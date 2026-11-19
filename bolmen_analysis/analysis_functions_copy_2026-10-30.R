#analysis functions


packages <- c("here", "tidyverse", "scico", "arrow", "ggpmisc" )

#library(base)
#"nanoparquet", "multcomp", "EnvStats", "raster", "doBy", "readxl", "ggtext", "readtext", "svglite",

install.packages(packages)
lapply(packages, library, character.only=TRUE)


# check if packages are used
#used.functions <- NCmisc::list.functions.in.file(filename = "analysis_functions.R", alphabetic = FALSE) |> print()
#used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE) |> gsub(pattern = "package:", replacement = "") |> print()
#unused.packages <- packages[!(packages %in% used.packages)] |> print()




here::i_am("analysis_functions.R")
setwd(here())

pathdata <- paste0(here(), "/data/")
pathbol <- paste0(here(), "/fig/svg/bol")
pathvom <- paste0(here(), "/fig/svg/vom")
pathrin <- paste0(here(), "/fig/svg/rin")
pathfig <- paste0(here(), "/fig/svg/")
pathcap <- paste0(here(), "/fig/captions/")



#     StationID

# "Ljungby A" - 63510 - BOL
# "Hörby A" - 53530 - RIN (and VOM)

#     ParameterID

# "Lufttemperatur"        -   1
# "Vindhastighet"         -   4
# "Relativ Luftfuktighet" -   6
# "Nederbördsmängd"       -   7
# "Daggpunktstemperatur"  -   39


# Site:     BOL, VOM, RIN
# location: BOL_BS-W, VOM_02 ...
# DF:       complete dataframe used for function
# df:       dataframe used for plotting, a subset of DF
# lastYear: end of last full Year
# Year:     last full year

###### climate_diagram() ###### 


climate_diagram <- function(filename, station_list)
{
  dfraw <- arrow::read_parquet(paste0(pathdata, filename), 
                               col_select = c(TimeStamp, StationID, StationName, ParameterID, 
                                              ParameterName, Unit, Value))
  dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")
  lastYear <- floor_date(max(dfraw$Date), unit="year")
  startYear <- as.Date(lastYear-years(1))
  
  
  DF <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30))) 
  DF$ParameterID <- as.factor(DF$ParameterID)
  DF$StationID <- as.factor(DF$StationID)
  DF$MonthYear <- as.factor(substring(DF$TimeStamp, 0, 7))

  
 for (Station in station_list)
 { 
   df <- subset(DF, StationID == Station)
   
   SiteName <- case_when(
     Station == "63510"   ~ "BOL",    
     Station == "53530"   ~ "RIN",
     TRUE ~ "XXX" )
   
   StationName <- case_when(
     Station == "63510"   ~ "Ljungby A",    
     Station == "53530"   ~ "Hörby A",
     TRUE ~ "XXX" )
   
   Path <- case_when(
     SiteName == "BOL"   ~ pathbol,    
     SiteName == "RIN"   ~ pathrin,
     SiteName == "VOM"   ~ pathvom,
     TRUE ~ pathfig )
   
   monthlyP <- setNames(aggregate(Value ~ MonthYear,
                        data = subset(df, ParameterID=="7"), FUN=sum, na.rm=TRUE), 
                        c("MonthYear", "Precipitation_m"))
   
   monthlyT <- setNames(aggregate(Value ~ MonthYear,
                        data = subset(df, ParameterID=="1"), FUN=mean, na.rm=TRUE), 
                        c("MonthYear", "Temperature_m"))
   
   avgP <- setNames(aggregate(Precipitation_m ~ str_sub(monthlyP$MonthYear, start = -2) ,
                    data = monthlyP, FUN=mean, na.rm=TRUE),
                    c("Month", "Precipitation"))
   
   avgT <- setNames(aggregate(Temperature_m ~ str_sub(monthlyT$MonthYear, start = -2) ,
                    data = monthlyT, FUN=mean, na.rm=TRUE),
                    c("Month", "Temperature"))
   
   climatelist <- list(
     setNames(data.frame(seq(startYear, by="month", length.out=12)), c("date")),
     setNames(data.frame(avgT$Temperature), c("avgTemperature")),
     setNames(data.frame(avgP$Precipitation), c("avgPrecipitation")),
     setNames(data.frame(tail(monthlyT$Temperature_m, n = 12)), c("Temperature")),
     setNames(data.frame(tail(monthlyP$Precipitation_m, n = 12)), c("Precipitation"))
   )
   dfclimate <- list_cbind(climatelist, name_repair = "universal")
   
   avgJT <- round(mean(dfclimate$avgTemperature), digits=1)
   avgJP <- round(sum(dfclimate$avgPrecipitation))
   JT <- round(mean(dfclimate$Temperature), digits=1)
   JP <- round(sum(dfclimate$Precipitation))
   Year <- format(startYear, format="%Y")
   
   ggplot(dfclimate, aes(x=date))+
     geom_bar(aes(y= Precipitation/2, fill="Precipitation"), stat="identity")  +
     geom_bar(aes(y= avgPrecipitation/2, fill="average Precipitation", ), color="grey15", linetype=5, stat="identity")+
     geom_line(aes(y = avgTemperature, color="average Temperature"), linetype = 1, linewidth=1) +
     geom_line(aes(y = Temperature, color="Temperature"), linewidth=1) +
     geom_point(aes(y = Temperature, color="Temperature")) +
     scale_x_date(date_labels = "%b", date_breaks = "1 month") +
     scale_y_continuous(sec.axis = sec_axis(~ . * 2, name = "Monthly Precipitation (mm)", 
                        breaks=c(0,20, 40, 60, 80, 100)),
                        expression("Average Monthly Temperature  (°C)") ) +
     scale_color_manual("", values=c('Temperature'='tomato3', 'average Temperature'='grey30'))+
     scale_fill_manual("", values=c('Precipitation'="dodgerblue2", "average Precipitation"="transparent")) +
     labs(title=paste(Station, df$StationName[1], "in", Year))+
     theme_bw()+
     theme(text=element_text(size=12),
           axis.text.x = element_text(angle=0, hjust=0.5),
           legend.position = "bottom",
           legend.direction="horizontal",
           axis.title.x = element_blank(),
           aspect.ratio=1/1.5)
   
   ggsave(filename = str_c("MET_climate_", SiteName, "_", Station, ".svg"), 
          path=Path, dpi = "retina")
   
   
   caption <- paste("Climate diagram at station", Station, StationName, "for the year", Year, 
                    "(mean temperature:", JT, "°C and yearly precipitation:", JP, "mm)",
                    "compared to the last 30 years (average yearly temperature:", avgJT, 
                    "°C and average yearly precipitation:", avgJP, "mm).")
   writeLines(caption, paste0(pathcap, "MET_climate_", SiteName, "_", Station, "_caption.md" ))
  
 }
  return(station_list) 
  
}   


# filename      name of raw parquetfile
# station_list  list of the StationIDs

filename <- "metobs.parquet"
station_list <- c("63510", "53530")

climate_diagram(filename, station_list)






###### precipitation_distribution() ###### 


Precipitation_distribution <- function(a, filename, station_list)
{
  dfraw <- arrow::read_parquet(paste0(pathdata, filename), 
                               col_select = c(TimeStamp, StationID, StationName, ParameterID, 
                                              ParameterName, Unit, Value))
  dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")
  lastYear <- floor_date(max(dfraw$Date), unit="year")
  
  
  DF <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & ParameterID == "7") 
  DF$StationID <- as.factor(DF$StationID)
  
  for (Station in station_list)
  {  
    DFstation <- subset(DF, StationID == Station)
    
    
    SiteName <- case_when(
      Station == "63510"   ~ "BOL",    
      Station == "53530"   ~ "RIN",
      TRUE ~ "XXX" )
    
    StationName <- case_when(
      Station == "63510"   ~ "Ljungby A",    
      Station == "53530"   ~ "Hörby A",
      TRUE ~ "XXX" )
    
    Path <- case_when(
      SiteName == "BOL"   ~ pathbol,    
      SiteName == "RIN"   ~ pathrin,
      SiteName == "VOM"   ~ pathvom,
      TRUE ~ pathfig )
    
    
    DFdaily <- setNames(aggregate(Value~Date, DFstation, 
                                  sum, na.rm=TRUE, na.action=NULL), c("Date", "Precipitation_d"))
    xmax <- ceiling(max(DFdaily$Precipitation_d, na.rm=TRUE))
    
    t1 <- 0
    
    
    while(t1 < 30) {
      df <- subset(DFdaily, Date >= (min(Date)+years(t1))   &   Date < (min(Date)+years(t1+a))  )

      
      raindays <- nrow(df[df$Precipitation_d >0,])
      rainsum <- sum(df$Precipitation_d, na.rm=TRUE)
      rainratio <- round(x=((raindays / nrow(df))*100), digits=0)
      rainmax <- max(df$Precipitation_d, na.rm=TRUE)
      
      
      
      ggplot(subset(df, Precipitation_d>0), aes(x=Precipitation_d )  )+
        geom_histogram(binwidth=1, fill="steelblue1", center=0.5)+
        theme_bw()+
        labs(y="day count", x="Amount of Precipitation in mm",  
             subtitle=paste("between", format(min(df$Date), format="%d.%m.%Y"), 
                            "and", format(max(df$Date), format="%d.%m.%Y")))+
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
      
      ggsave(filename = str_c("MET_hist_P_", SiteName, "_a", a, "_t", t1, ".svg"), 
             path=Path, dpi = "retina")
      
      caption <- paste("Distribution of daily precipitation at station", Station, StationName, 
                       "between", format(min(df$Date), format="%d.%m.%Y"), 
                       "and", format(max(df$Date), format="%d.%m.%Y"))
      writeLines(caption, paste0(pathcap, "MET_hist_P_", SiteName, "_a", a, "_t", t1, "_caption.md" ))
      
      t1 <- t1 + a       
    }   
    
    
  }
  
  return(t1)
  
}      


# a             windowlength, for how many years
# filename      name of raw parquetfile from SHMI
# station_list  list of the StationIDs

a <- 10
filename <- "metobs.parquet"
station_list <- c("63510", "53530")

Precipitation_distribution(a, filename, station_list)




###### temperature_distribution() ###### 


Temperature_distribution <- function(a, filename, station_list) 
{
  dfraw <- arrow::read_parquet(paste0(pathdata, filename), 
                               col_select = c(TimeStamp, StationID, StationName, ParameterID, 
                                              ParameterName, Unit, Value))
  dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")
  lastYear <- floor_date(max(dfraw$Date), unit="year")
  
  DF <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & ParameterID == "1") 
  DF$StationID <- as.factor(DF$StationID)
  
  
  for (Station in station_list)
  {
    DFstation <- subset(DF, StationID == Station)
    
    SiteName <- case_when(
      Station == "63510"   ~ "BOL",    
      Station == "53530"   ~ "RIN",
      TRUE ~ "XXX" )
    
    StationName <- case_when(
      Station == "63510"   ~ "Ljungby A",    
      Station == "53530"   ~ "Hörby A",
      TRUE ~ "XXX" )
    
    Path <- case_when(
      SiteName == "BOL"   ~ pathbol,    
      SiteName == "RIN"   ~ pathrin,
      SiteName == "VOM"   ~ pathvom,
      TRUE ~ pathfig )
    
    DFdaily <- setNames(aggregate(Value~Date, DFstation, 
                                  mean, na.rm=TRUE, na.action=NULL), c("Date", "Temperature_d"))
    
    t1 <- 0 
    
    
    while(t1 < 30) 
    {
      df <- subset(DFdaily, Date >= (min(Date)+years(t1))   &   Date < (min(Date)+years(t1+a))  )
      
      Tmean <- round(mean(df$Temperature_d, na.rm=TRUE), digits=1)
      Tmax <- round(max(df$Temperature_d, na.rm=TRUE), digits=1)
      Tmin <- round(min(df$Temperature_d, na.rm=TRUE), digits=1)
      
      ggplot(df, aes(x=Temperature_d )  )+
        geom_histogram(binwidth=1, fill="grey", center=0)+
        theme_bw()+
        labs(y="day count", x="daily air temperature in °C", 
             subtitle=paste("between", format(min(df$Date), format="%d.%m.%Y"), 
                            "and", format(max(df$Date), format="%d.%m.%Y")) )+
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
      
      ggsave(filename = str_c("MET_hist_T_", SiteName, "_a", a, "_t", t1, ".svg"),
             path=Path, dpi = "retina")
      
      caption <- paste("Distribution of daily air temperature at station", Station, StationName, 
                       "between", format(min(df$Date), format="%d.%m.%Y"), 
                       "and", format(max(df$Date), format="%d.%m.%Y"))
      writeLines(caption, paste0(pathcap, "MET_hist_T_", SiteName, "_a", a, "_t", t1, "_caption.md" ))
      
      t1 <- t1 + a  
      
    } 
    
    
  } 
  
  
  return(t1)
  
}


# a             windowlength, for how many years
# filename      name of raw parquetfile from SHMI
# station_list  list of the StationIDs

a <- 10
filename <- "metobs.parquet"
station_list <- c("63510", "53530")

Temperature_distribution(a, filename, station_list)


###### depth_measurements() ###### 


depth_profile <- function(Site, parameter_list)
{
  df_list <- list()
  location_list <- list()
  
  file_list <- list.files(pattern=paste0("SITES_SONDE-PROF_", Site), path = pathdata)
  
  for (file in file_list) {
    skip <- which((readLines(con= paste0(pathdata, file)))=="####")
    df <- read.csv(paste0(pathdata, file), skip = skip, sep=",", header = T, dec=".", stringsAsFactors = T)
    LocationName <- str_sub(file, start = 18, end=-36)
    location_list <- append(location_list, list(LocationName))
    df <- mutate(df, location = LocationName)
    df_list <- append(df_list, list(df))
    assign(paste0("df_", LocationName), df)
  }  
  
  DF <- bind_rows(df_list)
  DF <- mutate(DF, date=substring(DF$TIMESTAMP, 0, 10) )
  DF$location <- as.factor(DF$location)
  DF$date <- as.Date( DF$date, format="%Y-%m-%d")
  lastYear <- floor_date(max(DF$date), unit="year")
  Year <- format((lastYear - years(1)), format="%Y")
  
  
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
  
  
  for (parameter in parameter_list)
  {
    #plot settings for each parameter
    
    Legendtitel <- case_when(
      parameter == "TW"     ~ "Water\ntemperature\n(°C)",
      parameter == "EC"     ~ "Electrical\nconductivity\n(µS cm-1)",
      parameter == "SCOND"  ~ "Specific\nConductivity\nat 25°C\n(µS cm-1)",
      parameter == "pH"     ~ "pH",      
      parameter == "ORP"    ~ "Oxidation\nReduction\nPotential\n(mV)",      
      parameter == "TURBF"  ~ "Turbidity\n(FNU)",      
      parameter == "CHLF"   ~ "Chlorophyll\nFluorescence\n(µg L-1)",  
      parameter == "O2SAT"  ~ "Oxygen\nsaturation\n(%)",  
      parameter == "O2"     ~ "Dissolved\nOxygen\n(mg L-1)",  
      parameter == "FDOM"   ~ "Fluorescent\ndissolved\norganic\nmatter\n(µg L-1)",  
      TRUE ~ "unknown parameter"
    )
    
    Palette <- case_when(
      parameter == "TW"     ~ "managua",
      parameter == "O2SAT"  ~ "nuuk", 
      parameter == "O2"     ~ "nuuk", 
      parameter == "FDOM"   ~ "bamako", 
      parameter == "CHLF"   ~ "bamako",   
      TRUE ~ "imola" 
    )
    
    ParameterName <- case_when(
      parameter == "TW"     ~ "Water temperature (°C)",
      parameter == "EC"     ~ "Electrical conductivity (µS cm-1)",
      parameter == "SCOND"  ~ "Specific Conductivity at 25°C (µS cm-1)",
      parameter == "pH"     ~ "pH",      
      parameter == "ORP"    ~ "Oxidation Reduction Potential (mV)",      
      parameter == "TURBF"  ~ "Turbidity (FNU)",      
      parameter == "CHLF"   ~ "Chlorophyll Fluorescence (µg L-1)",  
      parameter == "O2SAT"  ~ "Oxygen saturation (%)",  
      parameter == "O2"     ~ "Dissolved Oxygen (mg L-1)",  
      parameter == "FDOM"   ~ "Fluorescent dissolved organic matter (µg L-1)",  
      TRUE ~ "unknown parameter"
    )
    Limits <- c(floor(min(DF[, parameter])), ceiling(max(DF[, parameter])))
    
    
    for (element in location_list)
    {
      #subset: one location, last full year, one parameter
      df <- subset(DF, location==element & date >= (lastYear-years(1))  &  date < lastYear) 
      df$depth <- c(df$D_Water)
      df$value <-  c(df[, parameter])
      
      interp_depth <- max(df$depth[df$depth != max(df$depth)])
      #max depth on the second deepest sample day?
      #deepestday <- df$date[which.max(df$depth)]
      #interp_depth <- max( df$depth[df$date != deepestday] )  
      
      Path <- case_when(
        str_sub(df[1, "location"], end=3) == "BOL"   ~ pathbol,    
        str_sub(df[1, "location"], end=3) == "RIN"   ~ pathrin,
        str_sub(df[1, "location"], end=3) == "VOM"   ~ pathvom,
        TRUE ~ pathfig )
      
      
      val_interp_depth <- crossing(
        tibble(date = unique(df$date)), 
        tibble(depth = seq(1, interp_depth, by=0.1)) 
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
        scale_fill_scico(palette = Palette, direction=-1, na.value = "transparent", limits = Limits) + 
        coord_cartesian(expand = FALSE)+
        labs(y = "depth in m", fill = Legendtitel )+
        theme(axis.title.x=element_blank(), )
      
      ggsave(filename = paste0("WQ_profile_", element, "_", parameter, ".svg" ), 
             path=Path, height = 6, width = 7, dpi="retina" )
      
      
      caption <- paste("Depth profile of", ParameterName, "at location", Site, "in", Year)
      writeLines(caption, paste0(pathcap, "WQ_profile_", element, "_", parameter, "_caption.md" ))
      
    } 
    
  } 
  
  return(print(file_list))
  
}


# Site            BOL, VOM or RIN 
# parameter_list  list of the column that should be used



parameter_list <- c( "TW", "O2", "O2SAT", "pH")

depth_profile("BOL", parameter_list)

#depth_profile("VOM", parameter_list) #still doesnt work





###### depth_measurements_continuous() ######


#interpolates over every date and does not only show timeframes where it was measured
depth_profile_continuous <- function(a, Site, parameter_list)
{
  df_list <- list()
  location_list <- list()
  
  file_list <- list.files(pattern=paste0("SITES_SONDE-PROF_", Site), path = pathdata)
  
  for (file in file_list) {
    skip <- which((readLines(con= paste0(pathdata, file)))=="####")
    df <- read.csv(paste0(pathdata, file), skip = skip, sep=",", header = T, dec=".", stringsAsFactors = T)
    LocationName <- str_sub(file, start = 18, end=-36)
    location_list <- append(location_list, list(LocationName))
    df <- mutate(df, location = LocationName)
    df_list <- append(df_list, list(df))
    assign(paste0("df_", LocationName), df)
  }  
  
  DF <- bind_rows(df_list)
  DF <- mutate(DF, date=substring(DF$TIMESTAMP, 0, 10) )
  DF$location <- as.factor(DF$location)
  DF$date <- as.Date( DF$date, format="%Y-%m-%d")
  lastYear <- floor_date(max(DF$date), unit="year")
  DF$year <- factor(format(DF$date, "%Y"))  
  
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
  
  
  for (parameter in parameter_list)
  {
    #plot settings for each parameter
    
    Legendtitel <- case_when(
      parameter == "TW"     ~ "Water\ntemperature\n(°C)",
      parameter == "EC"     ~ "Electrical\nconductivity\n(µS cm-1)",
      parameter == "SCOND"  ~ "Specific\nConductivity\nat 25°C\n(µS cm-1)",
      parameter == "pH"     ~ "pH",      
      parameter == "ORP"    ~ "Oxidation\nReduction\nPotential\n(mV)",      
      parameter == "TURBF"  ~ "Turbidity\n(FNU)",      
      parameter == "CHLF"   ~ "Chlorophyll\nFluorescence\n(µg L-1)",  
      parameter == "O2SAT"  ~ "Oxygen\nsaturation\n(%)",  
      parameter == "O2"     ~ "Dissolved\nOxygen\n(mg L-1)",  
      parameter == "FDOM"   ~ "Fluorescent\ndissolved\norganic\nmatter\n(µg L-1)",  
      TRUE ~ "unknown parameter"
    )
    
    Palette <- case_when(
      parameter == "TW"     ~ "managua",
      parameter == "O2SAT"  ~ "nuuk", 
      parameter == "O2"     ~ "nuuk", 
      parameter == "FDOM"   ~ "bamako", 
      parameter == "CHLF"   ~ "bamako",   
      TRUE ~ "imola" 
    )
    
    ParameterName <- case_when(
      parameter == "TW"     ~ "Water temperature (°C)",
      parameter == "EC"     ~ "Electrical conductivity (µS cm-1)",
      parameter == "SCOND"  ~ "Specific Conductivity at 25°C (µS cm-1)",
      parameter == "pH"     ~ "pH",      
      parameter == "ORP"    ~ "Oxidation Reduction Potential (mV)",      
      parameter == "TURBF"  ~ "Turbidity (FNU)",      
      parameter == "CHLF"   ~ "Chlorophyll Fluorescence (µg L-1)",  
      parameter == "O2SAT"  ~ "Oxygen saturation (%)",  
      parameter == "O2"     ~ "Dissolved Oxygen (mg L-1)",  
      parameter == "FDOM"   ~ "Fluorescent dissolved organic matter (µg L-1)",  
      TRUE ~ "unknown parameter"
    )
    Limits <- c(floor(min(DF[, parameter])), ceiling(max(DF[, parameter])))
    
    
    for (element in location_list)
    {
      #subset: one location, last a years, one parameter
      df <- subset(DF, location==element & date >= (lastYear-years(a))  &  date < lastYear) 
      df$depth <- c(df$D_Water)
      df$value <-  c(df[, parameter])
      
      interp_depth <- max(df$depth)-0.5      #!!!!!!
      
      Path <- case_when(
        str_sub(df[1, "location"], end=3) == "BOL"   ~ pathbol,    
        str_sub(df[1, "location"], end=3) == "RIN"   ~ pathrin,
        str_sub(df[1, "location"], end=3) == "VOM"   ~ pathvom,
        TRUE ~ pathfig )
      
      
      val_interp_depth <- crossing(
        tibble(date = unique(df$date)), 
        tibble(depth = seq(1, interp_depth, by=0.1)) 
      ) %>%
        group_by(date) %>%
        mutate(value = estimate_val_by_date(date[1], depth)) 
      
      val_raster <- crossing(
        tibble(date = seq(ymd(min(df$date)), ymd(max(df$date)), by = 1)),
        tibble(depth = unique(val_interp_depth$depth))
      ) %>%
        group_by(depth) %>%
        mutate(value = estimate_val_by_depth(depth[1], date))
      
      val_raster$year <- str_sub(val_raster$date, end=4)      
      
      ggplot(val_raster, aes(date, depth, fill = value)) +
        geom_raster() +
        scale_y_reverse() +
        scale_x_date(date_breaks="1 month", date_labels="%b")+
        scale_fill_scico(palette = Palette, direction=-1, na.value = "transparent", limits = Limits) + 
        coord_cartesian(expand = FALSE)+
        labs(y = "depth in m", fill = Legendtitel )+
        facet_grid(col = vars(year),  scale="free_x", space="free_x")+
        theme(axis.title.x=element_blank())
      
      ggsave(filename = paste0("WQ_profile_", element, "_", parameter, "continuous.svg" ), 
             path=Path, height = 6, width = 10, dpi="retina" )
      
      
      caption <- paste("Depth profile of", ParameterName, "at location", Site, "in", Year)
      writeLines(caption, paste0(pathcap, "WQ_profile_", element, "_", parameter, "continuous_caption.md" ))
      
    } 
    
  } 
  
  return(print(file_list))
  
}

#does not work yet
depth_measurements_LakeMonitoR <- function() {
  Site <- "BOL"
  parameter_list <- c( "TW", "O2", "O2SAT", "pH")
  #only for last full year or for all data (grouped by year)
  
  
  if(!require(remotes)){install.packages("remotes")}  #install if needed
  remotes::install_github("leppott/LakeMonitoR")
  library(LakeMonitoR)
  
  
  df_list <- list()
  location_list <- list()
  
  file_list <- list.files(pattern=paste0("SITES_SONDE-PROF_", Site), path = pathdata)
  
  for (file in file_list) {
    skip <- which((readLines(con= paste0(pathdata, file)))=="####")
    df <- read.csv(paste0(pathdata, file), skip = skip, sep=",", header = T, dec=".", stringsAsFactors = T)
    LocationName <- str_sub(file, start = 18, end=-36)
    location_list <- append(location_list, list(LocationName))
    df <- mutate(df, location = LocationName)
    df_list <- append(df_list, list(df))
    assign(paste0("df_", LocationName), df)
  }  
  
  DF <- bind_rows(df_list)
  DF <- mutate(DF, date=substring(DF$TIMESTAMP, 0, 10) )
  DF$location <- as.factor(DF$location)
  DF$date <- as_datetime( DF$TIMESTAMP)
  lastYear <- floor_date(max(DF$date), unit="year")
  
  element <- "BOL_BS-E"
  
  df <- subset(DF, location==element & date >= (lastYear-years(1))  &  date < lastYear) 
  df$depth <- c(df$D_Water)
  
  
  # Data (Test Lake)
  data         <- df
  
  # Column Names
  col_datetime <- "date"
  col_depth    <- "depth"
  col_measure  <- "TW"
  
  # Plot Labels
  lab_datetime <- "Date Time"
  lab_depth    <- "Depth (m)"
  lab_measure  <- "Temperature (F)"
  lab_title    <- "Test Lake"
  line_val     <- 2
  
  
  # Create Plot
  p_hm <- plot_heatmap(data = data
                       , col_datetime = col_datetime
                       , col_depth = col_depth
                       , col_measure = col_measure
                       , lab_datetime = lab_datetime
                       , lab_depth = lab_depth
                       , lab_measure = lab_measure
                       , lab_title = lab_title
                       , contours = TRUE)
  
  # Print Plot
  print(p_hm)
}



###### discharge() ###### 



discharge <- function(Site, a) 
{
  df_list <- list()
  location_list <- list()
  
  file_list <- list.files(pattern=paste0("SITES_WB-SL-Q_", Site), path = pathdata)
  
  for (file in file_list) {
    skip <- which((readLines(con= paste0(pathdata, file)))=="####")
    df <- read.csv(paste0(pathdata, file), skip = skip, sep=",", header = T, dec=".", stringsAsFactors = T)
    LocationCode <- str_sub(file, start = 19, end=-36)
    location_list <- append(location_list, list(LocationCode))
    df <- mutate(df, location = LocationCode) 
    df_list <- append(df_list, list(df)) 
    assign(paste0("df_", LocationCode), df) 
  }   
  
  DF <- bind_rows(df_list)
  DF <- mutate(DF, date=substring(DF$TIMESTAMP, 0, 10))
  DF$location <- as.factor(DF$location)
  DF$date <- as.Date( DF$date, format="%Y-%m-%d")
  DF$year <- factor(format(DF$date, "%Y"))
  DF$doy <- as.Date(yday(DF$date))
  lastYear <- floor_date(max(DF$date), unit="year")
  
  DF <- DF %>% mutate(LocationName = case_when(  
    location == "IN-LIL" ~ "Lillån",
    location == "IN-ONN" ~ "Önnekvarn",
    location == "IN-STO" ~ "Storån",
    TRUE ~ "unknown Location" ))
  
  Path <- case_when(
    Site == "BOL"   ~ pathbol,    
    Site == "RIN"   ~ pathrin,
    Site == "VOM"   ~ pathvom,
    TRUE ~ pathfig )
  

  ggplot(subset(DF, date >= max(date)-years(a)), 
         aes(doy, Q, color= LocationName))+
    geom_line()+ 
    geom_point()+
    facet_grid(col = vars(year),  scale="free_x", space="free_x")+
    scale_x_date(date_labels = ("%b"), breaks="month")+
    theme_bw()+
    labs(y = "Discharge in m³/s", color = "Inlet")+
    theme(axis.title.x=element_blank())
  ggsave(filename = paste0("WQ_discharge_", Site, "_a", a, ".svg" ), 
         path=Path,  dpi="retina", height=6, width=3*a ) 
  
  caption <- paste("Temporal progression of the discharge in tributaries at site", Site)
  writeLines(caption, paste0(pathcap, "WQ_discharge_", Site, "_a", a, "_caption.md" ))
  
  return(print(file_list))
}


#a      windowlength, for how many years
#Site   "BOL", "VOM", "RIN" (determines what files are used)

#LocationName-variable set for Lillån (IN-LIL), Önnekvarn (IN-ONN), Storån (IN-STO)
#add new LocationName to function for additional locations


a <- 3
Site <- "BOL"


discharge(Site, a)



