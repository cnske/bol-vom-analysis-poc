#analysis functions


packages <- c("here", "tidyverse", "doBy", "multcomp", "EnvStats", "readxl", "ggtext", "readtext", 
          "base", "nanoparquet", "scico", "arrow", "svglite", "ggpmisc", "raster")


install.packages(packages)
lapply(packages, library, character.only=TRUE)


here::i_am("analysis_functions.R")
setwd(here())

pathdata <- paste0(here(), "/data/")
pathbol <- paste0(here(), "/fig/svg/bol")
pathvom <- paste0(here(), "/fig/svg/vom")
pathrin <- paste0(here(), "/fig/svg/rin")
pathfig <- paste0(here(), "/fig")


###### climate_diagram() ###### 

filename <- "metobs_bol.parquet"


climate_diagram <- function(filename)
{
  dfraw <- arrow::read_parquet(paste0(pathdata, filename), 
                               col_select = c(TimeStamp,StationName, ParameterName, Unit, Value))
  dfraw$ParameterName <- as.factor(dfraw$ParameterName)
  dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")
  lastYear <- floor_date(max(dfraw$Date), unit="year")
  #adding StationNames to a list
  
  #loop for every station listed
  
  DF <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & StationName =="Ljungby A") #!!!
  StationName <- str_c(DF[1, "StationName"])
  DF$MonthYear <- as.factor(substring(DF$TimeStamp, 0, 7))
  
  SiteName <- case_when(
    StationName == "Ljungby A"   ~ "BOL",    
    StationName == "Hörby A "   ~ "RIN",
    StationName == "VOM Station?? "   ~ "VOM",           #!!!!
    TRUE ~ "unknown site" )
  
  Path <- case_when(
    SiteName == "BOL"   ~ pathbol,    
    SiteName == "RIN"   ~ pathrin,
    SiteName == "VOM"   ~ pathvom,
    TRUE ~ pathfig )
  
  
  monthlyP <- setNames(aggregate(Value ~ MonthYear,
    data = subset(DF, ParameterName=="Nederbördsmängd"), FUN=sum, na.rm=TRUE), 
    c("MonthYear", "Precipitation_m"))
  
  monthlyT <- setNames(aggregate(Value ~ MonthYear,
    data = subset(DF, ParameterName=="Lufttemperatur"), FUN=mean, na.rm=TRUE), 
    c("MonthYear", "Temperature_m"))
 
  avgP <- setNames(aggregate(Precipitation_m ~ str_sub(monthlyP$MonthYear, start = -2) ,
    data = monthlyP, FUN=mean, na.rm=TRUE),
    c("Month", "Precipitation"))
  
  avgT <- setNames(aggregate(Temperature_m ~ str_sub(monthlyT$MonthYear, start = -2) ,
    data = monthlyT, FUN=mean, na.rm=TRUE),
    c("Month", "Temperature"))
  avgT$Month_int <- as.integer(avgT$Month)
  str(avgT)
  #summary(subset(monthlyT, str_sub(monthlyT$MonthYear, start = -2) == "01")) #works!!
  
  #using avgP/T for climatediagram and monthlyP/T for last Year data
  #transform Month back into a Date? continous?
  
  ggplot()+
    geom_bar(data = avgP, aes(y = Precipitation/2, x = Month, fill="Precipitation"), stat="identity")+
    #geom_bar(mapping = aes(y = Precipitation_m/2, colour="grey"),
     #        data =  lastmonthlyP, stat="identity")
    geom_line(data = avgT, aes(y = Temperature, x = Month_int), linewidth=1.2)+
    scale_x_discrete()
  
  
  
  
  
  ggplot(df, aes(x= Date)) +
    geom_bar(aes(y= Precipitation_m/2, fill = "Precipitation_d"), stat="identity")  +
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
  
  ggsave(filename = str_c("MET_climate_", SiteName, "_", StationName, ".svg"), 
         path=Path, dpi = "retina")
  
  #end of loop for each station
  
}   #end of function




###### precipitation_distribution() ###### 


Precipitation_distribution <- function(a, filename, parameter)
{
  if (extension(filename) == ".parquet") {
    dfraw <- arrow::read_parquet(paste0(pathdata, filename), col_select = c(TimeStamp,StationName, ParameterName, Unit, Value))
  } else {
    dfraw <-  read.csv(paste0(pathdata, filename), sep=",", header = T, dec=".", stringsAsFactors = T)
  }
  
  dfraw$ParameterName <- as.factor(dfraw$ParameterName)
  dfraw$Unit <- as.factor(dfraw$Unit)
  dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")
  lastYear <- floor_date(max(dfraw$Date), unit="year")
  #add list of StationName in parquet
  
  #loop for every StationName
  
  df30y <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & StationName =="Ljungby A") #!!!
  StationName <- str_c(df30y[1, "StationName"])
  
  SiteName <- case_when(
    StationName == "Ljungby A"   ~ "BOL",    
    StationName == "Hörby A "   ~ "RIN",
    StationName == "VOM Station?? "   ~ "VOM",           #!!!!
    TRUE ~ "unknown site" )
  
  Path <- case_when(
    SiteName == "BOL"   ~ pathbol,    
    SiteName == "RIN"   ~ pathrin,
    SiteName == "VOM"   ~ pathvom,
    TRUE ~ pathfig )
  
  
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
    
    t1 <- t1 + a       
  }   
  
  #end of loop for every StationName
  
  return(t1)
  
}      

#parameters:    a = number of years for each plot (num)
#               filename = name of the raw parquet file (chr)
#               parameter = name of Precipitation column in parquet, e.g. "Nederbördsmängd" (chr)




Precipitation_distribution(10, "metobs_bol.parquet", "Nederbördsmängd")




###### temperature_distribution() ###### 


Temperature_distribution <- function(a, filename, parameter) 
{
  if (extension(filename) == ".parquet") {
    dfraw <- arrow::read_parquet(paste0(pathdata, filename), col_select = c(TimeStamp,StationName, ParameterName, Unit, Value))
  } else {
    dfraw <-  read.csv(paste0(pathdata, filename), sep=",", header = T, dec=".", stringsAsFactors = T)
  }
  
  dfraw$ParameterName <- as.factor(dfraw$ParameterName)
  dfraw$Unit <- as.factor(dfraw$Unit)
  dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")
  
  lastYear <- floor_date(max(dfraw$Date), unit="year")
  df30y <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & StationName =="Ljungby A") #!!!
  StationName <- str_c(df30y[1, "StationName"])
  
  
  SiteName <- case_when(
    StationName == "Ljungby A"   ~ "BOL",    
    StationName == "Hörby A "   ~ "RIN",
    StationName == "VOM Station?? "   ~ "VOM",           #!!!!
    TRUE ~ "unknown site" )
  
  Path <- case_when(
    SiteName == "BOL"   ~ pathbol,    
    SiteName == "RIN"   ~ pathrin,
    SiteName == "VOM"   ~ pathvom,
    TRUE ~ pathfig )
  
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
           subtitle=paste("between", format(min(dfT$Date), format="%d.%m.%Y"), 
                          "and", format(max(dfT$Date), format="%d.%m.%Y")) )+
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
  
  #loop for all year 
  #LastYear - years(a) 
  # a <- a+1
  
  for (parameter in parameter_list)
  {
    
 
    
    for (element in location_list)
    {
      #subset: one location, one year, one parameter
      df <- subset(DF, location==element & date >= (lastYear-years(1))  &  date < lastYear) 
      df$depth <- c(df$D_Water)
      df$value <-  c(df[, parameter])
      interp_depth <- max( df$depth[df$depth!=max(df$depth)] )
      
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
      
      #Binning <- max(DF$parameter) 
      rng = range(c(parameter_list))
      Limits <- c(floor(min(DF$parameter)), ceiling(max))
      
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
      
      
    }  #end of loop for element (location from location_list)
    
  }  #end of loop with parameter
  
  return(print(file_list))
  
}


#parameters:   Site = BOL, VOM or RIN 
#              parameter_list = list of the column that should be used



parameter_list <- c( "TW", "O2", "O2SAT", "pH")


depth_profile("BOL", parameter_list)

depth_profile("VOM", parameter_list)



###### discharge() ###### 


#a: for how many years
#Site: "BOL", "VOM", "RIN" (determines what files are used)

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
    assign(paste("df_", LocationCode, sep=""), df) 
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
  
  return(print(file_list))
}



a <- 3
Site <- "BOL"


discharge(Site, a)




