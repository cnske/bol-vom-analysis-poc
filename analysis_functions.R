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

#start of function
#needed for function: raw data (dfraw), parameter ("Nederbördsmängd"), window lenght (x years for one plot)
#raw data only from one station! add stationname in subtitle

a = 4
filename="metobs_bol.parquet"
parameter="Nederbördsmängd"



Precipitation_distribution() <- function(a, filename, parameter)
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
    
    ggsave(filename = str_c("histogram_P_t", t1, ".png"), dpi = "retina")
    
    
    t1 <- t1 + a       #change running index
  }    #end of loop
  
  return(t1)
  
}      #end of function




###### temperature_distribution() ###### 


#start of function
#needed for function: raw data (dfraw), parameter ("Lufttemperatur"), window lenght (x years for one plot)
#raw data only from one station!

dfraw <- arrow::read_parquet("metobs_bol.parquet", col_select = c(TimeStamp,StationName, ParameterName, Unit, Value))
dfraw$ParameterName <- as.factor(dfraw$ParameterName)
dfraw$Unit <- as.factor(dfraw$Unit)
dfraw$Date <- as.Date((substring(dfraw$TimeStamp, 0, 10)), format="%Y-%m-%d")

lastYear <- floor_date(max(dfraw$Date), unit="year")
df30y <- subset(dfraw, Date < lastYear & Date >= (lastYear-years(30)) & StationName =="Ljungby A") #!!!
StationName <- str_c(df30y[1, "StationName"])

DF <- setNames(aggregate(Value~Date,(subset(df30y, ParameterName=="Lufttemperatur")), 
                         mean, na.rm=TRUE, na.action=NULL), c("Date", "Temperatur_d"))
t1 <- 0 #start at year 0

#beginning of loop, set a (windowlength, how many years in one plot)

dfT <- subset(DF, Date >= (min(Date)+years(t1))   &   Date < (min(Date)+years(t1+a))  )

Tmean <- round(mean(dfT$Temperatur_d, na.rm=TRUE), digits=1)
Tmax <- round(max(dfT$Temperatur_d, na.rm=TRUE), digits=1)
Tmin <- round(min(dfT$Temperatur_d, na.rm=TRUE), digits=1)


windows()
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

ggsave(filename = str_c("histogram_T_t", t1, ".png"), dpi = "retina")

t1 <- t1 + a       #change running index

#end of loop


###### depth_measurements() ###### 

###### discharge() ###### 
 








