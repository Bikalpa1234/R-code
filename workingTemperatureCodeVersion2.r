library(dplyr)
library(lubridate)
library(readxl)


StationNumber <- c(paste0(0,905),paste0(0,906),paste0(0,922),1007,1022,1024,1029,1030,1036,1038,1039,1043,
                    1052,1059,1071,1073,1107,1111,1118,1121,1122,1212,1213,1216,1223,1307,1311,1316,1319,1320,1407,
                    1408,1416,1419,1421)
StationName <- c("Daman","Hetaunda N.F.I","Gaur","Kakani","Godavari","Dhulikhel","Khumaltar","Kathmandu Airport","Panchkhal",
                 "Dhunibesi","Panipokhari (Kathmandu)","Nagarkot","Bhaktapur","Changu Narayan","Buddhanilakantha",'Khokana',"Sindhuli Gadhi",
                 "Janakpur Airport","Manusmara","Karmaiya","Jalesore","Phatepur",'Udayapur Gadhi',"Lahan","Siraha","Rajbiraj",
                 "Dhankuta","Dharan Bazar","Chatara","Biratnagar Airport","Tarahara","Illam Tea Estate","Damak",
                 "Kanyam Tea Estate","Phidim (Panchthar)","Gaida(Kankai)")


path <- "E:/Project/Data_Assessment/Data/2.Processed Data/Excel Data/Temperature/TemperatureFinal/"


for (i in 1:35){
  mydata <- read_excel(paste0(path,StationNumber[i],"Final.xlsx"),
                       sheet="Sheet1",
                       skip=1,
                       col_names = FALSE)
  
  #Renaming columns
  colnames(mydata)[1] <- 'Date'
  colnames(mydata)[2] <- "Tmax"
  colnames(mydata)[3] <- "Tmin"
  
  #Converting Date in Date format
  mydata$Date <- as.Date(mydata$Date)
  
  #Plot of Daily Temperature vs Time
  
  
  mydata1 <- na.omit(mydata)
  
  #Plot of Monthly Average Temperature vs Time
  
  mydata1$Month <- format(mydata1$Date,"%m")
  Groupdata <- dplyr::group_by(mydata1,Month)
  Summary1 <- dplyr::summarise(Groupdata,AvgTmax = mean(Tmax))
  Summary2 <- dplyr::summarise(Groupdata,AvgTmin = mean(Tmin))
  Summary1$AvgTmin <- Summary2$AvgTmin
  Summary <- data.matrix(t(Summary1))
  Summary <- matrix(as.numeric(Summary),ncol=12)
  Summary <- Summary[-1,]
  colnames(Summary) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                         "Aug","Sep","Oct","Nov","Dec")
  
  
  #Plot Annual Max and Min Temp
  mydata1$Year <- format(mydata1$Date,"%Y")
  Groupdata <- dplyr::group_by(mydata1,Year)
  Summary3 <- dplyr::summarize(Groupdata,MaxTmax = max(Tmax))
  Summary4 <- dplyr::summarize(Groupdata,MaxTmin = max(Tmin))
  Summary3$Year <- as.Date(Summary3$Year, "%Y")
  Summary4$Year <- as.Date(Summary4$Year, "%Y")
  
  
  png(file=paste0("E:/Project/Data_Assessment/Data/2.Processed Data/Plots/Temperature/Changed/",StationNumber[i],"AllPlots.png"),
      width=1000, height = 550)
  
  par(mfrow = c(2,2),oma=c(0,0,2,0),mar=c(7,5,4.1,2.1))
  
  plot(mydata$Date, mydata$Tmax,
       type = "l", xaxt="n",
       xlab = " ", ylab = "Daily Temperature (\u00B0C)",
       las = 1,col = "blue",main ="(a)",
       ylim = c(-10,60))
  
  
  lines(mydata$Date, mydata$Tmin, col="orange")
  axis.Date(1,at=seq(min(mydata$Date),max(mydata$Date),by = "3 year"), format = "%Y-%m-%d",las=2)
  title(xlab = "Time(in days)",line = 6)
  
  legend("top",legend = c("Tmax", "Tmin"),
         col=c("blue", "orange"), lty=1:1,
         bty= "n")
  
  
  barplot(Summary,
          col= c("blue","orange"), ylim = c(0,50),
          ylab ="Monthly average Temp (\u00B0C)", xlab = "Time(in months)",
          las=2,
          axes =TRUE,
          main ="(b)",
          space = c(0.5,2),
          beside=TRUE)
  
  legend("top",legend = c("Tmax", "Tmin"),
         fill=c("blue", "orange"), 
         bty= "n")
  box()
  
  
  plot(Summary3$Year,Summary3$MaxTmax,
       type = "l",xaxt="n",
       xlab = " ", ylab = "Annual Maximum Tmax (\u00B0C)",
       col= "blue", lwd = 2, las = 1, main = "(c)"
       )
  title(xlab = "Time(in years)",line = 4)
  
  axis.Date(1,at=seq(min(Summary3$Year),max(Summary3$Year),by = "3 year"), format = "%Y",las=2)
  
  
  
  plot(Summary4$Year,Summary4$MaxTmin,
       type = "l",xaxt="n", 
       xlab = " ", ylab = "Annual Maximum Tmin (\u00B0C)",
       col= "darkorange", lwd = 2, las = 1, main ="(d)")
 
   title(xlab = "Time(in years)",line = 4)
  
  axis.Date(1,at=seq(min(Summary4$Year),max(Summary4$Year),by = "3 year"), format = "%Y",las=2)
  
  
  
       
  mtext(paste(StationName[i],"(","Index :",StationNumber[i],")"),outer=TRUE,cex=1.5)
  
  dev.off()

}  
  

