library(dplyr)
library(lubridate)
library(readxl)
library(padr)

## 3 year period interval

StationNumber <- c(paste0(0,906),paste0(0,922),1007,1022,1024,1029,1030,1038,1043,1071,1073,
                   1107,1111,1118,1121,1212,1213,1223,1307,1311,1319,1320,1407,
                   1416,1419,1421)
StationName <- c("Hetaunda N.F.I","Gaur","Kakani","Godavari","Dhulikhel","Khumaltar","Kathmandu Airport",
                 "Dhunibesi","Nagarkot","Buddhanilakantha","Khokhana","Sindhuli Gadhi",
                 "Janakpur Airport","Manusmara","Karmaiya","Phatepur","Udaypur Gadhi","Rajbiraj",
                 "Dhankuta","Dharan Bazar","Biratnagar Airport","Tarahara","Ilam Tea Estate",
                 "Kanyam Tea Estate","Phidim (Panchthar)","Gaida(Kankai)")

path1 <- "E:/Project/Data_Assessment/Data/2.Processed Data/Selected Data/Temperature/"
path2 <- "E:/Project/Data_Assessment/Data/3. Filled Data/Temperature/No Condition/"

path3  <- "E:/Project/Data_Assessment/Data/3. Filled Data/Temperature/"

for (i in 9:9){
  mydata1 <- read_excel(paste0(path1,StationNumber[i],"Selected.xlsx"),
                        sheet="Sheet1",
                        skip=1,
                        col_names = FALSE)
  mydata2 <- read_excel(paste0(path2,StationNumber[i],"Filled.xlsx"),
                        sheet="Sheet1",
                        skip=1,
                        col_names = FALSE)
  
  #Renaming columns
  colnames(mydata1)[1] <- 'Date'
  colnames(mydata1)[2] <- "Tmax"
  colnames(mydata1)[3] <- "Tmin"
  
  
  
  
  colnames(mydata2)[1] <- 'Date'
  colnames(mydata2)[2] <- "FTmax"
  colnames(mydata2)[3] <- "FTmin"
  
  
  
  mydata1$Date <- as.Date(mydata1$Date)
  mydata2$Date <- as.Date(mydata2$Date)
  
  
  
  mydata1 <- na.omit(mydata1)
  

  #Plot of Monthly Average Temperature vs Time
  
  
  
  
  mydata1$Month <- format(mydata1$Date,"%m")
  Groupdata <- dplyr::group_by(mydata1,Month)
  Summary1 <- dplyr::summarise(Groupdata,AvgTmax = mean(Tmax))
  Summary2 <- dplyr::summarise(Groupdata,AvgTmin = mean(Tmin))
 

 
  
  mydata2$Month <- format(mydata2$Date,"%m")
  Groupdata <- dplyr::group_by(mydata2,Month)
  Summary3 <- dplyr::summarise(Groupdata,AvgFTmax = mean(FTmax))
  Summary4 <- dplyr::summarise(Groupdata,AvgFTmin = mean(FTmin))
  
  
  Summary1$AvgFTmax <- Summary3$AvgFTmax
  Summary1$AvgTmin <- Summary2$AvgTmin
  Summary1$AvgFTmin <- Summary4$AvgFTmin
  
  SummaryA <- data.matrix(t(Summary1))
  SummaryA <- matrix(as.numeric(SummaryA),ncol=12)
  SummaryA <- SummaryA[-1,]
  colnames(SummaryA) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                          "Aug","Sep","Oct","Nov","Dec")
  
  

  

  #Plot Annual Max and Min Temp
  mydata1$Year <- format(mydata1$Date,"%Y")
  mydata2$Year <- format(mydata2$Date,"%Y")
  
  
  Groupdata1 <- dplyr::group_by(mydata1,Year)
  Groupdata2 <- dplyr::group_by(mydata2,Year)
  
  Summary5 <- dplyr::summarize(Groupdata1,AvgTmax = mean(Tmax))
  
  Summary5$Year <- as.Date(Summary5$Year, "%Y")
  Summary5 <- pad(Summary5)
  
 
  AA <- c("Date","AvgTmax","AvgTmin")
  BB <- c("1989-8-25"," " ,"  "  )
  
  
  
  
  
  
  Summary6 <- dplyr::summarize(Groupdata1,AvgTmin = mean(Tmin))
  
  
  Summary6$Year <- as.Date(Summary6$Year, "%Y")
  Summary6 <- pad(Summary6)
   
  
  
  
  
  Summary7 <- dplyr::summarize(Groupdata2,AvgFTmax = mean(FTmax))
  Summary8 <- dplyr::summarize(Groupdata2,AvgFTmin = mean(FTmin))
  Summary5$AvgTmin <- Summary6$AvgTmin
  

  Summary5 <- rbind(BB,Summary5)
  
  Summary7$AvgFTmin <- Summary8$AvgFTmin
  
  Summary7 <- Summary7[-c(1)]
  
  Summary5 <- cbind(Summary5,Summary7)
  
  Summary5$Year <- as.Date(Summary5$Year, "%Y")
  

  
  png(file=paste0("E:/Project/Data_Assessment/Data/3. Filled Data/Plots/Temperature/",StationNumber[i],"ComparePlots.png"),
      width=2000, height = 600)
  
  par(mfrow = c(1,2),oma=c(0,0,2,0),mar=c(7.4,5.5,4.1,2.1))
  

  barplot(SummaryA,
          col= c("blue","red","orange","maroon"), ylim = c(0,50),
          ylab ="Monthly average Temperature (\u00B0C)", xlab= " ",
          las=2,
          axes =TRUE,cex.lab=1.7,cex.axis = 1.7,cex.names =1.7,
          main ="(a)",cex.main=1.8,
          space = c(0.5,2),
          beside=TRUE)
  
  
  title(xlab = "Time(in months)",line = 4,cex.lab = 1.7)
  
  legend("top",legend = c("Unfilled Tmax", "Filled Tmax", "Unfilled Tmin","Filled Tmin"),
         fill=c("blue", "red","orange","maroon"), cex=1.6,
         bty= "n")
  box()
  
  
 
  
  
  
  
  plot(Summary5$Year,Summary5$AvgTmax,
       type = "l",xaxt="n",ylim=c(0,50),
       xlab = " ", ylab = " ",
       col= "blue", lwd = 4, las = 1, main = "(b)",cex.axis=1.7,cex.lab = 1.7,cex.main =1.8
  )
  
  
  lines(Summary5$Year, Summary5$AvgTmin, col="orange",lwd=4)
  lines(Summary5$Year, Summary5$AvgFTmax, col="red",lwd=4)
  lines(Summary5$Year, Summary5$AvgFTmin, col="maroon",lwd=4)
  
  legend("topleft",legend = c("Unfilled Tmax", "Filled Tmax", "Unfilled Tmin","Filled Tmin"),
         fill=c("blue", "red","orange","maroon"), cex=1.6,
         bty= "n")
  
  title(xlab = "Time(in years)",line = 4.7,cex.lab=1.7)
  title(ylab = "Annual Average Temperature (\u00B0C)",line = 4.3,cex.lab=1.7)
  
  axis.Date(1,at=seq(min(Summary5$Year),max(Summary5$Year),by = "3 year"), format = "%Y",las=2,cex.axis=1.7)
  
  
  
  
  
  mtext(paste(StationName[i],"(","Index :",StationNumber[i],")"),outer=TRUE,cex=2.5)
  
  dev.off()
  
} 





