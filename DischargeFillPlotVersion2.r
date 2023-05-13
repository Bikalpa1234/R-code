library(dplyr)
library(lubridate)
library(readxl)
library(padr)
library(plyr)

## 3 year period interval


path1 <- "G:/Final Year Project/3.Data Quality Assessment/2.Complete Data (Available+Bought)/3. Selected Data/Selected Excel Data/Discharge/"
path2 <- "G:/Final Year Project/3.Data Quality Assessment/3. Filled Data/1. Excel Data/Discharge/No Condition/"
path3 <- "G:/Final Year Project/3.Data Quality Assessment/3. Filled Data/2.Filled Plots/Process check/"


StationNumber <- c(505,550.05,589,728,795)
StationName <- c("Sundarijal","Khokhana","Pandheradovan","Rajdwali","Mainachuli")

for (i in 1:5){
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
  colnames(mydata1)[2] <- "Discharge"
  
  
  colnames(mydata2)[1] <- 'Date'
  colnames(mydata2)[2] <- "FDischarge"
  
  
  mydata1$Date <- as.Date(mydata1$Date)
  mydata2$Date <- as.Date(mydata2$Date)
  
  mydata1 <- na.omit(mydata1)
  
  
  
  
  #Plot of Monthly Average Temperature vs Time
  mydata1$Month <- format(mydata1$Date,"%m")
  Groupdata <- dplyr::group_by(mydata1,Month)
  Summary1 <- dplyr::summarise(Groupdata,AvgDis = mean(Discharge))
  
  
  
  
  mydata2$Month <- format(mydata2$Date,"%m")
  Groupdata <- dplyr::group_by(mydata2,Month)
  Summary2 <- dplyr::summarise(Groupdata,AvgFDis = mean(FDischarge))
  
  
  
  Summary1$AvgFDis <- Summary2$AvgFDis
  
  
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
  
  Summary3 <- dplyr::summarize(Groupdata1,AvgDis= mean(Discharge))
  Summary4 <- dplyr::summarize(Groupdata2,AvgFDis= mean(FDischarge))
  
  Summary3$Year <- as.Date(Summary3$Year, "%Y")
  Summary3 <- pad(Summary3)
  
  Summary4<- Summary4[-c(1)]
  Summary3$AvgFDis <- Summary4$AvgFDis
  
  
  
  
  
  
  
  png(file=paste0(path3,StationNumber[i],"ComparePlots.png"),
      width=2000, height = 600)
  
  par(mfrow = c(1,2),oma=c(0,0,2,0),mar=c(7.4,5.9,4.1,2.1))
  
  llimit <- round_any(min(SummaryA[,]),10)
  ulimit <- round_any(max(SummaryA[,]),10,f = ceiling)
  
  barplot(SummaryA,
          col= c("blue","red"), ylim = c(0,ulimit),
          ylab =bquote( "Monthly average Discharge" ~(m^3/s) ), xlab= " ",
          las=2,
          axes =TRUE,cex.lab=1.5,cex.axis = 1.5,cex.names =1.5,
          main ="(a)",cex.main=1.7,
          space = c(0.5,2),
          beside=TRUE)
  
  
  title(xlab = "Time(in months)",line = 4,cex.lab = 1.5)
  
  legend("topleft",legend = c("Unfilled Discharge", "Filled Discharge"),
         fill=c("blue", "red"), cex=1.5,
         bty= "n")
  box()
  
  
  
  
  
  llmit <- round_any(min(Summary3$AvgDis,Summary3$AvgFDis,na.rm=TRUE),5 )
  ulimit <- round_any(max(Summary3$AvgDis,Summary3$AvgFDis,na.rm=TRUE),5,f=ceiling)
    
  plot(Summary3$Year,Summary3$AvgDis,
       type = "l",xaxt="n",ylim=c(0,ulimit),
       xlab = " ", ylab = " ",
       col= "blue", lwd = 4, las = 1, main = "(b)",cex.axis=1.5,cex.lab = 1.5,cex.main =1.7
  )
  
  
  lines(Summary3$Year, Summary3$AvgFDis, col="red",lwd=4)
  
  
  legend("topleft",legend = c("Unfilled Discharge", "Filled Discharge"),
         fill=c("blue", "red"), cex=1.5,
         bty= "n")
  
  title(xlab = "Time(in years)",line = 4.7,cex.lab=1.5)
  title(ylab = bquote( "Annual average Discharge" ~(m^3/s) ),line = 3.9,cex.lab=1.5)
  
  axis.Date(1,at=seq(min(Summary3$Year),max(Summary3$Year),by = "3 year"), format = "%Y",las=2,cex.axis=1.5)
  
  
  
  
  
  mtext(paste(StationName[i],"(","Index :",StationNumber[i],")"),outer=TRUE,cex=2.5)
  
  dev.off()
  
} 


