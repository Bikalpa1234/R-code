library(dplyr)
library(lubridate)
library(readxl)

## 3 year interval 0-600 #4
StationNumber <- c(paste0(0,904),paste0(0,906),paste0(0,907),paste0(0,910))
StationName <- c("Chisapani Gadhi", "Hetaunda N.F.I", "Amlekhganj","Nijgadh")

for (i in 1:4){
  data <- read_excel(paste0("E:/Project/Data_Assessment/Data/2.Processed Data/Excel Data/Rainfall/Rainfall Final/",StationNumber[i],".xlsx"),
                     sheet="Sheet1",
                     skip=1,
                     col_names = FALSE)
  
  #Renaming columns
  colnames(data)[1] <- 'Date'
  colnames(data)[5] <- "Rainfall"
  data <- data[-c(2,3,4)]
  
  #Converting Date in Date format
  data$Date <- as.Date(data$Date)
  
  
  
  #Plot of Daily Rainfall vs Time
  
  
  data21 <- na.omit(data)
  mydata2 <- data21
  mydata3 <- data21
  mydata2$Rainfall <- mydata2$Rainfall/1000
  mydata2$cum <- cumsum(mydata2$Rainfall)
  mydata3$cum <- cumsum(mydata3$Rainfall)
  mydata3$Date <- as.Date(mydata3$Date)
  
  #Plot of Monthly Average Rainfall vs Time
  
  data21$Month <- format(data21$Date,"%m")
  Groupdata <- dplyr::group_by(data21,Month)
  Summary1 <- dplyr::summarise(Groupdata,AvgRain = max(Rainfall))
  Summary1$Month <- month.abb
  Summary <- data.matrix(t(Summary1))
  Summary <- Summary[-1,]
  Summary <- matrix(as.numeric(Summary),ncol=12)
  colnames(Summary) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  
  #Plot Annual Average Rainfall
  data21$Year <- format(data21$Date,"%Y")
  Groupdata <- dplyr::group_by(data21,Year)
  Summary2 <- dplyr::summarize(Groupdata,AAvgRain = sum(Rainfall))
  Summary2$Year <- as.Date(Summary2$Year,"%Y")
  
  png(file=paste0("E:/Project/Data_Assessment/Data/2.Processed Data/Plots/Rainfall/Changed/",StationNumber[i],"AllPlots.png"),
      width=1000, height = 550)
  
  par(mfrow = c(2,2),oma=c(0,0,2,0),mar=c(7.2,5,4.1,2.1))
  
  #Plots Rainfall vs Days
  plot(data$Date, data$Rainfall, 
       type = "l", xaxt="n",
       xlab = " ", ylab = " ",cex.axis=1.1,
       las = 1,col = "blue",main = "(a)")
  
  axis.Date(1,at=seq(min(data$Date),max(data$Date),by = "3 year"), format = "%Y-%m-%d",las=2,cex.axis=1.1)
  title(xlab = "Time(in days)",line = 6.2,cex.lab=1.2)
  
  #Shifts Y-Axis Labels as desired
  title(ylab = "Daily Rainfall (mm)",line = 3.3,cex.lab=1.2)
  
  #Plots Cumulative Rainfall vs Days
  plot(mydata2$Date, mydata2$cum,
       type = "l",xaxt="n",
       xlab = " ", ylab = " ",cex.axis=1.1,
       las = 1,col = "blue",main = "(b)")
  
  axis.Date(1,at=seq(min(mydata3$Date),max(mydata3$Date),by = "3 year"), format = "%Y-%m-%d",las=2,cex.axis=1.1)
  title(xlab = "Time(in days)",line = 6.2,cex.lab=1.2)
  
  reg1 <- lm(mydata2$cum ~ mydata2$Date) 
  reg2 <- lm(mydata3$cum ~ mydata3$Date)
  
  coeff=coefficients(reg2)
  coeff1 <- round(coeff[1],1)
  coeff2 <- round(coeff[2],1)
  
  a <- sign(round(coeff[1],1))
  if (a == -1){
    coeff1 <- coeff1*-1
    sgn = "-"
  }else{
    sgn = "+"
  }
  
  #eq = paste0("y = ",round(coeff[2],1)," * x ",sgn, round(coeff[1],1))
  #r2 = paste0(c(("R2 = ")) , round(summary(reg1)$r.squared,digits=3))
  #r2 = as.character(round(summary(reg1)$r.squared,digits=3))
  #r2.exp = expression(paste("",R^2, "=" ))
  
  modsum = summary(reg2)
  r2 = modsum$adj.r.squared
  mylabel = as.expression(bquote(italic(R)^2 == .(format(r2,digits = 3))))
  #text(x=2,y=30,labels = mylabel)
  
  
  l <- as.expression(bquote("y"== ~.(format(coeff2))~ "* x" ~.(format(sgn)) ~ .(format(coeff1))) )
  #text(x=2,y=,labels=l)
  
  legend("topleft",legend =c(l,mylabel), bty ="n")
  
  abline(reg1,col="black",lty=2,lwd=1.5)
  
  
  #Shifts Y-Axis Labels as desired
  title(ylab = "Cum. Rainfall in thousands (mm)",line = 3.3,cex.lab=1.2)
  
  #Plots Monthly Maximum vs Month
  
  barplot(Summary,
          col= "blue", 
          ylab ="Monthly Rainfall (mm)", xlab = " ",
          las=2,ylim=c(0,600),
          axes=TRUE,cex.axis = 1.1,cex.names=1.1,cex.lab=1.2,
          main = "(c)",
          space = c(2,2)
  )
  
  title(xlab = "Time(in months)",line = 4.2,cex.lab=1.2)
  
  box()
  
  #Plots Annual Average vs Year
  plot(Summary2$Year, Summary2$AAvgRain,
       type="l",ylim=c(0,5000),xaxt="n",
       xlab = " ", ylab = "  ",cex.axis=1.1,
       las = 1,lwd=2,col = "blue",main = "(d)")
  
  title(xlab = "Time(in years)",line = 4.2,cex.lab=1.2)
  title(ylab = "Annual Rainfall (mm)",line = 3.3,cex.lab=1.2)
  
  axis.Date(1,at=seq(min(Summary2$Year),max(Summary2$Year),by = "3 year"), format = "%Y",las=2)
  
  
  mtext(paste(StationName[i],"(","Index :",StationNumber[i],")"),outer=TRUE,cex=1.5)
  
  dev.off()
  
}





