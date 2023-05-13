#Prerequisite: Rainfall Filled, Rainfall Final and missing_rainfall_v2.xlsx should
#be in same directory as this R script

#Generates plots to ../Plots/Rainfall/

rm(list=ls())

library(dplyr)
library(lubridate)
library(readxl)

StationNumber <- c(paste0(0,904),paste0(0,905),paste0(0,906),paste0(0,907),paste0(0,910),paste0(0,912),paste0(0,915),
                   paste0(0,919),paste0(0,922),paste0(0,924),paste0(0,925),1007,1015,1018,1022,1024,1029,1030,1035,1036,1038,
                   1039,1043,1049,1052,1059,1060,1071,1073,1074,1075,1076,1077,1079,1080,1081,1082,1107,1108,1109,1110,1111,1112,
                   1115,1117,1118,1119,1120,1121,1122,1210,1211,1212,1213,1215,1216,1223,1226,1227,1306,1307,1308,1309,1311,1312,1316,
                   1319,1320,1322,1326,1406,1407,1408,1409,1410,1415,1416,1419,1421)
StationName <- c("ChisaPani Gadhi", "Daman","Hetaunda N.F.I","Amlekhganj","Nijgadh","Ramoli Bairiya",
                 "Markhu Gaun","Makwanpur Gadhi","Gaur","Chyuntaha","Rajaiya","Kakani","Thankot","Baunepati",
                 "Godavari","Dhulikhel","Khumaltar","Kathmandu Airport","Sankhu","Panchkhal","Dhunibesi",
                 "Panipokhari (Kathmandu)","Nagarkot","Khopasi (Panauti)","Bhaktapur","ChanguNarayan","Chapagaun",
                 "Buddhanilakantha",'Khokana',"Sundarijal","Lele","Naikap","Sundarijal","Nagarjun","Tikathali",
                 "Jeetpurphedhi","Nangkhel","Sindhuli Gadhi","Bahun Tiplung","Pattharkot (East)","Tulsi",
                 "Janakpur Airport","Chisapani Bazar","Nepalthok","Hariharpur Gadhi Valley","Manusmara",
                 "Gausala","'Malangwa","Karmaiya","Jalesore","Kurule Ghat","Khotang Bazar","Phatepur",'Udayapur Gadhi',
                 "Lahan","Siraha","Rajbiraj","Barmajhiya","Gaighat","Munga","Dhankuta","Mulghat","Tribeni","Dharan Bazar",
                 "Haraincha","Chatara","Biratnagar Airport","Tarahara","Machuwaghat","Letang","Memeng Jagat",
                 "Illam Tea Estate","Damak","Anarmani Birta","Himali Gaun","Sanischare","Kanyam Tea Estate","Phidim (Panchthar)",
                 "Gaida(Kankai)")
imageWidth=1000
imageHt=550
lineWidth=2
axLFS=1.25 #axis Label Font Size
axTFS=1.35  #axis Title Font size
legFS=1.1 #legend Font Size
cTFS=1.6  #chart title FOnt size


PATH<-paste0(getwd(),'/Rainfall Filled/')
PATH1<-paste0(getwd(),'/missing_rainfall_v2.xlsx')
PATH2<-paste0(getwd(),'/Rainfall Final/')

#Returns dataframe with cumulative rainfall stored in 'cum' column
cum_rain<-function(df){
  df<-na.omit(df)
  df$cum<-cumsum(df$Rainfall)/1000
  return(df)
}

#Returns dataframe with monthly average rainfall stored in AvgRain column
monthly_rain<-function(df){
  df[is.na(df)]<-0
  monthly<-df %>%
            group_by(Month, Year) %>%
            summarise(m = sum(Rainfall)) %>%
            summarise(AvgRain = mean(m)) %>%
            t() 

  monthly<-data.matrix(monthly)[-1,]
  monthly<-matrix(monthly, ncol=12)
  colnames(monthly) <- c("Jan","Feb","Mar","Apr","May","Jun",
                         "Jul","Aug","Sep","Oct","Nov","Dec")
  return(monthly)
  
}

seasonal_rain<-function(df){
  df[is.na(df)]<-0
  return(df %>%
    filter((Month %in% 6:9)) %>%
    group_by(Year) %>%
    summarise(SeasonalRain = sum(Rainfall))
  )
}
#Returns dataframe with annual rainfall stored in AnnualRain column
annual_rain<-function(df){
  df[is.na(df)]<-0
  annual<- df %>%
            group_by(Year) %>%
            summarise(AnnualRain = sum(Rainfall))
  return(annual)
}

to_plot=list.files(path=PATH)
# to_plot=c("1030.xlsx")
for (file in to_plot){
  data <- read_excel(paste0(PATH,file),
                     sheet="Sheet1")
  df <- read_excel(paste0(PATH2,file),
                   sheet="Sheet1")
  years<-unique(data$Year)
  start_year<-min(data$Year)
  end_year<-max(data$Year)
  df<- filter(df, (Year>=start_year & Year<=end_year))
  
  station=substring(file,1,4)
  i<-which(StationNumber==station)
  #Renaming columns
  colnames(data)[1] <- 'Date'
  colnames(df)[1] <- 'Date'
  # data <- data[-c(2,3,4)]
  

  #Converting Date in Date format
  data$Date <- as.Date(data$Date)

  start_date<-data$Date[1]
  end_date<-data$Date[nrow(data)]
  tickDates<-seq(start_date, end_date, by="year")
  labelDates<-seq(start_date, end_date, by="3 year")
  
  #Plot of Daily Rainfaldf vs Time

  mydata2<-cum_rain(data)
  Summary1<-monthly_rain(data)
  Summary1.5<-monthly_rain(df)
  MonthlyRain<-rbind(Summary1, Summary1.5)
  Summary2<-annual_rain(data)
  Summary2.5<-annual_rain(df)
  Summary3<-seasonal_rain(data)
  Summary3.5<-seasonal_rain(df)

  png(file=paste0(getwd(),"/Plots/Rainfall/",station,".png"),
      width=imageWidth, height = imageHt)

  par(mfrow = c(1,2),oma=c(0,0,2,0),mar=c(6.2,6,4.1,2.1))
  
  #Plots Annual Average vs Year
  plot(years, Summary2$AnnualRain,
       type="l", #ylim=c(0,2500), #for station 1030
       ylim=c(0,plyr::round_any(max(Summary2$AnnualRain),500, f=ceiling)),
       xaxt="n",
       xlab = " ", ylab = "  ",cex.axis=axLFS,
       las = 1,lwd=lineWidth,col = "blue",main = "(a)")
  lines(years, Summary2.5$AnnualRain,# subset=!is.na(Summary2.5$AnnualRain),
        col='red', lwd=lineWidth)
  #x axis labels
  axis(1, at=seq(start_year, end_year, 3),las=2, cex.axis=axLFS)
  #x axis ticks
  axis(1, at=years, labels=FALSE)
  
  title(xlab = "Time (in years)",line = 4.2,cex.lab=axTFS)
  title(ylab = "Annual Rainfall (mm)",line = 4.0,cex.lab=axTFS)
  
  legend(x="topright", legend=c("Filled", "Unfilled"),
         lty=c(1,1), col=c("blue", "red"), lwd=lineWidth, cex=legFS)
  
  # #Plots Seasonal vs Year
  # plot(years, Summary3$SeasonalRain,
  #      type="l",ylim=c(0,plyr::round_any(max(Summary3$SeasonalRain),500, f=ceiling)),
  #      xaxt="n",
  #      xlab = " ", ylab = "  ",cex.axis=1.1,
  #      las = 1,lwd=lineWidth,col = "blue",main = "(b)")
  # lines(years, Summary3.5$SeasonalRain,subset=!is.na(Summary2.5$AnnualRain),
  #       col='red', lwd=lineWidth)
  # #x axis labels
  # axis(1, at=seq(start_year, end_year, 3),las=2, cex.axis=1.1)
  # #x axis ticks
  # axis(1, at=years, labels=FALSE)
  # 
  # title(xlab = "Time(in years)",line = 4.2,cex.lab=1.2)
  # title(ylab = "Seasonal Rainfall (mm)",line = 3.3,cex.lab=1.2)
  
  # df<-na.omit(df)
  # #Plots Rainfall vs Days
  # plot(df$Date, df$Rainfall,
  #      type = "l", xaxt="n",
  #      xlab = " ", ylab = " ",cex.axis=1.1,
  #      las = 1,col = "blue
  #      ",main = "(c)")
  # lines(df$Date, df$Rainfall, 
  #       col="red")
  #X axis labels
  # axis.Date(1, at=labelDates,
  #           format = "%Y-%m-%d",las=2,cex.axis=1.1)
  # #x axis ticks
  # axis.Date(1, at=tickDates, labels=FALSE)
            
  # title(xlab = "Time(in days)",line = 6.2,cex.lab=1.2)
  # 
  # #Shifts Y-Axis Labels as desired
  # title(ylab = "Daily Rainfall (mm)",line = 3.3,cex.lab=1.2)
  # 
  # #Plots Cumulative Rainfall vs Days
  # plot(mydata2$Date, mydata2$cum,
  #      type = "l",xaxt="n",
  #      xlab = " ", ylab = " ",cex.axis=1.1,
  #      las = 1,col = "blue",main = "(b)")
  # 
  # #X axis labels
  # axis.Date(1, at=labelDates,
  #           format = "%Y-%m-%d",las=2,cex.axis=1.1)
  # #x axis ticks
  # axis.Date(1, at=tickDates, labels=FALSE)
  # 
  # title(xlab = "Time(in days)",line = 6.2,cex.lab=1.2)
  # 
  # reg1 <- lm(mydata2$cum ~ mydata2$Date) 
  # reg2 <- lm(mydata2$cum*1000 ~ mydata2$Date)
  # 
  # coeff=coefficients(reg2)
  # coeff1 <- round(coeff[1],1)
  # coeff2 <- round(coeff[2],1)
  # 
  # a <- sign(round(coeff[1],1))
  # if (a == -1){
  #   coeff1 <- coeff1*-1
  #   sgn = "-"
  # }else{
  #   sgn = "+"
  # }
  # 
  # #eq = paste0("y = ",round(coeff[2],1)," * x ",sgn, round(coeff[1],1))
  # #r2 = paste0(c(("R2 = ")) , round(summary(reg1)$r.squared,digits=3))
  # #r2 = as.character(round(summary(reg1)$r.squared,digits=3))
  # #r2.exp = expression(paste("",R^2, "=" ))
  # 
  # modsum = summary(reg2)
  # r2 = modsum$adj.r.squared
  # mylabel = as.expression(bquote(italic(R)^2 == .(format(r2,digits = 3))))
  # #text(x=2,y=30,labels = mylabel)
  # 
  # 
  # l <- as.expression(bquote("y"== ~.(format(coeff2))~ "* x" ~.(format(sgn)) ~ .(format(coeff1))) )
  # #text(x=2,y=,labels=l)
  # 
  # legend("topleft",legend =c(l,mylabel), bty ="n")
  # 
  # abline(reg1,col="black",lty=2,lwd=1.5)
  # 
  # 
  # #Shifts Y-Axis Labels as desired
  # title(ylab = "Cum. Rainfall in thousands (mm)",line = 3.3,cex.lab=1.2)
  
  #Plots Monthly Maximum vs Month

  barplot(MonthlyRain, beside=TRUE,
          col= c("blue", "red"),
          ylab ="Monthly Rainfall (mm)",
          xlab = " ",
          las=2,
          ylim=c(0,plyr::round_any(max(MonthlyRain),100, f=ceiling)),
          axes=TRUE,cex.axis = axLFS,cex.names=axTFS, cex.lab=axTFS,
          main = "(b)",
          space = c(0,2)
  )

  title(xlab = "Time (in months)",line = 4.2,cex.lab=axTFS)
  legend(x="topright", legend=c("Filled", "Unfilled"),
         fill=c("blue", "red"), border="black", cex=legFS)
  box()
  
  mtext(paste(StationName[i],"(","Index :",StationNumber[i],")"),
        outer=TRUE,cex=cTFS)
  
  
  dev.off()
  print(paste(StationNumber[i], StationName[i], 'executed.'))
  break
}

