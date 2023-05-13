rm(list=ls())

library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(readxl)
library(lubridate)



refpath <- "C:/Users/Dell/Desktop/New Climate Analysis Data/Precipitation/"
savepath1 <- "C:/Users/Dell/Desktop/Climate Projection Plots/Trial/Precipitation/Monthly/"
savepath2 <- "C:/Users/Dell/Desktop/Climate Projection Plots/Trial/Precipitation/Annual/"

modelname <- c("SSP 245", "SSP 585")

excelpath <- "C:/Users/Dell/Desktop/MultiModel Ensemble/Precipitation Data Available.xlsx"
excelfile <- readxl::read_excel(excelpath,col_names = TRUE)

Year_Month <- function(df){
  df[,2] <- round(df[,2],1)
  df[,1]  <- as.Date(df[[1]]) 
  df$Year <- format(df$Date,"%Y")
  df$Month <- format(df$Date,"%m")
  return(df)
}

#expandy = function(vec, ymin=NULL) {

# max.val = max(vec, na.rm=TRUE)
#min.log = floor(log10(max.val))

#expand_limits(y=c(ymin, ceiling(max.val/10^min.log)*10^min.log))
#}
roundUP <- function(x, roundTo, dir ) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}
#E.g.First all Rain values of Jan (from all years: 2015-2100) are grouped together..
#Then Rain values of Jan are again grouped based on year i.e 1 group of Rain of Jan of 2015,
#another group of Rain of Jan of 2016
#When first summarise is called, Rain of Jan 2015 is summed together, Rain of Jan 2016 is summed together and so on 
#When second summarise is called, Summed Rain of Jan 2015, 2016 upto 2100 is averaged
Summary <- function(df,modelname,name){
  df <- df %>% group_by(Month,Year) %>% summarise(Monthly_Sum = sum(Rain)) %>%
    summarise(Average_Rain = mean(Monthly_Sum))
  df$MonthName <- month.abb
  colnames(df)[2] <- modelname
  df$Period <- name
  return(df)
} 

Summary1 <- function(df,modelname,name){
  df <- df %>% group_by(Year) %>% summarise(AnnualRain = sum(Rain)) 
  colnames(df)[2] <- modelname
  df$Period <- name
  return(df)
} 

for (n in 1:66){   #66
  for (i in 1:length(modelname)){  #
    path21_45 <- paste0(refpath,modelname[i],"/2021-2045/")
    filepath21_45 <- list.files(path21_45,full.names=TRUE)
    path46_70 <-  paste0(refpath,modelname[i],"/2046-2070/")
    filepath46_70 <- list.files(path46_70,full.names=TRUE)
    path71_00 <- paste0(refpath,modelname[i],"/2071-2100/")
    filepath71_00 <- list.files(path71_00,full.names=TRUE)
    
    for (j in n:n){ #length(filepath)
      df21_45 <- read_excel(filepath21_45[j],col_names =  TRUE)
      df21_45 <- Year_Month(df21_45)
      
      df46_70 <- read_excel(filepath46_70[j],col_names =  TRUE)
      df46_70 <- Year_Month(df46_70)
      
      df71_00 <- read_excel(filepath71_00[j],col_names =  TRUE)
      df71_00 <- Year_Month(df71_00)
      
      if(j<=59){
        stationnum <- substr(list.files(path21_45)[j],1,4)
      }
      else{
        stationnum <- substr(list.files(path21_45)[j],1,3)
      }
    }
    
    stationname <- excelfile[n,1]
    
    #Monthly
    Melteddf21_45 <- reshape2::melt(Summary(df21_45,modelname[i],"Near-Future"),id.vars=c("Month","MonthName","Period"))
    Melteddf46_70 <- reshape2::melt(Summary(df46_70,modelname[i],"Mid-Future"),id.vars=c("Month","MonthName","Period"))
    Melteddf71_00 <- reshape2::melt(Summary(df71_00,modelname[i],"Far-Future"),id.vars=c("Month","MonthName","Period"))
    
    Melteddf <- rbind(Melteddf21_45,Melteddf46_70,Melteddf71_00)
    colnames(Melteddf)[4] <- "Model"
    colnames(Melteddf)[5] <- "AvgRain"
    Melteddf$Station <- stationnum
    
    #Annual
    Melteddf121_45 <- reshape2::melt(Summary1(df21_45,modelname[i],"Near-Future"),id.vars=c("Year","Period"))
    Melteddf146_70 <- reshape2::melt(Summary1(df46_70,modelname[i],"Mid-Future"),id.vars=c("Year","Period"))
    Melteddf171_00 <- reshape2::melt(Summary1(df71_00,modelname[i],"Far-Future"),id.vars=c("Year","Period"))
    
    Melteddf1 <- rbind(Melteddf121_45,Melteddf146_70,Melteddf171_00)
    colnames(Melteddf1)[3] <- "Model"
    colnames(Melteddf1)[4] <- "AvgRain"
    Melteddf1$Station <- stationnum
    
    
    if (i==1){
      df1 <- Melteddf
      df2 <- Melteddf1
    }
    else{
      df1 <- rbind(df1,Melteddf)
      df2 <- rbind(df2,Melteddf1)
    }
    
  }
  df1$MonthName<- factor(df1$MonthName,levels=month.abb,ordered=TRUE)
  #factor the cmonth names
  
  plottitle <- paste0(stationname," ","(Index: ",stationnum,")")
  
  #yupper <- roundUP(max(df1$AvgRain),100)
  yupper <- roundUP(max(df2$AvgRain),500,1)
  ylower <- roundUP(min(df2$AvgRain),500,0)
 
  
  A <- ggplot(df1,aes(x=MonthName,y=AvgRain)) + 
    geom_bar(stat='identity', fill="forest green",width=0.5)+   #create bar plot stat="identity" tells ggplot2 to skip the aggregation and that you'll provide the y values
    facet_grid(Model~factor(Period,levels = c("Near-Future","Mid-Future","Far-Future")),scales="free_y" )+     #Create gridded plot wih Models in x direction, no of column restricts no of plot in a row, scale=free_y allows yaxis scale to adjust freely according to data 
    ggtitle(plottitle)+                             #plot title at the top
    theme(axis.title.x=element_text(hjust=0.5, vjust=1,size=20,lineheight = 0.5),   #customize x axis label 
          axis.title.y=element_text(hjust=0.5,vjust=1,size=20,lineheight=0.5),     #customize y axis label
          plot.title=element_text(hjust=0.5,size=20,lineheight=0.5),               #customize plot title
          axis.text.x =element_text(angle=90,hjust=0.5, vjust=1,size=17,lineheight = 0.2), #customize x-axis tick label
          axis.text.y =element_text(hjust=0.5, vjust=1,size=17,lineheight = 0.2), #customize y-axis tick label
          strip.text=element_text(size=17))+    #customize facet_wrap Title( i.e Model name displayed)
        labs(x="Months", y="Average Monthly Rainfall (mm)")
  #coord_cartesian(ylim= c(0,yupper))+
  
  #coord_cartesian(expand=FALSE)
  ggsave(filename=paste0(stationnum,".png"),A, device=png,
        savepath1, width=12, height=8)
 
  
  B <- ggplot(df2,aes(x=as.numeric(Year),y=AvgRain)) + 
    geom_line(size=1)+geom_smooth(method=lm)+
    facet_grid(Model~factor(Period,levels = c("Near-Future","Mid-Future","Far-Future")),scales="free" )+      #Create gridded plot wih Models in x direction, no of column restricts no of plot in a row, scale=free_y allows yaxis scale to adjust freely according to data 
    ggtitle(plottitle)+                             #plot title at the top
    #scale_x_continuous(breaks = seq(2015,2100,4),expand=c(0.02,0.02))+
    theme(axis.title.x=element_text(hjust=0.5, vjust=1,size=20,lineheight = 0.5),   #customize x axis label 
          axis.title.y=element_text(hjust=0.5,vjust=1,size=20,lineheight=0.5),     #customize y axis label
          plot.title=element_text(hjust=0.5,size=20,lineheight=0.5),               #customize plot title
          axis.text.x =element_text(angle=90,hjust=0.5, vjust=1,size=17), #customize x-axis tick label
          axis.text.y =element_text(hjust=0.5, vjust=1,size=17,lineheight = 0.2), #customize y-axis tick label
          strip.text=element_text(size=17))+                                  #customize facet_wrap Title( i.e Model name displayed)
    labs(x="Years", y="Annual Rainfall (mm)")+
    coord_cartesian(ylim= c(ylower,yupper))
   ggsave(filename=paste0(stationnum,".png"),B, device=png,
        savepath2, width=12, height=8)
}