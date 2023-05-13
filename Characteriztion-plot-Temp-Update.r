rm(list=ls())

library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(readxl)

refpath1 <- "C:/Users/Dell/Desktop/New Climate Analysis Data/Temperature/Tmax/"
refpath2 <- "C:/Users/Dell/Desktop/New Climate Analysis Data/Temperature/Tmin/"
savepath1 <- "C:/Users/Dell/Desktop/Climate Projection Plots/Trial/Temperature/Monthly/"
savepath2 <- "C:/Users/Dell/Desktop/Climate Projection Plots/Trial/Temperature/Annual/"

modelname <- c("SSP 245", "SSP 585")

excelpath <- "C:/Users/Dell/Desktop/MultiModel Ensemble/Temperature Data Available.xlsx"
excelfile <- readxl::read_excel(excelpath,col_names = TRUE,sheet="Simulated")

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
  df <- df %>% group_by(Month,Year) %>% summarise(Monthly_Average = mean(T)) %>%
    summarise(Average_Temp = mean(Monthly_Average))
  df$MonthName <- month.abb
  colnames(df)[2] <- modelname
  df$Period <- name
  return(df)
} 

Summary1 <- function(df,modelname,name){
  df <- df %>% group_by(Year) %>% summarise(AnnualTemp = mean(T)) 
  colnames(df)[2] <- modelname
  df$Period <- name
  return(df)
} 


for (n in 1:26){  #26
  for (i in 1:length(modelname)){
    path21_45max <- paste0(refpath1,modelname[i],"/2021-2045/")
    filepath21_45max <- list.files(path21_45max,full.names=TRUE)
    path46_70max <-  paste0(refpath1,modelname[i],"/2046-2070/")
    filepath46_70max <- list.files(path46_70max,full.names=TRUE)
    path71_00max <- paste0(refpath1,modelname[i],"/2071-2100/")
    filepath71_00max <- list.files(path71_00max,full.names=TRUE)
    
    path21_45min <- paste0(refpath2,modelname[i],"/2021-2045/")
    filepath21_45min <- list.files(path21_45min,full.names=TRUE)
    path46_70min <-  paste0(refpath2,modelname[i],"/2046-2070/")
    filepath46_70min <- list.files(path46_70min,full.names=TRUE)
    path71_00min <- paste0(refpath2,modelname[i],"/2071-2100/")
    filepath71_00min <- list.files(path71_00min,full.names=TRUE)
    
    for (j in n:n){ #length(filepath)
      df21_45max <- read_excel(filepath21_45max[j],col_names =  TRUE)
      colnames(df21_45max)[2] <- "T"
      df21_45max <- Year_Month(df21_45max)
      
      df46_70max <- read_excel(filepath46_70max[j],col_names =  TRUE)
      colnames(df46_70max)[2] <- "T"
      df46_70max <- Year_Month(df46_70max)
      
      df71_00max <- read_excel(filepath71_00max[j],col_names =  TRUE)
      colnames(df71_00max)[2] <- "T"
      df71_00max <- Year_Month(df71_00max)
      
      df21_45min <- read_excel(filepath21_45min[j],col_names =  TRUE)
      colnames(df21_45min)[2] <- "T"
      df21_45min <- Year_Month(df21_45min)
      
      df46_70min <- read_excel(filepath46_70min[j],col_names =  TRUE)
      colnames(df46_70min)[2] <- "T"
      df46_70min <- Year_Month(df46_70min)
      
      df71_00min <- read_excel(filepath71_00min[j],col_names =  TRUE)
      colnames(df71_00min)[2] <- "T"
      df71_00min <- Year_Month(df71_00min)
      #dfmax <- read.csv(filepathmax[j],header = TRUE)
      #colnames(dfmax)[2] <- "T"
      #dfmax <- Year_Month(dfmax)
      
      #dfmin <- read.csv(filepathmin[j],header = TRUE)
      #colnames(dfmin)[2] <- "T"
      #dfmin <- Year_Month(dfmin)
      
      
      if(j<=24){
        stationnum <- substr(list.files(path21_45max)[j],1,4)
      }
      else{
        stationnum <- substr(list.files(path21_45max)[j],1,3)
      }
    }
    
    stationname <- excelfile[n,1]
    
    #Monthly -> 1
    Melteddf21_45max <- reshape2::melt(Summary(df21_45max,modelname[i],"Near-Future"),id.vars=c("Month","MonthName","Period"))
    Melteddf46_70max <- reshape2::melt(Summary(df46_70max,modelname[i],"Mid-Future"),id.vars=c("Month","MonthName","Period"))
    Melteddf71_00max <- reshape2::melt(Summary(df71_00max,modelname[i],"Far-Future"),id.vars=c("Month","MonthName","Period"))
    
    Melteddfmax <- rbind(Melteddf21_45max,Melteddf46_70max,Melteddf71_00max)
    colnames(Melteddfmax)[4] <- "Model"
    colnames(Melteddfmax)[5] <- "Tmax"
    Melteddfmax$Station <- stationnum
    
    
    Melteddf21_45min <- reshape2::melt(Summary(df21_45min,modelname[i],"Near-Future"),id.vars=c("Month","MonthName","Period"))
    Melteddf46_70min <- reshape2::melt(Summary(df46_70min,modelname[i],"Mid-Future"),id.vars=c("Month","MonthName","Period"))
    Melteddf71_00min <- reshape2::melt(Summary(df71_00min,modelname[i],"Far-Future"),id.vars=c("Month","MonthName","Period"))
    
    Melteddfmin <- rbind(Melteddf21_45min,Melteddf46_70min,Melteddf71_00min)
    colnames(Melteddfmin)[4] <- "Model"
    colnames(Melteddfmin)[5] <- "Tmin"
    Melteddfmin$Station <- stationnum
    
    Melteddfmonth <- Melteddfmax
    Melteddfmonth$Tmin <- Melteddfmin$Tmin
    
    Melteddfmonth <- reshape2::melt(Melteddfmonth,id.vars=c("Month","MonthName","Model","Station","Period"))
    
    Melteddfmonth$MonthName<-factor(Melteddfmonth$MonthName,levels=month.abb,ordered=TRUE)
    
    #Annual -> 2
    Melteddf121_45max <- reshape2::melt(Summary1(df21_45max,modelname[i],"Near-Future"),id.vars=c("Year","Period"))
    Melteddf146_70max <- reshape2::melt(Summary1(df46_70max,modelname[i],"Mid-Future"),id.vars=c("Year","Period"))
    Melteddf171_00max <- reshape2::melt(Summary1(df71_00max,modelname[i],"Far-Future"),id.vars=c("Year","Period"))
    
    Melteddfmax1 <- rbind(Melteddf121_45max,Melteddf146_70max,Melteddf171_00max)
    colnames(Melteddfmax1)[3] <- "Model"
    colnames(Melteddfmax1)[4] <- "Tmax"
    Melteddfmax1$Station <- stationnum
    
    
    Melteddf121_45min <- reshape2::melt(Summary1(df21_45min,modelname[i],"Near-Future"),id.vars=c("Year","Period"))
    Melteddf146_70min <- reshape2::melt(Summary1(df46_70min,modelname[i],"Mid-Future"),id.vars=c("Year","Period"))
    Melteddf171_00min <- reshape2::melt(Summary1(df71_00min,modelname[i],"Far-Future"),id.vars=c("Year","Period"))
    
    Melteddfmin1 <- rbind(Melteddf121_45min,Melteddf146_70min,Melteddf171_00min)
    colnames(Melteddfmin1)[3] <- "Model"
    colnames(Melteddfmin1)[4] <- "Tmin"
    Melteddfmin1$Station <- stationnum
    
    
    Melteddfannual <- Melteddfmax1
    Melteddfannual$Tmin <- Melteddfmin1$Tmin
    
    Melteddfannual <- reshape2::melt(Melteddfannual,id.vars=c("Year","Model","Station","Period"))
    
    plottitle <- paste0(stationname," ","(Index: ",stationnum,")")
    
    
    if (i==1){
      dfmonth <- Melteddfmonth
      dfannual <- Melteddfannual
    }
    else{
      dfmonth <- rbind(dfmonth,Melteddfmonth)
      dfannual <- rbind(dfannual,Melteddfannual)
      
    }
    
    
  
    
    A<- ggplot(dfmonth, aes(x=MonthName, y=value, fill=variable)) +
      geom_bar(stat='identity', position=position_dodge2(width=1,padding=0.2),width=0.6)+ # position = "dodge" presents stacking of bars
      facet_grid(Model~factor(Period,levels = c("Near-Future","Mid-Future","Far-Future")),scales="free_y" )+     #Create gridded plot wih Models in x direction, no of column restricts no of plot in a row, scale=free_y allows yaxis scale to adjust freely according to data 
      ggtitle(plottitle)+                             #plot title at the top
      theme(axis.title.x=element_text(hjust=0.5, vjust=1,size=16,lineheight = 0.5),   #customize x axis label 
            axis.title.y=element_text(hjust=0.5,vjust=1,size=16,lineheight=0.5),     #customize y axis label
            plot.title=element_text(hjust=0.5,size=20,lineheight=0.5),               #customize plot title
            axis.text.x =element_text(angle=90,hjust=0.5, vjust=1,size=16,lineheight = 0.2), #customize x-axis tick label
            axis.text.y =element_text(hjust=0.5, vjust=1,size=16,lineheight = 0.2), #customize y-axis tick label
            strip.text=element_text(size=16),
            legend.title = element_blank(),
            legend.text = element_text(size=16))+                                   #customize facet_wrap Title( i.e Model name displayed)
      labs(x="Months", y="Average Monthly Temperature (°C)")
    
    ggsave(filename=paste0(stationnum,".png"),A, device=png,
           savepath1, width=12, height=8)
    
    B<- ggplot(dfannual, aes(x=as.numeric(Year), y=value, color=variable)) +
      geom_line(size=1)+geom_smooth(method=lm,size=1,lty=6)+
      facet_grid(Model~factor(Period,levels = c("Near-Future","Mid-Future","Far-Future")),scales="free" )+
      ggtitle(plottitle)+ 
      #scale_x_continuous(breaks = seq(2015,2100,5),expand=c(0.02,0.02))+#plot title at the top
      theme(axis.title.x=element_text(hjust=0.5, vjust=1,size=20,lineheight = 0.5),   #customize x axis label 
            axis.title.y=element_text(hjust=0.5,vjust=1,size=20,lineheight=0.5),     #customize y axis label
            plot.title=element_text(hjust=0.5,size=20,lineheight=0.5),               #customize plot title
            axis.text.x =element_text(angle=90,hjust=0.5, vjust=1,size=18,lineheight = 0.2), #customize x-axis tick label
            axis.text.y =element_text(hjust=0.5, vjust=1,size=18,lineheight = 0.2), #customize y-axis tick label
            strip.text=element_text(size=16),
            legend.title = element_blank(),
            legend.text = element_text(size=16))+                                  #customize facet_wrap Title( i.e Model name displayed)
      labs(x="Years", y="Average Annual Temperature (°C)")
    
    ggsave(filename=paste0(stationnum,".png"),B, device=png,
           savepath2, width=12, height=8)
  }
}