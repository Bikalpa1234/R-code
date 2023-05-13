rm(list=ls())

library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)
library(gridExtra)
library(cowplot)

refpath1 <-"C:/Users/Dell/Desktop/New Climate Analysis Data/Temperature/Tmax/"
refpath2 <-"C:/Users/Dell/Desktop/New Climate Analysis Data/Temperature/Tmin/" 
savepath <- "C:/Users/Dell/Desktop/Climate Projection Plots/Characterization/Temperature/Seasonal/"


#modelname <- c("SSP 245", "SSP 585")

excelpath <- "C:/Users/Dell/Desktop/MultiModel Ensemble/Temperature Data Available.xlsx"
excelfile <- readxl::read_excel(excelpath,col_names = TRUE,sheet="Simulated")

Year_Month <- function(df){
  df[,2] <- round(df[,2],1)
  df[,1]  <- as.Date(df[[1]]) 
  df$Year <- format(df$Date,"%Y")
  df$Month <- format(df$Date,"%m")
  return(df)
}

Winter <- function(df,modelname,name){
  Winterdata <- df%>% dplyr::filter( Month == "01" | Month == "02"  | Month == "12" ) %>%
    group_by(Year) %>%
    summarise(Winter=mean(T))
  Winterdata$Model <- modelname
  Winterdata$Period <- name
  return(Winterdata)
  
}

PreMonsoon <- function(df,modelname,name){
  PreMonsoondata <- df%>% dplyr::filter( Month == "03"  | Month == "04"  | Month == "05" ) %>%
    group_by(Year) %>%
    summarise(PreMonsoon=mean(T))
  PreMonsoondata$Model <- modelname
  PreMonsoondata$Period <- name
  return(PreMonsoondata)
}

Monsoon <- function(df,modelname,name){
  Monsoondata <- df%>% dplyr::filter( Month == "06"  | Month == "07"  | Month == "08" | Month =="09" ) %>%
    group_by(Year) %>%
    summarise(Monsoon=mean(T))
  Monsoondata$Model <- modelname
  Monsoondata$Period <- name
  return(Monsoondata)
}

PostMonsoon <- function(df,modelname,name){
  PostMonsoondata <- df%>% dplyr::filter( Month == "10"  | Month == "11"  ) %>%
    group_by(Year) %>%
    summarise(PostMonsoon=mean(T))
  PostMonsoondata$Model <- modelname
  PostMonsoondata$Period <- name
  return(PostMonsoondata)
}

for(n in 1:26){#26
  #for (i in 1:length(modelname)){ 
  #SSP 245
  path21_45SSP245max <- paste0(refpath1,"SSP 245","/2021-2045/")
  filepath21_45SSP245max <- list.files(path21_45SSP245max,full.names=TRUE)
  path46_70SSP245max <-  paste0(refpath1,"SSP 245","/2046-2070/")
  filepath46_70SSP245max <- list.files(path46_70SSP245max,full.names=TRUE)
  path71_00SSP245max <- paste0(refpath1,"SSP 245","/2071-2100/")
  filepath71_00SSP245max <- list.files(path71_00SSP245max,full.names=TRUE)
  
  path21_45SSP245min <- paste0(refpath2,"SSP 245","/2021-2045/")
  filepath21_45SSP245min <- list.files(path21_45SSP245min,full.names=TRUE)
  path46_70SSP245min <-  paste0(refpath2,"SSP 245","/2046-2070/")
  filepath46_70SSP245min <- list.files(path46_70SSP245min,full.names=TRUE)
  path71_00SSP245min <- paste0(refpath2,"SSP 245","/2071-2100/")
  filepath71_00SSP245min <- list.files(path71_00SSP245min,full.names=TRUE)
  
  #SSP 585
  path21_45SSP585max <- paste0(refpath1,"SSP 585","/2021-2045/")
  filepath21_45SSP585max <- list.files(path21_45SSP585max,full.names=TRUE)
  path46_70SSP585max <-  paste0(refpath1,"SSP 585","/2046-2070/")
  filepath46_70SSP585max <- list.files(path46_70SSP585max,full.names=TRUE)
  path71_00SSP585max <- paste0(refpath1,"SSP 585","/2071-2100/")
  filepath71_00SSP585max <- list.files(path71_00SSP585max,full.names=TRUE)
  
  path21_45SSP585min <- paste0(refpath2,"SSP 585","/2021-2045/")
  filepath21_45SSP585min <- list.files(path21_45SSP585min,full.names=TRUE)
  path46_70SSP585min <-  paste0(refpath2,"SSP 585","/2046-2070/")
  filepath46_70SSP585min <- list.files(path46_70SSP585min,full.names=TRUE)
  path71_00SSP585min <- paste0(refpath2,"SSP 585","/2071-2100/")
  filepath71_00SSP585min <- list.files(path71_00SSP585min,full.names=TRUE)
  
  for (j in n:n){  #length(filepath)
    #SSP 245
    #Tmax
    df21_45SSP245max <- read_excel(filepath21_45SSP245max[j],col_names =  TRUE)
    colnames(df21_45SSP245max)[2] <- "T"
    df21_45SSP245max <- Year_Month(df21_45SSP245max)
    
    df46_70SSP245max <- read_excel(filepath46_70SSP245max[j],col_names =  TRUE)
    colnames(df46_70SSP245max)[2] <- "T"
    df46_70SSP245max <- Year_Month(df46_70SSP245max)
    
    df71_00SSP245max <- read_excel(filepath71_00SSP245max[j],col_names =  TRUE)
    colnames(df71_00SSP245max)[2] <- "T"
    df71_00SSP245max <- Year_Month(df71_00SSP245max)
    
    #Tmin
    df21_45SSP245min <- read_excel(filepath21_45SSP245min[j],col_names =  TRUE)
    colnames(df21_45SSP245min)[2] <- "T"
    df21_45SSP245min <- Year_Month(df21_45SSP245min)
    
    df46_70SSP245min <- read_excel(filepath46_70SSP245min[j],col_names =  TRUE)
    colnames(df46_70SSP245min)[2] <- "T"
    df46_70SSP245min <- Year_Month(df46_70SSP245min)
    
    df71_00SSP245min <- read_excel(filepath71_00SSP245min[j],col_names =  TRUE)
    colnames(df71_00SSP245min)[2] <- "T"
    df71_00SSP245min <- Year_Month(df71_00SSP245min)
    
    
    #SSP 585
    #Tmax
    df21_45SSP585max <- read_excel(filepath21_45SSP585max[j],col_names =  TRUE)
    colnames(df21_45SSP585max)[2] <- "T"
    df21_45SSP585max <- Year_Month(df21_45SSP585max)
    
    df46_70SSP585max <- read_excel(filepath46_70SSP585max[j],col_names =  TRUE)
    colnames(df46_70SSP585max)[2] <- "T"
    df46_70SSP585max <- Year_Month(df46_70SSP585max)
    
    df71_00SSP585max <- read_excel(filepath71_00SSP585max[j],col_names =  TRUE)
    colnames(df71_00SSP585max)[2] <- "T"
    df71_00SSP585max <- Year_Month(df71_00SSP585max)
    
    #Tmin
    df21_45SSP585min <- read_excel(filepath21_45SSP585min[j],col_names =  TRUE)
    colnames(df21_45SSP585min)[2] <- "T"
    df21_45SSP585min <- Year_Month(df21_45SSP585min)
    
    df46_70SSP585min <- read_excel(filepath46_70SSP585min[j],col_names =  TRUE)
    colnames(df46_70SSP585min)[2] <- "T"
    df46_70SSP585min <- Year_Month(df46_70SSP585min)
    
    df71_00SSP585min <- read_excel(filepath71_00SSP585min[j],col_names =  TRUE)
    colnames(df71_00SSP585min)[2] <- "T"
    df71_00SSP585min <- Year_Month(df71_00SSP585min)
    
    if(j<=24){
      stationnum <- substr(list.files(path21_45SSP245max)[j],1,4)
    }
    else{
      stationnum <- substr(list.files(path21_45SSP245max)[j],1,3)
    }
  }
    
  stationname <- excelfile[n,1]
  
  #SSP 245
  #Winter
  #Tmax
  Winterdata21_45SSP245max <- Winter(df21_45SSP245max,"SSP 245","Near-Future")
  Winterdata46_70SSP245max <- Winter(df46_70SSP245max,"SSP 245","Mid-Future")
  Winterdata71_00SSP245max <- Winter(df71_00SSP245max,"SSP 245","Far-Future")
  
  #Tmin
  Winterdata21_45SSP245min <- Winter(df21_45SSP245min,"SSP 245","Near-Future")
  Winterdata46_70SSP245min <- Winter(df46_70SSP245min,"SSP 245","Mid-Future")
  Winterdata71_00SSP245min <- Winter(df71_00SSP245min,"SSP 245","Far-Future")
  
  
  WinterdataSSP245max <- rbind(Winterdata21_45SSP245max,Winterdata46_70SSP245max,Winterdata71_00SSP245max)
  
  MeltWinterSSP245max <- melt(WinterdataSSP245max,id.vars=c("Year","Model","Period"))
  colnames(MeltWinterSSP245max)[4] <- "Season"
  colnames(MeltWinterSSP245max)[5] <- "Tmax"
  
  
  WinterdataSSP245min <- rbind(Winterdata21_45SSP245min,Winterdata46_70SSP245min,Winterdata71_00SSP245min)
  
  MeltWinterSSP245min <- melt(WinterdataSSP245min,id.vars=c("Year","Model","Period"))
  colnames(MeltWinterSSP245min)[4] <- "Season"
  colnames(MeltWinterSSP245min)[5] <- "Tmin"
  
  MeltWinterSSP245max$Tmin <- MeltWinterSSP245min$Tmin
  
  MeltWinterSSP245 <- reshape2::melt(MeltWinterSSP245max,id.vars=c("Year","Model","Season","Period"))
  
  #PreMonsoon
  #Tmax
  PreMonsoondata21_45SSP245max <- PreMonsoon(df21_45SSP245max,"SSP 245","Near-Future")
  PreMonsoondata46_70SSP245max <- PreMonsoon(df46_70SSP245max,"SSP 245","Mid-Future")
  PreMonsoondata71_00SSP245max <- PreMonsoon(df71_00SSP245max,"SSP 245","Far-Future")
  
  #Tmin
  PreMonsoondata21_45SSP245min <- PreMonsoon(df21_45SSP245min,"SSP 245","Near-Future")
  PreMonsoondata46_70SSP245min <- PreMonsoon(df46_70SSP245min,"SSP 245","Mid-Future")
  PreMonsoondata71_00SSP245min <- PreMonsoon(df71_00SSP245min,"SSP 245","Far-Future")
  
  
  PreMonsoondataSSP245max <- rbind(PreMonsoondata21_45SSP245max,PreMonsoondata46_70SSP245max,PreMonsoondata71_00SSP245max)
  
  MeltPreMonsoonSSP245max <- melt(PreMonsoondataSSP245max,id.vars=c("Year","Model","Period"))
  colnames(MeltPreMonsoonSSP245max)[4] <- "Season"
  colnames(MeltPreMonsoonSSP245max)[5] <- "Tmax"
  
  
  PreMonsoondataSSP245min <- rbind(PreMonsoondata21_45SSP245min,PreMonsoondata46_70SSP245min,PreMonsoondata71_00SSP245min)
  
  MeltPreMonsoonSSP245min <- melt(PreMonsoondataSSP245min,id.vars=c("Year","Model","Period"))
  colnames(MeltPreMonsoonSSP245min)[4] <- "Season"
  colnames(MeltPreMonsoonSSP245min)[5] <- "Tmin"
  
  MeltPreMonsoonSSP245max$Tmin <- MeltPreMonsoonSSP245min$Tmin
  
  MeltPreMonsoonSSP245 <- reshape2::melt(MeltPreMonsoonSSP245max,id.vars=c("Year","Model","Season","Period"))
  
  #Monsoon
  #Tmax
  Monsoondata21_45SSP245max <- Monsoon(df21_45SSP245max,"SSP 245","Near-Future")
  Monsoondata46_70SSP245max <- Monsoon(df46_70SSP245max,"SSP 245","Mid-Future")
  Monsoondata71_00SSP245max <- Monsoon(df71_00SSP245max,"SSP 245","Far-Future")
  
  #Tmin
  Monsoondata21_45SSP245min <- Monsoon(df21_45SSP245min,"SSP 245","Near-Future")
  Monsoondata46_70SSP245min <- Monsoon(df46_70SSP245min,"SSP 245","Mid-Future")
  Monsoondata71_00SSP245min <- Monsoon(df71_00SSP245min,"SSP 245","Far-Future")
  
  
  MonsoondataSSP245max <- rbind(Monsoondata21_45SSP245max,Monsoondata46_70SSP245max,Monsoondata71_00SSP245max)
  
  MeltMonsoonSSP245max <- melt(MonsoondataSSP245max,id.vars=c("Year","Model","Period"))
  colnames(MeltMonsoonSSP245max)[4] <- "Season"
  colnames(MeltMonsoonSSP245max)[5] <- "Tmax"
  
  
  MonsoondataSSP245min <- rbind(Monsoondata21_45SSP245min,Monsoondata46_70SSP245min,Monsoondata71_00SSP245min)
  
  MeltMonsoonSSP245min <- melt(MonsoondataSSP245min,id.vars=c("Year","Model","Period"))
  colnames(MeltMonsoonSSP245min)[4] <- "Season"
  colnames(MeltMonsoonSSP245min)[5] <- "Tmin"
  
  MeltMonsoonSSP245max$Tmin <- MeltMonsoonSSP245min$Tmin
  
  MeltMonsoonSSP245 <- reshape2::melt(MeltMonsoonSSP245max,id.vars=c("Year","Model","Season","Period"))
  
  #PostMonsoon
  #Tmax
  PostMonsoondata21_45SSP245max <- PostMonsoon(df21_45SSP245max,"SSP 245","Near-Future")
  PostMonsoondata46_70SSP245max <- PostMonsoon(df46_70SSP245max,"SSP 245","Mid-Future")
  PostMonsoondata71_00SSP245max <- PostMonsoon(df71_00SSP245max,"SSP 245","Far-Future")
  
  #Tmin
  PostMonsoondata21_45SSP245min <- PostMonsoon(df21_45SSP245min,"SSP 245","Near-Future")
  PostMonsoondata46_70SSP245min <- PostMonsoon(df46_70SSP245min,"SSP 245","Mid-Future")
  PostMonsoondata71_00SSP245min <- PostMonsoon(df71_00SSP245min,"SSP 245","Far-Future")
  
  
  PostMonsoondataSSP245max <- rbind(PostMonsoondata21_45SSP245max,PostMonsoondata46_70SSP245max,PostMonsoondata71_00SSP245max)
  
  MeltPostMonsoonSSP245max <- melt(PostMonsoondataSSP245max,id.vars=c("Year","Model","Period"))
  colnames(MeltPostMonsoonSSP245max)[4] <- "Season"
  colnames(MeltPostMonsoonSSP245max)[5] <- "Tmax"
  
  
  PostMonsoondataSSP245min <- rbind(PostMonsoondata21_45SSP245min,PostMonsoondata46_70SSP245min,PostMonsoondata71_00SSP245min)
  
  MeltPostMonsoonSSP245min <- melt(PostMonsoondataSSP245min,id.vars=c("Year","Model","Period"))
  colnames(MeltPostMonsoonSSP245min)[4] <- "Season"
  colnames(MeltPostMonsoonSSP245min)[5] <- "Tmin"
  
  MeltPostMonsoonSSP245max$Tmin <- MeltPostMonsoonSSP245min$Tmin
  
  MeltPostMonsoonSSP245 <- reshape2::melt(MeltPostMonsoonSSP245max,id.vars=c("Year","Model","Season","Period"))
  
  
  dfseasonSSP245 <- rbind(MeltWinterSSP245,MeltPreMonsoonSSP245,MeltMonsoonSSP245,MeltPostMonsoonSSP245)
 
  #SSP 585
  #Winter
  #Tmax
  Winterdata21_45SSP585max <- Winter(df21_45SSP585max,"SSP 585","Near-Future")
  Winterdata46_70SSP585max <- Winter(df46_70SSP585max,"SSP 585","Mid-Future")
  Winterdata71_00SSP585max <- Winter(df71_00SSP585max,"SSP 585","Far-Future")
  
  #Tmin
  Winterdata21_45SSP585min <- Winter(df21_45SSP585min,"SSP 585","Near-Future")
  Winterdata46_70SSP585min <- Winter(df46_70SSP585min,"SSP 585","Mid-Future")
  Winterdata71_00SSP585min <- Winter(df71_00SSP585min,"SSP 585","Far-Future")
  
  
  WinterdataSSP585max <- rbind(Winterdata21_45SSP585max,Winterdata46_70SSP585max,Winterdata71_00SSP585max)
  
  MeltWinterSSP585max <- melt(WinterdataSSP585max,id.vars=c("Year","Model","Period"))
  colnames(MeltWinterSSP585max)[4] <- "Season"
  colnames(MeltWinterSSP585max)[5] <- "Tmax"
  
  
  WinterdataSSP585min <- rbind(Winterdata21_45SSP585min,Winterdata46_70SSP585min,Winterdata71_00SSP585min)
  
  MeltWinterSSP585min <- melt(WinterdataSSP585min,id.vars=c("Year","Model","Period"))
  colnames(MeltWinterSSP585min)[4] <- "Season"
  colnames(MeltWinterSSP585min)[5] <- "Tmin"
  
  MeltWinterSSP585max$Tmin <- MeltWinterSSP585min$Tmin
  
  MeltWinterSSP585 <- reshape2::melt(MeltWinterSSP585max,id.vars=c("Year","Model","Season","Period"))
  
  #PreMonsoon
  #Tmax
  PreMonsoondata21_45SSP585max <- PreMonsoon(df21_45SSP585max,"SSP 585","Near-Future")
  PreMonsoondata46_70SSP585max <- PreMonsoon(df46_70SSP585max,"SSP 585","Mid-Future")
  PreMonsoondata71_00SSP585max <- PreMonsoon(df71_00SSP585max,"SSP 585","Far-Future")
  
  #Tmin
  PreMonsoondata21_45SSP585min <- PreMonsoon(df21_45SSP585min,"SSP 585","Near-Future")
  PreMonsoondata46_70SSP585min <- PreMonsoon(df46_70SSP585min,"SSP 585","Mid-Future")
  PreMonsoondata71_00SSP585min <- PreMonsoon(df71_00SSP585min,"SSP 585","Far-Future")
  
  
  PreMonsoondataSSP585max <- rbind(PreMonsoondata21_45SSP585max,PreMonsoondata46_70SSP585max,PreMonsoondata71_00SSP585max)
  
  MeltPreMonsoonSSP585max <- melt(PreMonsoondataSSP585max,id.vars=c("Year","Model","Period"))
  colnames(MeltPreMonsoonSSP585max)[4] <- "Season"
  colnames(MeltPreMonsoonSSP585max)[5] <- "Tmax"
  
  
  PreMonsoondataSSP585min <- rbind(PreMonsoondata21_45SSP585min,PreMonsoondata46_70SSP585min,PreMonsoondata71_00SSP585min)
  
  MeltPreMonsoonSSP585min <- melt(PreMonsoondataSSP585min,id.vars=c("Year","Model","Period"))
  colnames(MeltPreMonsoonSSP585min)[4] <- "Season"
  colnames(MeltPreMonsoonSSP585min)[5] <- "Tmin"
  
  MeltPreMonsoonSSP585max$Tmin <- MeltPreMonsoonSSP585min$Tmin
  
  MeltPreMonsoonSSP585 <- reshape2::melt(MeltPreMonsoonSSP585max,id.vars=c("Year","Model","Season","Period"))
  
  #Monsoon
  #Tmax
  Monsoondata21_45SSP585max <- Monsoon(df21_45SSP585max,"SSP 585","Near-Future")
  Monsoondata46_70SSP585max <- Monsoon(df46_70SSP585max,"SSP 585","Mid-Future")
  Monsoondata71_00SSP585max <- Monsoon(df71_00SSP585max,"SSP 585","Far-Future")
  
  #Tmin
  Monsoondata21_45SSP585min <- Monsoon(df21_45SSP585min,"SSP 585","Near-Future")
  Monsoondata46_70SSP585min <- Monsoon(df46_70SSP585min,"SSP 585","Mid-Future")
  Monsoondata71_00SSP585min <- Monsoon(df71_00SSP585min,"SSP 585","Far-Future")
  
  
  MonsoondataSSP585max <- rbind(Monsoondata21_45SSP585max,Monsoondata46_70SSP585max,Monsoondata71_00SSP585max)
  
  MeltMonsoonSSP585max <- melt(MonsoondataSSP585max,id.vars=c("Year","Model","Period"))
  colnames(MeltMonsoonSSP585max)[4] <- "Season"
  colnames(MeltMonsoonSSP585max)[5] <- "Tmax"
  
  
  MonsoondataSSP585min <- rbind(Monsoondata21_45SSP585min,Monsoondata46_70SSP585min,Monsoondata71_00SSP585min)
  
  MeltMonsoonSSP585min <- melt(MonsoondataSSP585min,id.vars=c("Year","Model","Period"))
  colnames(MeltMonsoonSSP585min)[4] <- "Season"
  colnames(MeltMonsoonSSP585min)[5] <- "Tmin"
  
  MeltMonsoonSSP585max$Tmin <- MeltMonsoonSSP585min$Tmin
  
  MeltMonsoonSSP585 <- reshape2::melt(MeltMonsoonSSP585max,id.vars=c("Year","Model","Season","Period"))
  
  #PostMonsoon
  #Tmax
  PostMonsoondata21_45SSP585max <- PostMonsoon(df21_45SSP585max,"SSP 585","Near-Future")
  PostMonsoondata46_70SSP585max <- PostMonsoon(df46_70SSP585max,"SSP 585","Mid-Future")
  PostMonsoondata71_00SSP585max <- PostMonsoon(df71_00SSP585max,"SSP 585","Far-Future")
  
  #Tmin
  PostMonsoondata21_45SSP585min <- PostMonsoon(df21_45SSP585min,"SSP 585","Near-Future")
  PostMonsoondata46_70SSP585min <- PostMonsoon(df46_70SSP585min,"SSP 585","Mid-Future")
  PostMonsoondata71_00SSP585min <- PostMonsoon(df71_00SSP585min,"SSP 585","Far-Future")
  
  
  PostMonsoondataSSP585max <- rbind(PostMonsoondata21_45SSP585max,PostMonsoondata46_70SSP585max,PostMonsoondata71_00SSP585max)
  
  MeltPostMonsoonSSP585max <- melt(PostMonsoondataSSP585max,id.vars=c("Year","Model","Period"))
  colnames(MeltPostMonsoonSSP585max)[4] <- "Season"
  colnames(MeltPostMonsoonSSP585max)[5] <- "Tmax"
  
  
  PostMonsoondataSSP585min <- rbind(PostMonsoondata21_45SSP585min,PostMonsoondata46_70SSP585min,PostMonsoondata71_00SSP585min)
  
  MeltPostMonsoonSSP585min <- melt(PostMonsoondataSSP585min,id.vars=c("Year","Model","Period"))
  colnames(MeltPostMonsoonSSP585min)[4] <- "Season"
  colnames(MeltPostMonsoonSSP585min)[5] <- "Tmin"
  
  MeltPostMonsoonSSP585max$Tmin <- MeltPostMonsoonSSP585min$Tmin
  
  MeltPostMonsoonSSP585 <- reshape2::melt(MeltPostMonsoonSSP585max,id.vars=c("Year","Model","Season","Period"))
  
  
  dfseasonSSP585 <- rbind(MeltWinterSSP585,MeltPreMonsoonSSP585,MeltMonsoonSSP585,MeltPostMonsoonSSP585)
  
  plottitleSSP245 <- "SSP 245" 
  plottitleSSP585 <- "SSP 585" 
  stationdet <- paste0(stationname," ","(Index: ",stationnum,")")
  
  #SSP245
  A <- ggplot(dfseasonSSP245,aes(x=as.numeric(Year),y=value,color=variable)) + 
    geom_line(size=1)+geom_smooth(method=lm,size=1,lty=6)+
    facet_grid(Season~factor(Period,levels = c("Near-Future","Mid-Future","Far-Future")),scales="free" )+      #Create gridded plot wih Models in x direction, no of column restricts no of plot in a row, scale=free_y allows yaxis scale to adjust freely according to data 
    ggtitle(plottitleSSP245)+                             #plot title at the top
    #scale_y_continuous(expand=c(0.02,0.02))+
    #scale_x_continuous(expand=c(0.02,0.02))+
    theme(axis.title.x=element_text(hjust=0.5, vjust=1,size=14,lineheight = 0.5),   #customize x axis label 
          axis.title.y=element_text(hjust=0.5,vjust=1,size=14,lineheight=0.5),     #customize y axis label
          plot.title=element_text(hjust=0.5,size=16,lineheight=0.5),               #customize plot title
          axis.text.x =element_text(angle=90,hjust=0.5,vjust=1,size=10), #customize x-axis tick label
          axis.text.y =element_text(hjust=0.5, vjust=0.5,size=10,lineheight = 0.2), #customize y-axis tick label
          strip.text=element_text(size=14),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=12))+                                  #customize facet_wrap Title( i.e Model name displayed)
    labs(x="Years", y="Seasonal Temperature (°C)")
  #coord_cartesian(ylim= c(1000,yupper))
  #ggsave(filename=paste0(stationnum,".png"),A, device=png,
  #    savepath, width=12, height=8)
  
  
  
  #SSP585
  B <- ggplot(dfseasonSSP585,aes(x=as.numeric(Year),y=value,color=variable)) + 
    geom_line(size=1)+geom_smooth(method=lm,size=1,lty=6)+
    facet_grid(Season~factor(Period,levels = c("Near-Future","Mid-Future","Far-Future")),scales="free" )+      #Create gridded plot wih Models in x direction, no of column restricts no of plot in a row, scale=free_y allows yaxis scale to adjust freely according to data 
    ggtitle(plottitleSSP585)+                             #plot title at the top
    #scale_x_continuous(expand=c(0.02,0.02))+
    theme(axis.title.x=element_text(hjust=0.5, vjust=1,size=14,lineheight = 0.5),   #customize x axis label 
          axis.title.y=element_text(hjust=0.5,vjust=1,size=14,lineheight=0.5),     #customize y axis label
          plot.title=element_text(hjust=0.5,size=16,lineheight=0.5),               #customize plot title
          axis.text.x =element_text(angle=90,hjust=0.5,vjust=1,size=10), #customize x-axis tick label
          axis.text.y =element_text(hjust=0.5, vjust=0.5,size=10,lineheight = 0.2), #customize y-axis tick label
          strip.text=element_text(size=14),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=12))+                                  #customize facet_wrap Title( i.e Model name displayed)
    labs(x="Years", y="Seasonal Temperature (°C)")
  #coord_cartesian(ylim= c(1000,yupper))
  #ggsave(filename=paste0(stationnum,".png"),B, device=png,
  #    savepath, width=12, height=8)
  #}
  
  
  C <- cowplot::plot_grid(A,B,ncol = 2,nrow=1)
  title <- ggdraw() + draw_label(stationdet, fontface='bold',size="20")
  D <- plot_grid(title, C, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
  save_plot(paste0(savepath,stationnum,".png"),D,base_width = 20,base_height = 10)  
  
}
