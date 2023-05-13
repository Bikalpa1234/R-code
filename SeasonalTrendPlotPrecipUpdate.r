rm(list=ls())

library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)
library(gridExtra)
library(cowplot)

refpath <- "C:/Users/Dell/Desktop/New Climate Analysis Data/Precipitation/"
savepath <- "C:/Users/Dell/Desktop/Climate Projection Plots/Characterization/Precipitation/Seasonal/"


#modelname <- c("SSP 245", "SSP 585")

excelpath <- "C:/Users/Dell/Desktop/MultiModel Ensemble/Precipitation Data Available.xlsx"
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
    summarise(Winter=sum(Rain))
  Winterdata$Model <- modelname
  Winterdata$Period <- name
  return(Winterdata)
}

PreMonsoon <- function(df,modelname,name){
  PreMonsoondata <- df%>% dplyr::filter( Month == "03"  | Month == "04"  | Month == "05" ) %>%
    group_by(Year) %>%
    summarise(PreMonsoon=sum(Rain))
  PreMonsoondata$Model <- modelname
  PreMonsoondata$Period <- name
  return(PreMonsoondata)
}

Monsoon <- function(df,modelname,name){
  Monsoondata <- df%>% dplyr::filter( Month == "06"  | Month == "07"  | Month == "08" | Month =="09" ) %>%
    group_by(Year) %>%
    summarise(Monsoon=sum(Rain))
  Monsoondata$Model <- modelname
  Monsoondata$Period <- name
  return(Monsoondata)
}

PostMonsoon <- function(df,modelname,name){
  PostMonsoondata <- df%>% dplyr::filter( Month == "10"  | Month == "11"  ) %>%
    group_by(Year) %>%
    summarise(PostMonsoon=sum(Rain))
  PostMonsoondata$Model <- modelname
  PostMonsoondata$Period <- name
  return(PostMonsoondata)
}


for(n in 1:66){ #66
  #for (i in 1:1){   #length(modelname)
  #SSP 245
    path21_45SSP245 <- paste0(refpath,"SSP 245","/2021-2045/")
    filepath21_45SSP245 <- list.files(path21_45SSP245,full.names=TRUE)
    path46_70SSP245 <-  paste0(refpath,"SSP 245","/2046-2070/")
    filepath46_70SSP245 <- list.files(path46_70SSP245,full.names=TRUE)
    path71_00SSP245 <- paste0(refpath,"SSP 245","/2071-2100/")
    filepath71_00SSP245 <- list.files(path71_00SSP245,full.names=TRUE)
    
    #SSP 585
    path21_45SSP585 <- paste0(refpath,"SSP 585","/2021-2045/")
    filepath21_45SSP585 <- list.files(path21_45SSP585,full.names=TRUE)
    path46_70SSP585 <-  paste0(refpath,"SSP 585","/2046-2070/")
    filepath46_70SSP585 <- list.files(path46_70SSP585,full.names=TRUE)
    path71_00SSP585 <- paste0(refpath,"SSP 585","/2071-2100/")
    filepath71_00SSP585 <- list.files(path71_00SSP585,full.names=TRUE)
    
    for (j in n:n){  #length(filepath)
      #SSP 245
      df21_45SSP245 <- read_excel(filepath21_45SSP245[j],col_names =  TRUE)
      df21_45SSP245 <- Year_Month(df21_45SSP245)
      
      df46_70SSP245 <- read_excel(filepath46_70SSP245[j],col_names =  TRUE)
      df46_70SSP245 <- Year_Month(df46_70SSP245)
      
      df71_00SSP245 <- read_excel(filepath71_00SSP245[j],col_names =  TRUE)
      df71_00SSP245 <- Year_Month(df71_00SSP245)
      
      #SSP 585
      df21_45SSP585 <- read_excel(filepath21_45SSP585[j],col_names =  TRUE)
      df21_45SSP585 <- Year_Month(df21_45SSP585)
      
      df46_70SSP585 <- read_excel(filepath46_70SSP585[j],col_names =  TRUE)
      df46_70SSP585 <- Year_Month(df46_70SSP585)
      
      df71_00SSP585 <- read_excel(filepath71_00SSP585[j],col_names =  TRUE)
      df71_00SSP585 <- Year_Month(df71_00SSP585)
      
      if(j<=59){
        stationnum <- substr(list.files(path21_45SSP245)[j],1,4)
      }
      else{
        stationnum <- substr(list.files(path21_45SSP245)[j],1,3)
      }
    }
    stationname <- excelfile[n,1]
    
    #SSP 245
    #Winter
    Winterdata21_45SSP245 <- Winter(df21_45SSP245,"SSP 245","Near-Future")
    Winterdata46_70SSP245 <- Winter(df46_70SSP245,"SSP 245","Mid-Future")
    Winterdata71_00SSP245 <- Winter(df71_00SSP245,"SSP 245","Far-Future")
    
    WinterdataSSP245 <- rbind(Winterdata21_45SSP245,Winterdata46_70SSP245,Winterdata71_00SSP245)
    
    MeltwinterSSP245 <- melt(WinterdataSSP245,id.vars=c("Year","Model","Period"))
    colnames(MeltwinterSSP245)[4] <- "Season"
    colnames(MeltwinterSSP245)[5] <- "Rain"
    
    #Pre-Monsoon
    Premonsoondata21_45SSP245 <- PreMonsoon(df21_45SSP245,"SSP 245","Near-Future")
    Premonsoondata46_70SSP245 <- PreMonsoon(df46_70SSP245,"SSP 245","Mid-Future")
    Premonsoondata71_00SSP245 <- PreMonsoon(df71_00SSP245,"SSP 245","Far-Future")
   
    PremonsoondataSSP245 <- rbind(Premonsoondata21_45SSP245,Premonsoondata46_70SSP245,Premonsoondata71_00SSP245)
    
    MeltpremonsoonSSP245 <- melt(PremonsoondataSSP245,id.vars=c("Year","Model","Period"))
    colnames(MeltpremonsoonSSP245)[4] <- "Season"
    colnames(MeltpremonsoonSSP245)[5] <- "Rain"
    
    
    #Monsoon
    Monsoondata21_45SSP245 <- Monsoon(df21_45SSP245,"SSP 245","Near-Future")
    Monsoondata46_70SSP245 <- Monsoon(df46_70SSP245,"SSP 245","Mid-Future")
    Monsoondata71_00SSP245 <- Monsoon(df71_00SSP245,"SSP 245","Far-Future")
    
    MonsoondataSSP245 <- rbind(Monsoondata21_45SSP245,Monsoondata46_70SSP245,Monsoondata71_00SSP245)
    
    MeltmonsoonSSP245 <- melt(MonsoondataSSP245,id.vars=c("Year","Model","Period"))
    colnames(MeltmonsoonSSP245)[4] <- "Season"
    colnames(MeltmonsoonSSP245)[5] <- "Rain"
    
    
    #Post-Monsoon
    PostMonsoondata21_45SSP245 <- PostMonsoon(df21_45SSP245,"SSP 245","Near-Future")
    PostMonsoondata46_70SSP245 <- PostMonsoon(df46_70SSP245,"SSP 245","Mid-Future")
    PostMonsoondata71_00SSP245 <- PostMonsoon(df71_00SSP245,"SSP 245","Far-Future")
    
    PostMonsoondataSSP245 <- rbind(PostMonsoondata21_45SSP245,PostMonsoondata46_70SSP245,PostMonsoondata71_00SSP245)
    
    MeltpostmonsoonSSP245 <- melt(PostMonsoondataSSP245,id.vars=c("Year","Model","Period"))
    colnames(MeltpostmonsoonSSP245)[4] <- "Season"
    colnames(MeltpostmonsoonSSP245)[5] <- "Rain"
    
    
    dfseasonSSP245 <- rbind(MeltwinterSSP245,MeltpremonsoonSSP245,MeltmonsoonSSP245,MeltpostmonsoonSSP245)
    
    #SSP 585
    #Winter
    Winterdata21_45SSP585 <- Winter(df21_45SSP585,"SSP 585","Near-Future")
    Winterdata46_70SSP585 <- Winter(df46_70SSP585,"SSP 585","Mid-Future")
    Winterdata71_00SSP585 <- Winter(df71_00SSP585,"SSP 585","Far-Future")
    
    WinterdataSSP585 <- rbind(Winterdata21_45SSP585,Winterdata46_70SSP585,Winterdata71_00SSP585)
    
    MeltwinterSSP585 <- melt(WinterdataSSP585,id.vars=c("Year","Model","Period"))
    colnames(MeltwinterSSP585)[4] <- "Season"
    colnames(MeltwinterSSP585)[5] <- "Rain"
    
    #Pre-Monsoon
    Premonsoondata21_45SSP585 <- PreMonsoon(df21_45SSP585,"SSP 585","Near-Future")
    Premonsoondata46_70SSP585 <- PreMonsoon(df46_70SSP585,"SSP 585","Mid-Future")
    Premonsoondata71_00SSP585 <- PreMonsoon(df71_00SSP585,"SSP 585","Far-Future")
    
    PremonsoondataSSP585 <- rbind(Premonsoondata21_45SSP585,Premonsoondata46_70SSP585,Premonsoondata71_00SSP585)
    
    MeltpremonsoonSSP585 <- melt(PremonsoondataSSP585,id.vars=c("Year","Model","Period"))
    colnames(MeltpremonsoonSSP585)[4] <- "Season"
    colnames(MeltpremonsoonSSP585)[5] <- "Rain"
    
    
    #Monsoon
    Monsoondata21_45SSP585 <- Monsoon(df21_45SSP585,"SSP 585","Near-Future")
    Monsoondata46_70SSP585 <- Monsoon(df46_70SSP585,"SSP 585","Mid-Future")
    Monsoondata71_00SSP585 <- Monsoon(df71_00SSP585,"SSP 585","Far-Future")
    
    MonsoondataSSP585 <- rbind(Monsoondata21_45SSP585,Monsoondata46_70SSP585,Monsoondata71_00SSP585)
    
    MeltmonsoonSSP585 <- melt(MonsoondataSSP585,id.vars=c("Year","Model","Period"))
    colnames(MeltmonsoonSSP585)[4] <- "Season"
    colnames(MeltmonsoonSSP585)[5] <- "Rain"
    
    
    #Post-Monsoon
    PostMonsoondata21_45SSP585 <- PostMonsoon(df21_45SSP585,"SSP 585","Near-Future")
    PostMonsoondata46_70SSP585 <- PostMonsoon(df46_70SSP585,"SSP 585","Mid-Future")
    PostMonsoondata71_00SSP585 <- PostMonsoon(df71_00SSP585,"SSP 585","Far-Future")
    
    PostMonsoondataSSP585 <- rbind(PostMonsoondata21_45SSP585,PostMonsoondata46_70SSP585,PostMonsoondata71_00SSP585)
    
    MeltpostmonsoonSSP585 <- melt(PostMonsoondataSSP585,id.vars=c("Year","Model","Period"))
    colnames(MeltpostmonsoonSSP585)[4] <- "Season"
    colnames(MeltpostmonsoonSSP585)[5] <- "Rain"
    
    
    dfseasonSSP585 <- rbind(MeltwinterSSP585,MeltpremonsoonSSP585,MeltmonsoonSSP585,MeltpostmonsoonSSP585)
    
    
    
    plottitleSSP245 <- "SSP 245" 
    plottitleSSP585 <- "SSP 585" 
    stationdet <- paste0(stationname," ","(Index: ",stationnum,")")
    
    #SSP245
    A <- ggplot(dfseasonSSP245,aes(x=as.numeric(Year),y=Rain)) + 
      geom_line(size=1)+geom_smooth(method=lm)+
      facet_grid(Season~factor(Period,levels = c("Near-Future","Mid-Future","Far-Future")),scales="free" )+      #Create gridded plot wih Models in x direction, no of column restricts no of plot in a row, scale=free_y allows yaxis scale to adjust freely according to data 
      ggtitle(plottitleSSP245)+                             #plot title at the top
      #scale_x_continuous(expand=c(0.02,0.02))+
      theme(axis.title.x=element_text(hjust=0.5, vjust=1,size=14,lineheight = 0.5),   #customize x axis label 
            axis.title.y=element_text(hjust=0.5,vjust=1,size=14,lineheight=0.5),     #customize y axis label
            plot.title=element_text(hjust=0.5,size=16,lineheight=0.5),               #customize plot title
            axis.text.x =element_text(angle=90,hjust=0.5, vjust=1,size=10), #customize x-axis tick label
            axis.text.y =element_text(hjust=0.5, vjust=1,size=12,lineheight = 0.2), #customize y-axis tick label
            strip.text=element_text(size=14))+                                  #customize facet_wrap Title( i.e Model name displayed)
      labs(x="Years", y="Seasonal Rainfall (mm)")
    #coord_cartesian(ylim= c(1000,yupper))
    #ggsave(filename=paste0(stationnum,".png"),A, device=png,
    #    savepath, width=12, height=8)
    
   
    
    #SSP585
    B <- ggplot(dfseasonSSP585,aes(x=as.numeric(Year),y=Rain)) + 
      geom_line(size=1)+geom_smooth(method=lm)+
      facet_grid(Season~factor(Period,levels = c("Near-Future","Mid-Future","Far-Future")),scales="free" )+      #Create gridded plot wih Models in x direction, no of column restricts no of plot in a row, scale=free_y allows yaxis scale to adjust freely according to data 
      ggtitle(plottitleSSP585)+                             #plot title at the top
      #scale_x_continuous(expand=c(0.02,0.02))+
      theme(axis.title.x=element_text(hjust=0.5, vjust=1,size=14,lineheight = 0.5),   #customize x axis label 
            axis.title.y=element_text(hjust=0.5,vjust=1,size=14,lineheight=0.5),     #customize y axis label
            plot.title=element_text(hjust=0.5,size=16,lineheight=0.5),               #customize plot title
            axis.text.x =element_text(angle=90,hjust=0.5, vjust=1,size=10), #customize x-axis tick label
            axis.text.y =element_text(hjust=0.5, vjust=1,size=12,lineheight = 0.2), #customize y-axis tick label
            strip.text=element_text(size=14))+                                  #customize facet_wrap Title( i.e Model name displayed)
      labs(x="Years", y="Seasonal Rainfall (mm)")
    #coord_cartesian(ylim= c(1000,yupper))
    #ggsave(filename=paste0(stationnum,".png"),B, device=png,
    #    savepath, width=12, height=8)
    #}
    
    
    C <- cowplot::plot_grid(A,B,ncol = 2,nrow=1)
    title <- ggdraw() + draw_label(stationdet, fontface='bold',size="20")
    D <- plot_grid(title, C, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
    save_plot(paste0(savepath,stationnum,".png"),D,base_width = 20,base_height = 10)  
}

#SSP245 <- plotter("SSP 245")
#SSP585 <- plotter("SSP 585")