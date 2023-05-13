rm(list=ls())

library(dplyr)
library(modifiedmk)
library(tibble)
library(crayon)
library(writexl)
library(readxl)

refpath1 <-"C:/Users/Dell/Desktop/New Climate Analysis Data/Temperature/Tmax/"
refpath2 <-"C:/Users/Dell/Desktop/New Climate Analysis Data/Temperature/Tmin/" 
filesavepath <-"C:/Users/Dell/Desktop/Future Trend/"


modelname <- c("SSP 245", "SSP 585")
period <- c("2021-2045","2046-2070","2071-2100")

YearMonth <- function(df){
  df$Year <- format(as.Date(df$Date),"%Y")
  df$Month <- format(as.Date(df$Date),"%m")
  return(df)
}


for (i in 1:length(modelname)){ #
  for(k in 1:length(period)){#
    
  path1 <- paste0(refpath1,modelname[i],"/",period[k])
  filepath1 <- list.files(path1,full.names = TRUE)
  
  path2 <-  paste0(refpath2,modelname[i],"/",period[k])
  filepath2 <- list.files(path2,full.names = TRUE)
  
  for (j in 1:length(filepath1)){ #
    df1 <- read_excel(filepath1[j],col_names = TRUE)
    df1 <- YearMonth(df1)
    
    df2 <- read_excel(filepath2[j],col_names =  TRUE)
    df2 <- YearMonth(df2)
    
    if(j <= 24){
      stationnum <- substr(list.files(path1)[j],1,4)
    }
    else{
      stationnum <- substr(list.files(path1)[j],1,3)
    }
    
    
    #AnnualAverage
    Annualdatamax <- df1 %>% group_by(Year) %>% summarise(AnnualAvg = mean(Tmax))
    Annualtrendmax <- data.frame(t(modifiedmk::mkttest(Annualdatamax$AnnualAvg))) 
    rownames(Annualtrendmax) <- stationnum
    Annualtrendmax <- tibble::rownames_to_column(Annualtrendmax)
    Annualtrendmax[,1] <- as.numeric(Annualtrendmax[,1])
    colnames(Annualtrendmax) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
    
    Annualdatamin <- df2 %>% group_by(Year) %>% summarise(AnnualAvg = mean(Tmin))
    Annualtrendmin <- data.frame(t(modifiedmk::mkttest(Annualdatamin$AnnualAvg))) 
    rownames(Annualtrendmin) <- stationnum
    Annualtrendmin <- tibble::rownames_to_column(Annualtrendmin)
    Annualtrendmin[,1] <- as.numeric(Annualtrendmin[,1])
    colnames(Annualtrendmin) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
    
    
    #Winter
    Winterdatamax <- df1%>% dplyr::filter( Month == "01" | Month == "02"  | Month == "12" ) %>%
      group_by(Year) %>%
      summarise(WinterAvg=mean(Tmax))
    Wintertrendmax <- data.frame(t(modifiedmk::mkttest(Winterdatamax$WinterAvg))) 
    rownames(Wintertrendmax) <- stationnum
    Wintertrendmax <- tibble::rownames_to_column(Wintertrendmax)
    Wintertrendmax[,1] <- as.numeric(Wintertrendmax[,1])
    colnames(Wintertrendmax) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
    
    Winterdatamin <- df2%>% dplyr::filter( Month == "01" | Month == "02"  | Month == "12" ) %>%
      group_by(Year) %>%
      summarise(WinterAvg=mean(Tmin))
    Wintertrendmin <- data.frame(t(modifiedmk::mkttest(Winterdatamin$WinterAvg))) 
    rownames(Wintertrendmin) <- stationnum
    Wintertrendmin <- tibble::rownames_to_column(Wintertrendmin)
    Wintertrendmin[,1] <- as.numeric(Wintertrendmin[,1])
    colnames(Wintertrendmin) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
    
    
    #Pre-Monsoon
    PreMonsoondatamax <- df1%>% dplyr::filter( Month == "03"  | Month == "04"  | Month == "05" ) %>%
      group_by(Year) %>%
      summarise(PreMonsoonAvg=mean(Tmax))
    PreMonsoontrendmax <- data.frame(t(modifiedmk::mkttest(PreMonsoondatamax$PreMonsoonAvg))) 
    rownames(PreMonsoontrendmax) <- stationnum
    PreMonsoontrendmax <- tibble::rownames_to_column(PreMonsoontrendmax)
    PreMonsoontrendmax[,1] <- as.numeric(PreMonsoontrendmax[,1])
    colnames(PreMonsoontrendmax) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
    
    PreMonsoondatamin <- df2%>% dplyr::filter( Month == "03"  | Month == "04"  | Month == "05" ) %>%
      group_by(Year) %>%
      summarise(PreMonsoonAvg=mean(Tmin))
    PreMonsoontrendmin <- data.frame(t(modifiedmk::mkttest(PreMonsoondatamin$PreMonsoonAvg))) 
    rownames(PreMonsoontrendmin) <- stationnum
    PreMonsoontrendmin <- tibble::rownames_to_column(PreMonsoontrendmin)
    PreMonsoontrendmin[,1] <- as.numeric(PreMonsoontrendmin[,1])
    colnames(PreMonsoontrendmin) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
    
    
    #Monsoon
    Monsoondatamax <- df1%>% dplyr::filter( Month == "06"  | Month == "07"  | Month == "08" | Month =="09" ) %>%
      group_by(Year) %>%
      summarise(MonsoonAvg=mean(Tmax))
    Monsoontrendmax <- data.frame(t(modifiedmk::mkttest(Monsoondatamax$MonsoonAvg))) 
    rownames(Monsoontrendmax) <- stationnum
    Monsoontrendmax <- tibble::rownames_to_column(Monsoontrendmax)
    Monsoontrendmax[,1] <- as.numeric(Monsoontrendmax[,1])
    colnames(Monsoontrendmax) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
    
    Monsoondatamin <- df2%>% dplyr::filter(  Month == "06"  | Month == "07"  | Month == "08" | Month =="09" ) %>%
      group_by(Year) %>%
      summarise(MonsoonAvg=mean(Tmin))
    Monsoontrendmin <- data.frame(t(modifiedmk::mkttest(Monsoondatamin$MonsoonAvg))) 
    rownames(Monsoontrendmin) <- stationnum
    Monsoontrendmin <- tibble::rownames_to_column(Monsoontrendmin)
    Monsoontrendmin[,1] <- as.numeric(Monsoontrendmin[,1])
    colnames(Monsoontrendmin) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
    
    
    
    #Post-Monsoon
    PostMonsoondatamax <- df1%>% dplyr::filter( Month == "10"  | Month == "11" ) %>%
      group_by(Year) %>%
      summarise(PostMonsoonAvg=mean(Tmax))
    PostMonsoontrendmax <- data.frame(t(modifiedmk::mkttest(PostMonsoondatamax$PostMonsoonAvg))) 
    rownames(PostMonsoontrendmax) <- stationnum
    PostMonsoontrendmax <- tibble::rownames_to_column(PostMonsoontrendmax)
    PostMonsoontrendmax[,1] <- as.numeric(PostMonsoontrendmax[,1])
    colnames(PostMonsoontrendmax) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
    
    PostMonsoondatamin <- df2%>% dplyr::filter( Month == "10"  | Month == "11" ) %>%
      group_by(Year) %>%
      summarise(PostMonsoonAvg=mean(Tmin))
    PostMonsoontrendmin <- data.frame(t(modifiedmk::mkttest(PostMonsoondatamin$PostMonsoonAvg))) 
    rownames(PostMonsoontrendmin) <- stationnum
    PostMonsoontrendmin <- tibble::rownames_to_column(PostMonsoontrendmin)
    PostMonsoontrendmin[,1] <- as.numeric(PostMonsoontrendmin[,1])
    colnames(PostMonsoontrendmin) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
    
    
    
    
    if(j==1){
      TableAnnualmax <- Annualtrendmax
      TableWintermax <- Wintertrendmax
      TablePreMonsoonmax <- PreMonsoontrendmax
      TableMonsoonmax <- Monsoontrendmax
      TablePostMonsoonmax <- PostMonsoontrendmax
      
      TableAnnualmin <- Annualtrendmin
      TableWintermin <- Wintertrendmin
      TablePreMonsoonmin <- PreMonsoontrendmin
      TableMonsoonmin <- Monsoontrendmin
      TablePostMonsoonmin <- PostMonsoontrendmin
    }
    else {
      TableAnnualmax <- rbind(TableAnnualmax,Annualtrendmax)
      TableWintermax <- rbind(TableWintermax,Wintertrendmax)
      TablePreMonsoonmax <- rbind(TablePreMonsoonmax,PreMonsoontrendmax)
      TableMonsoonmax <- rbind(TableMonsoonmax,Monsoontrendmax)
      TablePostMonsoonmax <- rbind(TablePostMonsoonmax,PostMonsoontrendmax)
      
      TableAnnualmin <- rbind(TableAnnualmin,Annualtrendmin)
      TableWintermin <- rbind(TableWintermin,Wintertrendmin)
      TablePreMonsoonmin <- rbind(TablePreMonsoonmin,PreMonsoontrendmin)
      TableMonsoonmin <- rbind(TableMonsoonmin,Monsoontrendmin)
      TablePostMonsoonmin <- rbind(TablePostMonsoonmin,PostMonsoontrendmin)
    }
    
  }                                    
  
  write_xlsx(list(Tmax = TableAnnualmax, Tmin = TableAnnualmin),paste0(filesavepath,modelname[i],"/Temperature/",period[k],"/Annualtrend.xlsx"))
  write_xlsx(list(Tmax = TableWintermax, Tmin = TableWintermin),paste0(filesavepath,modelname[i],"/Temperature/",period[k],"/Wintertrend.xlsx"))
  write_xlsx(list(Tmax = TablePreMonsoonmax, Tmin = TablePreMonsoonmin),paste0(filesavepath,modelname[i],"/Temperature/",period[k],"/PreMonsoontrend.xlsx"))
  write_xlsx(list(Tmax = TableMonsoonmax, Tmin = TableMonsoonmin),paste0(filesavepath,modelname[i],"/Temperature/",period[k],"/Monsoontrend.xlsx"))
  write_xlsx(list(Tmax = TablePostMonsoonmax, Tmin = TablePostMonsoonmin),paste0(filesavepath,modelname[i],"/Temperature/",period[k],"/PostMonsoontrend.xlsx"))
  }
}   
