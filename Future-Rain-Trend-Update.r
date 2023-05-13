rm(list=ls())

library(dplyr)
library(modifiedmk)
library(tibble)
library(crayon)
library(writexl)
library(readxl)



refpath <-"C:/Users/Dell/Desktop/New Climate Analysis Data/Precipitation/" 
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
    path <- paste0(refpath,modelname[i],"/",period[k])
    filepath <- list.files(path,full.names = TRUE)
    
    for (j in 1:length(filepath)){ #
      df <- read_excel(filepath[j],col_names =  TRUE)
      df <- YearMonth(df)
      
      if(j <= 59){
        stationnum <- substr(list.files(path)[j],1,4)
      }
      else{
        stationnum <- substr(list.files(path)[j],1,3)
      }
      
      
      
      #AnnualSum
      Annualdata <- df %>% group_by(Year) %>% summarise(AnnualSum = sum(Rain))
      Annualtrend <- data.frame(t(modifiedmk::mkttest(Annualdata$AnnualSum))) 
      rownames(Annualtrend) <- stationnum
      Annualtrend <- tibble::rownames_to_column(Annualtrend)
      Annualtrend[,1] <- as.numeric(Annualtrend[,1])
      colnames(Annualtrend) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
      
      
      
      #Winter
      Winterdata <- df%>% dplyr::filter( Month == "01" | Month == "02"  | Month == "12" ) %>%
        group_by(Year) %>%
        summarise(WinterSum=sum(Rain))
      Wintertrend <- data.frame(t(modifiedmk::mkttest(Winterdata$WinterSum))) 
      rownames(Wintertrend) <- stationnum
      Wintertrend <- tibble::rownames_to_column(Wintertrend)
      Wintertrend[,1] <- as.numeric(Wintertrend[,1])
      colnames(Wintertrend) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
      
      
      #Pre-Monsoon
      PreMonsoondata <- df%>% dplyr::filter( Month == "03"  | Month == "04"  | Month == "05" ) %>%
        group_by(Year) %>%
        summarise(PreMonsoonSum=sum(Rain))
      PreMonsoontrend <- data.frame(t(modifiedmk::mkttest(PreMonsoondata$PreMonsoonSum))) 
      rownames(PreMonsoontrend) <- stationnum
      PreMonsoontrend <- tibble::rownames_to_column(PreMonsoontrend)
      PreMonsoontrend[,1] <- as.numeric(PreMonsoontrend[,1])
      colnames(PreMonsoontrend) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
      
      
      #Monsoon
      Monsoondata <- df%>% dplyr::filter( Month == "06"  | Month == "07"  | Month == "08" | Month =="09" ) %>%
        group_by(Year) %>%
        summarise(MonsoonSum=sum(Rain))
      Monsoontrend <- data.frame(t(modifiedmk::mkttest(Monsoondata$MonsoonSum))) 
      rownames(Monsoontrend) <- stationnum
      Monsoontrend <- tibble::rownames_to_column(Monsoontrend)
      Monsoontrend[,1] <- as.numeric(Monsoontrend[,1])
      colnames(Monsoontrend) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
      
      
      #Post-Monsoon
      PostMonsoondata <- df%>% dplyr::filter( Month == "10"  | Month == "11"  ) %>%
        group_by(Year) %>%
        summarise(PostMonsoonSum=sum(Rain))
      PostMonsoontrend <- data.frame(t(modifiedmk::mkttest(PostMonsoondata$PostMonsoonSum))) 
      rownames(PostMonsoontrend) <- stationnum
      PostMonsoontrend <- tibble::rownames_to_column(PostMonsoontrend)
      PostMonsoontrend[,1] <- as.numeric(PostMonsoontrend[,1])
      colnames(PostMonsoontrend) <- c("Station","Z-value","Sen's Slope","S","Var S","P-Value","Tau")
      
      
      
      if(j==1){
        TableAnnual <- Annualtrend
        TableWinter <- Wintertrend
        TablePreMonsoon <- PreMonsoontrend
        TableMonsoon <- Monsoontrend
        TablePostMonsoon <- PostMonsoontrend
      }
      else {
        TableAnnual <- rbind(TableAnnual,Annualtrend)
        TableWinter <- rbind(TableWinter,Wintertrend)
        TablePreMonsoon <- rbind(TablePreMonsoon,PreMonsoontrend)
        TableMonsoon <- rbind(TableMonsoon,Monsoontrend)
        TablePostMonsoon <- rbind(TablePostMonsoon,PostMonsoontrend)
        
        
      }
    }
    write_xlsx(TableAnnual,paste0(filesavepath,modelname[i],"/Precipitation/",period[k],"/Annualtrend.xlsx"))
    write_xlsx(TableWinter,paste0(filesavepath,modelname[i],"/Precipitation/",period[k],"/Wintertrend.xlsx"))
    write_xlsx(TablePreMonsoon,paste0(filesavepath,modelname[i],"/Precipitation/",period[k],"/PreMonsoontrend.xlsx"))
    write_xlsx(TableMonsoon,paste0(filesavepath,modelname[i],"/Precipitation/",period[k],"/Monsoontrend.xlsx"))
    write_xlsx(TablePostMonsoon,paste0(filesavepath,modelname[i],"/Precipitation/",period[k],"/PostMonsoontrend.xlsx"))
  }
}

