library(dplyr)
library(lubridate)
library(writexl)

refpath <-"C:/Users/Dell/Desktop/MultiModel Ensemble/Temperature/Tmin/"
refsavepath <- "C:/Users/Dell/Desktop/New Climate Analysis Data/Temperature/Tmin/"
modelname <- c("SSP 245","SSP 585")


for (i in 1:length(modelname)){ #
  path <- paste0(refpath,modelname[i])
  filepath <- list.files(path,full.names = TRUE)
  savepath <- paste0(refsavepath,modelname[i],"/")
  
  for (j in 1:length(filepath)){ #
    df <- read.csv(filepath[j])
    df$Date <- as.Date(df$Date)
    
    df21_45 <- df%>% dplyr::filter(Date >="2021-01-01",Date <="2045-12-31" )   #as.Date("2021-01-01","%Y-%m-%d")
    df46_70 <- df%>% dplyr::filter(Date >="2046-01-01",Date <="2070-12-31" )
    df71_00 <- df%>% dplyr::filter(Date >="2071-01-01",Date <="2100-12-31" )
    
    if(j <=24){
      stationdet <- substr(list.files(path)[j],1,20)
    }
    else{
      stationdet <- substr(list.files(path)[j],1,19)
    }
    
    filename21_45 <- paste0(stationdet,"2021-2045.xlsx")
    filename46_70 <- paste0(stationdet,"2046-2070.xlsx")
    filename71_00 <- paste0(stationdet,"2071-2100.xlsx")
    
    
    
    write_xlsx(df21_45,paste0(savepath,"2021-2045/",filename21_45))
    write_xlsx(df46_70,paste0(savepath,"2046-2070/",filename46_70))
    write_xlsx(df71_00,paste0(savepath,"2071-2100/",filename71_00))
  }
}

