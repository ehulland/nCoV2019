library(sp)
library(reshape2)
library(stringr)

#CHECK LINELIST DATA FUNCTION FOR nCOV DATA
#linelist is a dataframe read in from CSVs of the Hubei or Non-Hubei cases
#out_dir is where you would like the flagged observations and their error messages saved
#hubei_linelist_flag is a 0/1 flag of whether the linelist is for Hubei (1) or outside (0)

#Produces "Long" format of flags with multiple IDs but 1 error per row and an indicator of whether the list is from Hubei our outside Hubei
linelist.flags <- function(linelist, out_dir, hubei_linelist_flag){
  error.df<<-c()
  linelist<-linelist[!is.na(linelist$ID)& is.numeric(linelist$ID),]
  dups <-linelist[duplicated(linelist$ID),]
  dups.ID<-dups[,1]
    for(i in 1:nrow(linelist)){
    if(is.na(linelist[i,]$latitude) | is.na(linelist[i,]$longitude)){
      ID<-linelist[i,1]
      date.conf<-as.character(linelist[i,13])
      error<-"No Lat / Long coordinates"
      df<-data.frame(ID, date.conf, error)
      error.df<<-as.data.frame(rbind(error.df, df))
    }
    if(length(dups.ID)>=1){   
      if(linelist[i,]$ID %in% dups.ID){
          ID<-linelist[i,1]
          date.conf<-as.character(linelist[i,13])
          error<-"ID is duplicated"
          dfd<-data.frame(ID, date.conf, error)
          error.df<<-as.data.frame(rbind(error.df, dfd))
      }
    }
    if(linelist[i,]$age!=""){
      if(!substr(linelist[i,]$age,1,2) %in% 1:110){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"Age not an integer"
        df.a<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df.a))
      } else if(!str_detect(linelist[i,]$age,"-")& as.numeric(substr(linelist[i,]$age,1,2))%in% 1:110 & as.numeric(substr(linelist[i,]$age,4,5)) %in% 1:110){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"Age range not in correct format"
        df.a2<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df.a2))
      }
    }
    if(linelist[i,]$date_onset_symptoms!=""){
      if(substr(linelist[i,]$date_onset_symptoms,3,3)!="." | substr(linelist[i,]$date_onset_symptoms,6,6)!="."){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_onset_symptoms not in DD.MM.YYYY format"
        df3<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df3))
      }else if(!(as.numeric(substr(linelist[i,]$date_onset_symptoms,4,5)) %in% 1:12)){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_onset_symptoms has month outside 1-12 range"
        df4<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df4))
      }else if(!(as.numeric(substr(linelist[i,]$date_onset_symptoms,7,10)) %in% c(2019,2020))){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_onset_symptoms has year outside 2019 or 2020"
        df5<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df5))
      }else if(as.Date(linelist[i,]$date_onset_symptoms, "%d.%m.%Y")> (Sys.Date()+1)){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_onset_symptoms is in the future"
        df6<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df6))
      }
    }
    if(linelist[i,]$date_admission_hospital!=""){
      if(substr(linelist[i,]$date_admission_hospital,3,3)!="." | substr(linelist[i,]$date_admission_hospital,6,6)!="."){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_admission_hospital not in DD.MM.YYYY format"
        df7<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df7))
      } else if(!(as.numeric(substr(linelist[i,]$date_admission_hospital,4,5)) %in% 1:12)){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_admission_hospital has month outside 1-12 range"
        df8<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df8))
      }else if(!(as.numeric(substr(linelist[i,]$date_admission_hospital,7,10)) %in% c(2019,2020))){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_admission_hospital has year outside 2019 or 2020"
        df9<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df9))
      }else if(as.Date(linelist[i,]$date_admission_hospital, "%d.%m.%Y")> (Sys.Date()+1)){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_admission_hospital is in the future"
        df10<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df10))
      }
    }
    
    if(linelist[i,]$date_confirmation!=""){
      if(substr(linelist[i,]$date_confirmation,3,3)!="." | substr(linelist[i,]$date_confirmation,6,6)!="."){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_confirmation not in DD.MM.YYYY format"
        df11<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df11))
      } else if(!(as.numeric(substr(linelist[i,]$date_confirmation,4,5)) %in% 1:12)){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_confirmation has month outside 1-12 range"
        df12<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df12))
      }else if(!(as.numeric(substr(linelist[i,]$date_confirmation,7,10)) %in% c(2019,2020))){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_confirmation has year outside 2019 or 2020"
        df13<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df13))
      }else if(as.Date(linelist[i,]$date_confirmation, "%d.%m.%Y")> (Sys.Date()+1)){
        ID<-linelist[i,1]
        date.conf<-as.character(linelist[i,13])
        error<-"date_confirmation is in the future"
        df14<-data.frame(ID, date.conf, error)
        error.df<<-as.data.frame(rbind(error.df, df14))
      }
    }
    if(hubei_linelist_flag==0){
      if(linelist[i,]$travel_history_dates!=""){
        if(substr(linelist[i,]$travel_history_dates,3,3)!="." | substr(linelist[i,]$travel_history_dates,6,6)!="."){
          ID<-linelist[i,1]
          date.conf<-as.character(linelist[i,13])
          error<-"travel_history_dates not in DD.MM.YYYY"
          df15<-data.frame(ID, date.conf, error)
          error.df<<-as.data.frame(rbind(error.df, df15))
        } else if(str_detect(linelist[i,]$travel_history_dates,"-") & substr(linelist[i,]$travel_history_dates,12,12)!="-"){
          ID<-linelist[i,1]
          date.conf<-as.character(linelist[i,13])
          error<-"travel_history_dates range not in DD.MM.YYYY - DD.MM.YYYY format"
          df23<-data.frame(ID, date.conf, error)
          error.df<<-as.data.frame(rbind(error.df, df23))
        } else if (!str_detect(linelist[i,]$travel_history_dates,"-") & nchar(as.character(linelist[i,]$travel_history_dates))>10){
          ID<-linelist[i,1]
          date.conf<-as.character(linelist[i,13])
          error<-"travel_history_dates has additional information beyond just date"
          df.length<-data.frame(ID, date.conf,error)
          error.df<<-as.data.frame(rbind(error.df, df.length))
        } else if(!(as.numeric(substr(linelist[i,]$travel_history_dates,4,5)) %in% 1:12)){
          ID<-linelist[i,1]
          date.conf<-as.character(linelist[i,13])
          error<-"travel_history_dates has month outside 1-12 range"
          df16<-data.frame(ID, date.conf, error)
          error.df<<-as.data.frame(rbind(error.df, df16))
        }else if(!(as.numeric(substr(linelist[i,]$travel_history_dates,7,10)) %in% c(2019,2020))){
          ID<-linelist[i,1]
          date.conf<-as.character(linelist[i,13])
          error<-"travel_history_dates has year outside 2019 or 2020"
          df17<-data.frame(ID, date.conf, error)
          error.df<<-as.data.frame(rbind(error.df, df17))
        }else if(as.Date(linelist[i,]$travel_history_dates, "%d.%m.%Y")> (Sys.Date()+1)){
          ID<-linelist[i,1]
          date.conf<-as.character(linelist[i,13])
          error<-"travel_history_dates is in the future"
          df18<-data.frame(ID, date.conf, error)
          error.df<<-as.data.frame(rbind(error.df, df18))
        }
      }
      
      if(linelist[i,]$date_onset_symptoms!="" & linelist[i,]$date_confirmation!="" &
         substr(linelist[i,]$date_confirmation,3,3)=="." & substr(linelist[i,]$date_confirmation,6,6)=="." &
         substr(linelist[i,]$date_onset_symptoms,3,3)=="." & substr(linelist[i,]$date_onset_symptoms,6,6)=="."){
        onset<-as.Date(linelist[i,]$date_onset_symptoms, "%d.%m.%Y")
        confirm<-as.Date(linelist[i,]$date_confirmation, "%d.%m.%Y")
        if(confirm < onset){
          ID<-linelist[i,1]
          date.conf<-as.character(linelist[i,13])
          error<-"date_confirmation preceeds date_onset_symptoms"
          df24<-data.frame(ID, date.conf, error)
          error.df<<-as.data.frame(rbind(error.df, df24))
        }
      }
    }
  }
  error.df2<-error.df
  if(hubei_linelist_flag==1){
    error.df2<<-error.df[order(error.df$ID),]
    error.df2$list<-"hubei"
    write.csv(error.df2, paste0(out_dir,"/hubei_flags_long_", Sys.Date(),".csv"), row.names=FALSE)
  } else{
    error.df2<<-error.df[order(error.df$ID),]
    error.df2$list<-"outside hubei"
    write.csv(error.df2, paste0(out_dir,"/outside_hubei_flags_long_", Sys.Date(),".csv"), row.names=FALSE)
  }
}

