library(dplyr)
library(jsonlite)
library(pbmcapply)
library(rvest)
library(tm)
library(stringr)
rm(list=ls())

getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

datasets <- list.dirs('../input/bulk-data', full.names = T)[-1]

for(i in datasets){
  
  # get list of all .json-files of dataset
  files <- list.files(i, full.names = T)
  # run through .json-files and extract the text
  df <- pbmclapply(files, function(x){
    df <- try(fromJSON(x))
    if('try-error'%in%class(df)){
      write(x, file="out.txt", append=T)
      return(NULL)
    }else{
      id <- df$id
      if(ifelse(identical(!df$html=="", logical(0)), F, !df$html=="")){
        df <- df$html
      }else if(ifelse(identical(!df$html_with_citations=="", logical(0)), F, !df$html_with_citations=="")){
        df <- df$html_with_citations
      }else if(ifelse(identical(!df$html_lawbox=="", logical(0)), F, !df$html_lawbox=="")){
        df <- df$html_lawbox
      }else if(ifelse(identical(!df$html_columbia=="", logical(0)), F, !df$html_columbia=="")){
        df <- df$html_columbia
      }else if(ifelse(identical(!df$html_harvard=="", logical(0)), F, !df$html_harvard=="")){
        df <- df$html_harvard
      }else if(ifelse(identical(!df$plain_text=="", logical(0)), F, !df$plain_text=="")){
        df <- df$plain_text
      }
      
      df_try <- try(read_html(df))
      if(!'try-error'%in%class(df_try)){
        df_date <- try(html_text(html_node(df_try, css='.date')))
        if(!'try-error'%in%class(df_date)){
          df_date <- stringr::str_extract(df_date, '[0-9]{4}')
        }else{
          df_date <- NA
        }
        df <- try(html_text(df_try))
        if('try-error'%in%class(df)){
          write(x, file="out.txt", append=T)
          return(NULL)
        }
      }
      return(list(id, df_date, df))
    }
  }, mc.cores = 4)
  # collapse data
  df <- do.call(rbind, df)
  # coerce to tibble
  df <- as_tibble(df)
  # rename vars
  names(df) <- c('id', 'year', 'txt')
  # collapse the id list to vec
  if(length(unlist(df$id)) == nrow(df)) df <- mutate(df, id=unlist(id))
  # collapse the year list to vec
  if(length(unlist(df$year)) == nrow(df)) df <- mutate(df, year=unlist(year))
  # get rid of problematic text imports
  df <- filter(df, !lengths(txt)>1)
  # collapse text list to vec
  if(length(unlist(df$txt)) == nrow(df)) df <- mutate(df, txt=unlist(txt))
  
  out <- paste0('../output/00-bulk-data/', gsub('.*\\/', '', i), '.RDS')
  save(df, file = out)
}


