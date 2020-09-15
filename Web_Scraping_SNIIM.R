library(dplyr)
library(rvest)
library(magrittr)
library(lubridate)
library(htmltab)
library(XML)
library(RCurl)


link <- read_html("http://www.economia-sniim.gob.mx/nuevo/Home.aspx?opcion=/SNIIM-Pecuarios-Nacionales/MenAve.asp")

linkiframe <- "http://www.economia-sniim.gob.mx/SNIIM-Pecuarios-Nacionales/e_SelEnt.asp?"


submit_form2 <- function(session, form){
  url <- XML::getRelativeURL(form$url, session$url)
  url <- paste(url,'?',sep='')
  values <- as.vector(rvest:::submit_request(form)$values)
  att <- names(values)
  if (tail(att, n=1) == "NULL"){
    values <- values[1:length(values)-1]
    att <- att[1:length(att)-1]
  }
  q <- paste(att,values,sep='=')
  q <- paste(q, collapse = '&')
  q <- gsub(" ", "+", q)
  url <- paste(url, q, sep = '')
  html_session(url)
}  


descarga_SNIIM <- function(rango_años){
  
  tabla_final <- data.frame()
  
  año_actual <- year(Sys.Date())
  mes_actual <- month(Sys.Date())
    
  for (a in rango_años) {
  
        for(m in 1:12){
          
          if (!(a == año_actual && m > mes_actual)) {
          
          dia_1 <- as.Date(paste(a,m,1, sep = "-"))
          dia_final <- as.Date(paste(a,m,days_in_month(dia_1), sep = "-"))
          
          del <- day(dia_1)
          
          al <- if_else(dia_final < Sys.Date(), day(dia_final), day(Sys.Date()-1))
          
          session <- rvest::html_session(linkiframe, encoding = "UTF-8")
          
          form <- rvest::html_form(session)
          
          form <- rvest::set_values(form[[2]], 'anio' = a, 
                             'del' = del, 'al' = al,
                             'mes' = m, 'RegPag' = 10000)

          url <- submit_form2(session, form)

          url2 <- RCurl::getURL(enc2utf8(url$url))
          
          
          tabla_temp <-  tryCatch({htmltab::htmltab(doc=enc2utf8(url2),
                    which = "/html/body/div[3]/center/table" ,
                    header = 1 + "//tr/td[@colspan = '7']", #determinamos los 2 niveles de encabezado
                    encoding="UTF-8")},
                    error=function(e){
                      return(NA)
                    })
          
          if(!is.na(tabla_temp)){
            
          tabla_final <- as_tibble(tabla_temp)%>%
            rename(Distribuidor=Header_1)%>%
            rbind(., tabla_final)
          
          }
          
          
          }
      
      }
    
  }
  
  return(tabla_final)
  
}


data_completa <- descarga_SNIIM(rango_años=c(2015:2020))


write.csv(data_completa, file = "data_SNIIM.csv", row.names=F)


