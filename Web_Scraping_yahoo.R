library(tidyquant)
library(dplyr)


articulos <- c("C=F", "S=F", "KW=F")


#descarga de datos

data <-  tq_get(articulos, from = '2015-01-01',
         to = "2020-09-12",
         get = "stock.prices")

data_final <- data %>%
  mutate(symbol = ifelse(symbol == "S=F", "Soy",
                         ifelse(symbol=="C=F", "Corn", "Wheat")))%>%
  filter(!is.na(close))

#escribimos data en csv
write.csv(data_final, file="data_yahoo.csv", row.names = F)
