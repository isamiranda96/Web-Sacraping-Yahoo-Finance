library(forecast)
library(TSstudio)
library(forecastHybrid)
library(reshape)
library(Hmisc)
library(prophet)
library(imputeTS)
library(scales)
library(dplyr)
library(lubridate)

#cargamos la data

sniim_data <- read.csv("data_sniim.csv", encoding = "latin1")

yahoo_data <- read.csv("data_yahoo.csv")

#estandarizamos

sniim <- sniim_data%>%
  filter(Producto == "Pollo entero",
         Marca == "Bachoco")%>%
  mutate(Fecha = format(dmy(Fecha), "%Y-%m-%d"),
         Semana = isoweek(Fecha),
         Año = isoyear(Fecha),
         Precio = as.numeric(gsub("\\$","",`Precio...kg.`)))%>%
  filter(Semana < 53)%>%
  group_by(Año, Semana, Fecha)%>%
  summarise(PrecioPollo = mean(Precio, na.rm = T))%>%
  arrange(Año,Semana, Fecha)

yahoo <- yahoo_data %>%
  mutate(date=format(as.Date(date), "%Y-%m-%d"))%>%
  cast(., date ~ symbol, value="close")%>%
  dplyr::rename(Fecha=date)

df_final <- left_join(sniim, yahoo)%>%
  group_by(Año, Semana)%>%
  summarise(PrecioPollo = mean(PrecioPollo),
            PrecioMaiz = mean(Corn, na.rm = T),
            PrecioSoya = mean(Soy, na.rm = T),
            PrecioTrigo = mean(Wheat, na.rm = T))%>%
  na_mean() #sustituimos los valores faltantes por el promedio de la columna

#visualizamos nuestras variables para proceder a normalizarlas

hist(df_final$PrecioPollo)
hist(df_final$PrecioSoya)
hist(df_final$PrecioMaiz)
hist(df_final$PrecioTrigo)

#Normalizamos nuestras variables transformandolas con utilizando el metodo BoxCox

lam1 <- BoxCox.lambda(df_final$PrecioPollo, "guerrero")
lam2 <- BoxCox.lambda(df_final$PrecioMaiz, "guerrero")
lam3 <- BoxCox.lambda(df_final$PrecioSoya, "guerrero")
lam4 <- BoxCox.lambda(df_final$PrecioTrigo, "guerrero")


df_trans <- df_final %>%
      mutate(PrecioPollo = BoxCox(PrecioPollo, lam1),
              PrecioMaiz = BoxCox(PrecioMaiz, lam2),
              PrecioSoya = BoxCox(PrecioSoya, lam3),
              PrecioTrigo = BoxCox(PrecioTrigo, lam4))


#visualizamos nuevamente

hist(df_trans$PrecioPollo)
hist(df_trans$PrecioSoya)
hist(df_trans$PrecioMaiz)
hist(df_trans$PrecioTrigo)


#convertimos nuestro dataset en un objeto time series

ts_sniim <- ts(df_trans[,-c(1:2)], freq=52, start=c(2015,1), end=c(2020,37))

InvBoxCox(tail(df_trans),lam1)
InvBoxCox(tail(ts_sniim),lam1)

#visualizamos nuestra data para proceder a crear el modelo

ts_info(ts_sniim)

ts_plot(ts_sniim)

ts_decompose(ts_sniim)

ts_seasonal(ts_sniim)

ts_heatmap(ts_sniim)

ts_lags(ts_sniim)


#------CREACION DE MODELO 1 ARIMA----------


#dividimos nuestra data en training y test sets para generar el modelo

sniim_split <- ts_split(ts_sniim, sample.out = 60) 


modelo <- auto.arima(sniim_split$train[,1], #seleccionamos nuestra variable objetivo que es el precio del pollo
                     xreg = sniim_split$train[,2:4], #utilizamos como regresores los otros 4 commodities
                     seasonal = TRUE,
                     stepwise = FALSE, 
                     approximation = FALSE,
                     D = 1)

forecast_sniim <- forecast(modelo, h=67, xreg = sniim_split$test[,2:4])

#evaluamos metricas del modelo
accuracy(forecast_sniim, sniim_split$test[,1])

#regresamos variables a su estado original
forecast_sniim$fitted <- InvBoxCox(forecast_sniim$fitted, lam1)
forecast_sniim$mean <- InvBoxCox(forecast_sniim$mean, lam1)

#graficamos modelo
test_forecast(forecast.obj = forecast_sniim, train = InvBoxCox(sniim_split$train[,1],lam1),
              actual = InvBoxCox(ts_sniim[,1],lam1), test = InvBoxCox(sniim_split$test[,1], lam1))


#-----------CREACION DE MODELO 2 PROPHET----------

m2_train <- df_trans[1:231,] %>%
  ungroup()%>%
  mutate(ds = as.Date(paste(Año,Semana, 1, sep="-"), "%Y-%W-%w"))%>%
  select(ds, y=PrecioPollo, PrecioMaiz, PrecioSoya, PrecioTrigo)


m2_test <- df_trans[232:291,] %>%
  ungroup()%>%
  mutate(ds = as.Date(paste(Año,Semana, 1, sep="-"), "%Y-%W-%w"))%>%
  select(ds, y=PrecioPollo, PrecioMaiz, PrecioSoya, PrecioTrigo)

df_test <- df_trans %>%
  ungroup()%>%
  mutate(ds = as.Date(paste(Año,Semana, 1, sep="-"), "%Y-%W-%w"))%>%
  select(ds, y=PrecioPollo, PrecioMaiz, PrecioSoya, PrecioTrigo)


modelo2 <- prophet(growth = "linear",
                   mcmc.samples = 0,
                   interval.width = 0.95,
                   seasonality.mode = "multiplicative",
                   changepoint.prior.scale = 0.6, #esto fue lo ultimo en cambiar 0.6
                   seasonality.prior.scale = 0.6, #esto fue lo ultimo en cambiar 0.4
                   changepoint.range = 0.98,
                   yearly.seasonality = F)%>%
  add_seasonality(.,name = "yearly", period = 52, fourier.order = 25)%>%
  add_regressor(., "PrecioMaiz")%>%
  add_regressor(., "PrecioSoya")%>%
  add_regressor(., "PrecioTrigo")%>%
  fit.prophet(.,m2_train)


m2.cv <- cross_validation(modelo2, initial = 52, period = 52, horizon = 52, units = 'weeks')

performance_metrics(m2.cv)

futuro <-  make_future_dataframe(modelo2, periods = 67, freq = "week", include_history = FALSE)%>%
    left_join(., m2_test[,-2])%>%
    na_mean()

prediccion <- predict(modelo2, futuro)

prediccion$yhat <- InvBoxCox(prediccion$yhat, lam1)
prediccion$yhat_lower <- InvBoxCox(prediccion$yhat_lower, lam1)
prediccion$yhat_upper <- InvBoxCox(prediccion$yhat_upper, lam1)
modelo2$history$y <- InvBoxCox(modelo2$history$y,lam1)


dyplot.prophet(modelo2, prediccion, main = "Forecast de Precios de Pollo",
               uncertainty = TRUE)

pred <- prediccion %>%
    left_join(df_test, by=c("ds"))%>%
    select(ds, PrecioPollo=y, PrecioPrediccion=yhat)%>%
    mutate(Año = year(ds), #regresamos la data a que sea semanal
           Semana = week(ds),
           PrecioPollo = InvBoxCox(PrecioPollo, lam1))%>% #regresamos valor a su estado original
    select(-ds)%>%
    na_mean()%>%
    mutate(error_porcentual = round(abs(PrecioPrediccion - PrecioPollo)/PrecioPollo, digits=2))%>%
    select(Año, Semana, PrecioPollo, PrecioPrediccion, error_porcentual)

mean(pred$error_porcentual)

#----- backtesting con prophet----------


backtest <- df_trans %>%
  ungroup()%>%
  mutate(ds = as.Date(paste(Año,Semana, 1, sep="-"), "%Y-%W-%w"))%>%
  select(ds, y=PrecioPollo, PrecioMaiz, PrecioSoya, PrecioTrigo)


futuro <-  make_future_dataframe(modelo2, periods = 66, freq = "week")%>%
  left_join(., backtest[,-2])%>%
  na_mean()

prediccion <- predict(modelo2, futuro)

prediccion$yhat <- InvBoxCox(prediccion$yhat, lam1)
prediccion$yhat_lower <- InvBoxCox(prediccion$yhat_lower, lam1)
prediccion$yhat_upper <- InvBoxCox(prediccion$yhat_upper, lam1)
modelo2$history$y <- InvBoxCox(modelo2$history$y,lam1)


dyplot.prophet(modelo2, prediccion, main = "Forecast de Precios de Pollo",
               uncertainty = TRUE)


df_backtest <- prediccion %>%
  left_join(., backtest[,c("ds","y")])%>%
  select(ds, PrecioPollo=y, PrecioPrediccion=yhat)%>%
  mutate(Año = year(ds), #regresamos la data a que sea semanal
    Semana = week(ds),
    PrecioPollo = InvBoxCox(PrecioPollo, lam1))%>% #regresamos valor a su estado original
  select(-ds)%>%
  na_mean()%>%
  mutate(error_porcentual = round(abs(PrecioPrediccion - PrecioPollo)/PrecioPollo, digits=2))%>%
  select(Año, Semana, PrecioPollo, PrecioPrediccion, error_porcentual)


mean(df_backtest$error_porcentual[df_backtest$Semana > 23])
mean(df_backtest$error_porcentual[df_backtest$Semana <= 23])

write.csv(df_backtest, "data_backtest.csv", row.names = F)
