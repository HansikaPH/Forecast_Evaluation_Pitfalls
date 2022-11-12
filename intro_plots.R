library(fable)
library(tsibbledata)
library(tidyverse)
library(lubridate)
library(tsibble)
library(ggpubr)

# plots in the terminology and motivation sections

# compute the errors for all the methods
RMSE <- function (pred, true){
  rmse <- sqrt(mean((true - pred) ^ 2))
  rmse
}


###### figure with forecast horizon, origin
data <- tsibbledata::aus_retail %>% filter(`Series ID`=="A3349849A") %>% filter(year(Month)>=1990)
train_data <- data %>% filter(year(Month)<=2013)
test_data <- data %>% filter(year(Month)>2013)
model <- train_data %>% model(ETS(Turnover))
forecasts <-model %>% forecast(h=60) 
forecasts %>% autoplot(data, level=NULL)


####### Figure with different non-stationarities
trended_data <- tsibbledata::aus_production %>% mutate(trend=slider::slide_dbl(Electricity, mean, .before = 40)) 
p1 <- trended_data%>% autoplot(trend) + ylab("Trended Series") + xlab("Time")#trend 
seasonal_data <- tsibbledata::aus_production %>% mutate(seasonality=difference(log(Beer), lag=1)) %>% filter(year(Quarter)<2000) # seasonality
p2 <-seasonal_data %>% autoplot(seasonality) + ylab("Seasonal Series") + xlab("Time")#seasonality
# unit roots
x <- rnorm(100)
data <- cumsum(x)
data <- data - min(data)
unit_root_data <- data.frame(date=1:100, data=data)
p3 <- ggplot(unit_root_data) + geom_line(aes(x=date, y=data)) + ylab("Unit Root Series") + xlab("Time")
# structural change
data <- tsibbledata::PBS %>% filter(Concession=='Concessional' & Type=='Co-payments'& ATC1=='A' & ATC1_desc == 'Alimentary tract and metabolism' &
                                      ATC2=='A01' & ATC2_desc=='STOMATOLOGICAL PREPARATIONS') %>% filter(year(Month) <= 1997)
data %>% autoplot(Scripts)

data_new <- new_data(data, n=72)
data_new$Scripts <- data$Scripts[1:72] + 10000
data <- data %>% select(-ATC1_desc, -ATC2_desc, -Cost)
data <- bind_rows(data, data_new)
p4 <- data %>% autoplot(Scripts) + ylab("Series with Changepoint") + xlab("Time")
#Heteroscedasticity
x <- rnorm(50)
y<-rnorm(50, sd=4)
data <-c(x,y)
data <- data - min(data)
changepoint_data <- data.frame(date=1:100, data=data)
p5 <- ggplot(changepoint_data) + geom_line(aes(x=date, y=data)) + ylab("Heteroscedastic Series") + xlab("Time")
ggarrange(p1, p2, p3, p4, p5,
          # labels = c("A", "B"),
          ncol = 2, nrow = 3)

#################### Rolling origin vs fixed origin naive forecast
data <- tsibbledata::aus_retail %>% filter(`Series ID`=="A3349589T")
data_train <- data %>% filter(Month < yearmonth("2012 Jan"))
data_test <- data %>% filter(Month >= yearmonth("2012 Jan"))

fit <- data_train %>% model(
  ets = ETS(Turnover))
ets_forecasts <- fit %>% forecast(h=nrow(data_test))
ets_forecasts <- c(rep(NA, nrow(data_train)), ets_forecasts$.mean)
data <- data %>% mutate(rolling_naive=case_when((Month >= yearmonth("2012 Jan")) ~ lag(Turnover)),
                        fixed_naive = case_when((Month >= yearmonth("2012 Jan")) ~ data_train$Turnover[length(data_train$Turnover)]))
data <- data %>% mutate(ets=ets_forecasts)

data_plot <- data %>% filter(Month > yearmonth("1995 Jan"))
data_plot %>% autoplot(Turnover, aes(color="Actual")) +
  geom_line(aes(y=rolling_naive, color="Naive Forecast (Rolling Origin)")) +
  geom_line(aes(y=fixed_naive, color="Naive Forecast (Fixed Origin)")) +
  geom_line(aes(y=ets, color="ETS Forecast")) +
  scale_colour_manual(values=c("Actual" = "black",
                               "Naive Forecast (Rolling Origin)"="red",
                               "Naive Forecast (Fixed Origin)"="green",
                               "ETS Forecast"="blue")) +
  theme(legend.title = element_blank(), text = element_text(size = 15))

RMSE(na.omit(ets_forecasts), data_test$Turnover)
RMSE(na.omit(data$rolling_naive), data_test$Turnover)
RMSE(na.omit(data$fixed_naive), data_test$Turnover)

# zoom into specific part - Horizontal and Vertical Distances of Naive forecast
data_plot <- data %>% filter(Month < yearmonth("2014 Jan") & Month >= yearmonth("2013 Jan"))
data_plot %>% autoplot(Turnover, aes(color="Actual")) +
  geom_line(aes(y=rolling_naive, color="Naive Forecast (Rolling Origin)")) +
  scale_colour_manual(values=c("Actual" = "black",
                               "Naive Forecast (Rolling Origin)"="red",
                               "Naive Forecast (Fixed Origin)"="green",
                               "ETS Forecast"="blue")) +
  theme(text = element_text(size = 15), legend.position = "none")

########## Fixed origin naive forecast on multi-horizon forecasting of random walk series
library(randomForest)
library(nnet)
library(e1071)

set.seed(3)
myTS <- cumsum(rnorm(1000))
plot(myTS, type="l", ylab="share price", xlab="time")

horizon <-60
maxLags <- horizon * 1.25
data_train <- myTS[1:(length(myTS) - horizon)]
data_test <- myTS[(length(myTS) - horizon + 1) : length(myTS)]

naive_forecast <- rep(data_train[length(data_train)], horizon)

data_train_tsibble <- as.data.frame(data_train) %>% mutate(index=seq(as.Date("2015-01-01"), by="day" ,length.out = length(data_train))) %>% as_tsibble(index=index)
fit <- data_train_tsibble %>% model(
  arima = ARIMA(data_train)
)
arima_forecasts <- fit %>% forecast(h=horizon)

myEmb <- function(x, maxLags, usedLags) {
  data.frame(embed(x,maxLags)[,c(usedLags:1),drop=FALSE])
}

embTS <- myEmb(data_train, (maxLags + 1), (maxLags + 1))

modRF <- randomForest(embTS[,-ncol(embTS)], embTS[,ncol(embTS)])
modNN <- nnet(embTS[,-ncol(embTS)], embTS[,ncol(embTS)], size=5, linout=TRUE)
modSVM <- svm(embTS[,-ncol(embTS)], embTS[, ncol(embTS)], linout=TRUE)

recursive_forecast <- function (lags, model){
  predictions <- c()
  for (i in 1:horizon){
    if(i!=1){
      lags <- data.frame(t(lags))
    }
    colnames(lags) <- colnames(embTS)[1:(ncol(embTS) - 1)]
    pred <- predict(model, lags)
    predictions <- c(predictions, pred)
    lags <- c(as.numeric(lags[2:ncol(lags)]), as.numeric(pred))
  }
  predictions
}

final_window <- embTS[nrow(embTS), 2:ncol(embTS)]
predictions_RF <- recursive_forecast(final_window, modRF)
predictions_NN <- recursive_forecast(final_window, modNN)
predictions_SVM <- recursive_forecast(final_window, modSVM)

plot_ts <- myTS[400 : length(myTS)]
allInd <- 1:length(plot_ts)
testInd <- allInd[(length(plot_ts) - horizon + 1):length(plot_ts)]
plot(plot_ts, type="l", xlim=c(0,600), ylim=c(-30, 80), ylab="share price", xlab="time", col="black")
lines(testInd, predictions_RF, col="blue")
lines(testInd, predictions_SVM, col="purple")
lines(testInd, predictions_NN, col="green")
lines(testInd, arima_forecasts$.mean, col="orange")
lines(testInd, naive_forecast, col="red")
legend("topleft", legend=c("Reference", "SVM", "Random Forest", "Neural Network", "ARIMA", "Naive"), col=c("black", "blue", "purple", "green", "orange", "red"), lty=1)

RMSE(predictions_RF, data_test)
RMSE(predictions_SVM, data_test)
RMSE(predictions_NN, data_test)
RMSE(arima_forecasts$.mean, data_test)
RMSE(naive_forecast, data_test)

########## ML method comparison for forecasting
library(randomForest)
library(nnet)
library(e1071)

set.seed(3)
myTS <- cumsum(rnorm(1000))
plot(myTS, type="l", ylab="share price", xlab="time")

myEmb <- function(x, maxLags, usedLags, h=embHor) {
  data.frame(embed(x,maxLags)[,c(usedLags:h),drop=FALSE])	
}

maxLags <- 5
horizon <- 1

trainRatio <- 0.8

embTS <- myEmb(myTS, maxLags, maxLags, horizon)

allInd <- 1:nrow(embTS)
trainInd <- 1:(length(allInd)*trainRatio)
testInd <- allInd[-trainInd]
testInd <- testInd[-(2*maxLags):-1] # we remove 2*maxLags between 

reference <- embTS[testInd,ncol(embTS), drop=TRUE]
naive <- embTS[testInd,ncol(embTS)-1, drop=TRUE]

modRF <- randomForest(embTS[trainInd,-ncol(embTS)], embTS[trainInd,ncol(embTS)])
modNN <- nnet(embTS[trainInd,-ncol(embTS)], embTS[trainInd,ncol(embTS)], size=5, linout=TRUE)
modSVM <- svm(embTS[trainInd,-ncol(embTS)], embTS[trainInd,ncol(embTS)], linout=TRUE)

predRF <- predict(modRF, embTS[testInd,-ncol(embTS)])
predSVM <- predict(modSVM, embTS[testInd,-ncol(embTS)])
predNN <- predict(modNN, embTS[testInd,-ncol(embTS)])


plot(embTS[trainInd,ncol(embTS), drop=TRUE], type="l", xlim=c(0,1000), ylim=c(-30, 80), ylab="share price", xlab="time")
lines(testInd, reference, col="black")
lines(testInd, predRF, col="blue")
lines(testInd, predSVM, col="orange")
lines(testInd, predNN, col="green")
legend("topleft", legend=c("Reference", "Random Forest", "SVM", "Neural Network"), col=c("black", "blue", "orange", "green"), lty=1)

################## ML method comparison with differenced series

library(randomForest)
library(nnet)
library(e1071)
library(fpp3)

set.seed(3)
myTS <- cumsum(rnorm(1000))
plot(myTS, type="l", ylab="share price", xlab="time")

# difference the series for ML models
myTS_diff <- difference(myTS, lag=1, differences = 1)
myTS_diff <- myTS_diff[2: length(myTS_diff)]


myEmb <- function(x, maxLags, usedLags, h=embHor) {
  data.frame(embed(x,maxLags)[,c(usedLags:h),drop=FALSE])	
}

maxLags <- 5
horizon <- 1

trainRatio <- 0.8

embTS_diff <- myEmb(myTS_diff, maxLags, maxLags, horizon)
embTS <- myEmb(myTS[2: length(myTS)], maxLags, maxLags, horizon)

allInd <- 1:nrow(embTS_diff)
trainInd <- 1:(length(allInd)*trainRatio)
testInd <- allInd[-trainInd]
testInd <- testInd[-(2*maxLags):-1] # we remove 2*maxLags between 

reference <- embTS[testInd,ncol(embTS), drop=TRUE]
naive <- embTS[testInd,ncol(embTS)-1, drop=TRUE]

modRF <- randomForest(embTS_diff[trainInd,-ncol(embTS_diff)], embTS_diff[trainInd,ncol(embTS_diff)])
modNN <- nnet(embTS_diff[trainInd,-ncol(embTS_diff)], embTS_diff[trainInd,ncol(embTS_diff)], size=5, linout=TRUE)
modSVM <- svm(embTS_diff[trainInd,-ncol(embTS_diff)], embTS_diff[trainInd,ncol(embTS_diff)], linout=TRUE)

predRF <- predict(modRF, embTS_diff[testInd,-ncol(embTS_diff)])
predSVM <- predict(modSVM, embTS_diff[testInd,-ncol(embTS_diff)])
predNN <- predict(modNN, embTS_diff[testInd,-ncol(embTS_diff)])

# convert the ML model predictions to non-differenced format
predRF_non_differenced <- embTS[testInd, (ncol(embTS) - 1)] + predRF
predSVM_non_differenced <- embTS[testInd, (ncol(embTS) - 1)] + predSVM
predNN_non_differenced <- embTS[testInd, (ncol(embTS) - 1)] + predNN

plot(embTS[trainInd,ncol(embTS), drop=TRUE], type="l", xlim=c(0,1000), ylim=c(-30, 80), ylab="share price", xlab="time")
lines(testInd, reference, col="black")
lines(testInd, predRF_non_differenced, col="blue")
lines(testInd, predSVM_non_differenced, col="orange")
lines(testInd, predNN_non_differenced, col="green")
lines(testInd, naive, col="red")
legend("topleft", legend=c("Reference", "Random Forest", "SVM", "Neural Network", "Naive"), col=c("black", "blue", "orange", "green", "red"), lty=1)


# plotting only the test region
plot(c(testInd[1]:(testInd[1] + 49)), reference[1:50], type="l", col="black", ylab="share price", xlab="time",ylim=c(46, 59))
lines(c(testInd[1]:(testInd[1] + 49)), predRF_non_differenced[1:50], col="blue")
lines(c(testInd[1]:(testInd[1] + 49)), predNN_non_differenced[1:50], col="green")
lines(c(testInd[1]:(testInd[1] + 49)), predSVM_non_differenced[1:50], col="orange")
lines(c(testInd[1]:(testInd[1] + 49)), naive[1:50], col="red")
legend("topleft", legend=c("Reference", "Random Forest", "SVM", "Neural Network", "Naive"), col=c("black", "blue", "orange", "green", "red"), lty=1)


RMSE(predRF_non_differenced, reference)
RMSE(predSVM_non_differenced, reference)
RMSE(predNN_non_differenced, reference)
RMSE(naive, reference)


