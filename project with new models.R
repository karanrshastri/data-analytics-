library(xts)
library(forecast)
library(astsa)
library(zoo)
library(stats)
library(tseries)
library(aTSA)
par(mfrow=c(1,1))
projectData<-read.csv("C:/Users/lenovo1/Desktop/projectdata.csv")
torontoData<-data.frame(Toronto=projectData[,8])
torontoData.ts<-ts(torontoData)
new_toronto<-torontoData.ts[c(111073:121992)] #Reducing Data

find.freq <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  return(period)
}


find.freq(new_toronto)
torontoData.ts<-ts(new_toronto,frequency=24)
plot(torontoData.ts,main="Original Data")
adf.test(new_toronto)
acf(new_toronto)
plot(decompose(torontoData.ts))


#Identify Outlier
boxplot.stats(torontoData.ts)
boxplot(torontoData.ts)

#Subtracting seasonal component
decomp<-stl(torontoData.ts,s.window = "periodic")
deseaonal_data<-seasadj(decomp)
plot(decomp)


training<-torontoData.ts[c(1:8784)]
testing<-torontoData.ts[c(8784:10920)]

#Clean data
clean_train<-boxplot(tsclean(training))
clean_test<-boxplot(tsclean(test))



plot(ma(torontoData.ts,order=24),col="red",lwd=1,main="1 Day Moving Average")
plot(ma(torontoData.ts,order=168),col="blue",lwd=1,main="1 Week Moving Average")
plot(ma(torontoData.ts,order=8766),col="orange",lwd=1,main="1 Year Moving Average")
acf2(torontoData.ts)
acf2(diff(torontoData.ts))
acf2(diff(diff(torontoData.ts),24))
acf2(diff(diff(diff(torontoData.ts),24),168))
#torontoData.ts<-ts(torontoData,start=c(1,1),end=c(5359,24),frequency=24)




#ARIMA MODEL
trainingARIMA<-auto.arima(training)
tsdisplay(residuals(trainingARIMA), main = "(2,1,5) Model residuals")
summary(trainingARIMA)
predictARIMA<-forecast(trainingARIMA,h=17544)
accuracy(predictARIMA$mean,testing)


#ETS MODEL
trainingETS<-ets(training)
summary(trainingETS)
predictETS<-forecast(trainingETS,h=17544)
accuracy(predictETS$mean,testing)

#TSLM MODEL
trainingTslm<-tslm(Toronto~trend+season,data=training)
summary(trainingTslm)
predictTslm<-forecast(trainingTslm,h=17544)
accuracy(predictTslm$mean,testing)

#Holt
training_Holt<-holt(training, type=c("additive", "multiplicative"), plot=TRUE)
summary(training_Holt)
predictHolt<-forecast(training_Holt,h=100)
plot(forecast(training_Holt, h=100), main="Holt forecast")

#HW


#NN MODEL
NN<-nnetar(training)
accuracy(NN)
plot(forecast(NN, h=100), main="NN forecast for Toronto Demand")
##NN has the least rmse values, good model and forecast.

#Accuracy display of models
results<-resamples(list(trainingARIMA, trainingETS, NN))
summary(results)

