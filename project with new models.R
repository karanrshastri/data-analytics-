library(xts)
library(forecast)
library(astsa)
library(zoo)
library(stats)
par(mfrow=c(1,1))
projectData<-read.csv("C:/Users/lenovo1/Desktop/projectdata.csv")
torontoData<-data.frame(Toronto=projectData[,8])
torontoData.ts<-ts(torontoData)

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
find.freq(torontoData.ts)
torontoData.ts<-ts(torontoData,start=c(1,1),end=c(5359,24),frequency=24)
torontoData.ts<-window(torontoData.ts,start=c(246,1))
plot(decompose(torontoData.ts))
plot(torontoData.ts,main="Original Data")
plot(ma(torontoData.ts,order=24),col="red",lwd=1,main="1 Day Moving Average")
plot(ma(torontoData.ts,order=168),col="blue",lwd=1,main="1 Week Moving Average")
plot(ma(torontoData.ts,order=8766),col="orange",lwd=1,main="1 Year Moving Average")
acf2(torontoData.ts)
acf2(diff(torontoData.ts))
acf2(diff(diff(torontoData.ts),24))
acf2(diff(diff(diff(torontoData.ts),24),168))
torontoData.ts<-ts(torontoData,start=c(1,1),end=c(5359,24),frequency=24)
training<-window(torontoData.ts,start=c(246,1),end=c(4628,24))
testing<-window(torontoData.ts,start=c(4629,1))


#ARIMA MODEL
trainingARIMA<-auto.arima(training)
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

#NN MODEL
trainingNN<-nnetar(training)
nn.forecast<-forecast(trainingNN, h=17544)
plot(nn.forecast)
accuracy(nn.forecast$mean,testing)

#MLP MODEL
require(RSNNS)
trainingMLP<-mlp(training,testing)
plot(trainingMLP)
print(trainingMLP)
mlp.fore<-forecast(trainingMLP,h=17544)
accuracy(mlp.fore$mean,testing)

#KNN MODEL 
cl <- factor(training)
train.knn<-knn(training,testing,cl)
predictKNN<-forecast(train.knn,h=17544)
accuracy(predictKNN$mean,testing)

#SVR and prediction 
require(e1071)
#Problem of X and Y variable

