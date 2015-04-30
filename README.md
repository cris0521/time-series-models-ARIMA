# time-series-models-ARIMA
R code on time series model/ ARIMA

setwd("F:/data")
x=read.table("F:/data/nasdaq2001-2014.txt",header=T) #NASDAQ composite index from Jan 2001 to Dec 2014
x1=x[,7] # extract adjusted close index
par(mfrow=c(1,2)) 
x2=x[,c(1,7)] # both time and adjusted close index
x3=diff(log(x1)) # log return series 
x4=c[x2$Date,x3] # time and log return
x2$Date=as.Date(x2$Date,"%m/%d/%Y" )
plot(AdjClose ~ Date, x2, xaxt = "n", type = "l",xlab="(a)") # plot the adjusted close index 
axis(1, x2$Date, format(x2$Date, "%m/%d/%Y"), cex.axis = .9) 

r=list(Date=x2[1:3520,], logreturn=x3)
z=data.frame(r$Date,r$logreturn)
return=z[,c(1,3)]
return$Date=as.Date(return$Date,"%Y/%m/%d")
plot(r$logreturn~Date,return,xaxt="n",xlab="(b)",type='l') # plot log return series
axis(1,return$Date,format(return$Date,"%Y/%m/%d"),cex.axis=.9)

adfTest(x3) # check unit root 
acf(x3,main='autocorrelation of log returns', xlab='Lag (a)') 
pacf(x3,main='partial ACF of log returns', xlab='Lag (b)')
