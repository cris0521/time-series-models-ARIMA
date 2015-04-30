# time-series-models-ARIMA
R code on time series model/ ARIMA
#data processing
> setwd("F:/data")
> x=read.table("F:/data/nasdaq2001-2014.txt",header=T)
> x1=x[,7]                # extract adjusted close index
> par(mfrow=c(1,2))
> x2=x[,c(1,7)] # both time and adjusted close index
> x3=diff(log(x1)) # log return series 

# unit root test
> library(fUnitRoots)
> adfTest(x3)

Title:
 Augmented Dickey-Fuller Test

Test Results:
  PARAMETER:
    Lag Order: 1
  STATISTIC:
    Dickey-Fuller: -45.6687
  P VALUE:
    0.01 

Warning message:
In adfTest(x3) : p-value smaller than printed p-value # ADF test statistic value is -45.6687 with p value 0.01. Reject null hypothesis at 5% significance level, which means log return series are stationary  

# auto.arima
> m=auto.arima(x3)
> m
Series: x3 
ARIMA(2,0,2) with zero mean     

Coefficients:
          ar1      ar2     ma1     ma2
      -0.2106  -0.0731  0.1714  0.0007
s.e.   0.2479   0.2589  0.2482  0.2567

sigma^2 estimated as 0.0002404:  log likelihood=9671.96
AIC=-19333.92   AICc=-19333.91   BIC=-19303.09   # we can find that all estimates of parameters are not significant!

# select ARIMA model
> acf(x3,main='autocorrelation of log returns', xlab='Lag (a)')
> pacf(x3,main='partial ACF of log returns', xlab='Lag (b)') # According to time plot of acfs of log returns, we find there are several serial correlations 
we find there are some spikes of PACF at lag 1 as well as 2. Therefore, we can fit the AR(2)model.
> ar=ar(x3,method="mle")
> ar

Call:
ar(x = x3, method = "mle")

Coefficients:
      1        2  
-0.0401  -0.0658  

Order selected 2  sigma^2 estimated as  0.0002404   # In the same time, we fit ar model and the fitted model selected by ar() function automatically 
> ar$aic
        0         1         2         3         4         5         6         7 
15.881170 12.966448  0.000000  1.467643  3.288797  3.836438  5.580218  7.447017 
        8         9        10        11        12 
 8.672546 10.477409 11.978147 10.444731  2.826400 
> ar$order
[1] 2   The output proved that ar(2) model can be fitted. Therefore, we just try ARIMA(2,0,0) model
> model=arima(x3, order=c(2,0,0))
> model

Call:
arima(x = x3, order = c(2, 0, 0))

Coefficients:
         ar1      ar2  intercept
      -0.040  -0.0657      2e-04
s.e.   0.017   0.0170      2e-04

sigma^2 estimated as 0.0002404:  log likelihood = 9672.03,  aic = -19336.06  # All estimates except mean are statistically significant! 

# modify the model
> model=arima(x3, order=c(2,0,0),include.mean=F)
> model

Call:
arima(x = x3, order = c(2, 0, 0), include.mean = F)

Coefficients:
          ar1      ar2
      -0.0399  -0.0655
s.e.   0.0170   0.0170

sigma^2 estimated as 0.0002404:  log likelihood = 9671.66,  aic = -19337.32  # revise the new model and AIC is smaller. Therefore, I think it may be the best model

# check reisduals
> arimaresidual=residuals(model)
> acf(arimaresidual,xlab="(a) Lag")
> qqnorm(arimaresidual,xlab="(b) Theoretical Quantiles")
> qqline(arimaresidual,  probs = c(0.05, 0.95))  most residuals are removed. However, it is not good enough especially residuals follow normal distribution not very well

# forecasting by loop
> x=read.table("F:/data/nasdaq2001-2014.txt",header=T) # within sample + out of sample from Jan 2015 
> x1=x[,7]
> x3=diff(log(x1))
> i=3521
> c=x3[1:3520]
> mloop=model
> l=length(c)
> for (l in c(3520:3550))
{
c[i]=predict(mloop,n.ahead=1)
l=length(c)
mloop=arima(x3[1:i], order=c(2,0,0),include.mean=F)
i=i+1
} # forecast about 30 data and compare them with real data from Jan 2015.
