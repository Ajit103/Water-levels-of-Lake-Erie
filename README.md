# Water-levels-of-Lake-Erie
The “LakeErie.csv” data gives the year, month and water levels in tens of metres above sea level for Lake Erie from 1967 to 2016. Levels have been somewhat high lately and if water levels get too high, there is a risk of flooding. On the other hand, if water levels get too low, shorelines start to recede.

# Import file
laker<-read_csv(“LakeErie.csv”)

laker$Water.level.meters<- laker$Monthly.Lake.Erie.Levels.1967.01.to.2016.12*10

awl<- mean(laker$Water.level.meters)
awl #average water level = 149.9305 m

# Creating time series object
laker.ts<- ts(laker[,c(-1,-2,-3)], start = c(1967,1), end = c(2016,12), frequency = 12)
# Labeled plot for the water level
plot.ts(laker.ts, xlab="Months since January 1967",
        ylab="Water level in meters",
        main="Water level in Lake Erie from January 1967 to December 2012",
        plot.type = "single", col=("blue")) 
legend("topleft", legend=("Water level"), 
       col=("blue"), lty=1, cex=0.5) 
plot(decompose(laker.ts))

# Moving Average methods
install.packages("TTR") 
library(TTR) 

# Error for MA
ERRORS<-function(data, L){ 
  ma.data<-SMA(data, n=L)
  error<-NULL 
  for (i in 1:length(data)-L){
    error[i]<-data[i+L]-ma.data[i+L-1]
  } 
  error.p<-NULL 
  for(i in 1:length(data)-L){  
    error.p[i]<-abs(data[i+L]-ma.data[i+L-1])/abs(data[i+L])
  } 
  MSE<-mean(error^2)
  MAD<-mean(abs(error))
  MAPE<-mean(error.p)*100
  error.df<-data.frame(errors=c(MSE, MAD, MAPE), row.names=c("MSE", "MAD", "MAPE")) 
  return(error.df)
} 

# Finding MAPE for moving average with every possible lengths
err<-NULL

for (n in 2:599){
  
  e<-ERRORS(laker.ts,n)
  
  err[n]<-e$errors[3]
  
}

# Finding the least error moving average model
err

min(err,na.rm = TRUE)
which.min(err)

Least error MA-2 with MAPE = 4.781118

As the error is least for the model with length 2, 
it is the best moving average model.

# Fitting	an	MA-2	model 
laker.ts.ma2<-SMA(laker.ts, n=2) 

# Plot for best moving average model(MA-2)

plot.ts(cbind(laker.ts,laker.ts.ma2), plot.type="s",  
       col=c("blue","red"),
       ylab="Water level in meters",    
       main="Water level in Lake Erie from January 1967 to December 2012")
legend("topleft", legend=c("Data", "MA-2"), 
       col=c("blue","red"), lty=1, cex=0.5) 

# Exponential smoothing
laker.ses<-HoltWinters(laker.ts, beta=FALSE, gamma=FALSE, l.start=laker.ts[1]) 
laker.ses 

laker.hes<-HoltWinters(laker.ts, gamma=FALSE, l.start=laker.ts[1]) 
laker.hes

laker.hws<-HoltWinters(laker.ts, l.start = laker.ts[1])
laker.hws

# Errors
install.packages("forecast") 
library(forecast) 
# Finding errors
errors<-accuracy(forecast(laker.ses))
errors
      ME     RMSE     MAE         MPE     MAPE      MASE      ACF1
 0.03040251 6.422728 5.15426 -0.07484075 3.463714 0.4491337 0.5312474
 
errorh<-accuracy(forecast(laker.hes))
errorh
     ME     RMSE      MAE       MPE     MAPE    MASE      ACF1
 0.3513506 6.458416 5.134135 0.1603321 3.442516 0.44738 0.5315925

errorhw<-accuracy(forecast(laker.hws))
errorhw
     ME      RMSE     MAE       MPE     MAPE      MASE      ACF1
 0.2435057 4.528022 3.38994 0.1348166 2.304791 0.2953938 0.2866962

The best exponential smoothing model i.e. with Least error is HWS

# PLotting best exponential smoothing - HOlt Winters
plot(laker.hws, xlab="Months since January 1967",
        ylab="Water level in meters",
        main="Water level in Lake Erie from January 1967 to December 2012",
        plot.type = "single", col=("blue")) 
legend("topleft", legend=c("Data", "HWS"), 
       col=c("blue","red"), lty=1, cex=0.5) 

#	Forecasting	the	water	level	in	Lake	Erie	for	the	next	five	months using Holt-WInters
laker.forecast<- forecast(laker.hws, h = 5)
laker.forecast

#               Forecast   Lo 80    Hi 80    Lo 95    Hi 95
Jan 2017       163.7037 157.9043 169.5031 154.8342 172.5732
Feb 2017       166.4990 158.7017 174.2963 154.5741 178.4239
Mar 2017       170.1873 160.7999 179.5746 155.8305 184.5440
Apr 2017       180.3763 169.6242 191.1284 163.9324 196.8203
May 2017       186.1274 174.1585 198.0963 167.8225 204.4323

# Plotting the forecast
plot(laker.forecast, main = "Forecast of water level for the first five months of 2017")
The water level will increase for the next 5 months in Lake Erie.

# Aaccess the quality by plotting residuals
plot(laker.forecast$residuals, ylab="Residuals", 
     main="Residuals for Lake Erie HWS")
abline(h=0, col="red", lty=2)

# Taking 100 random samples
library(dplyr)
rs.laker<-sample_n(laker, 100)

ng categorical variable for water level
rs.laker$wat.lev<- ifelse(rs.laker$Water.level.meters >150 , "high", "low")
#making categorical variable for seasons
rs.laker$Seasons<- ifelse(rs.laker$Month %in% c(12,1,2), "Winter",
                    ifelse(rs.laker$Month %in% c(3,4,5), "Spring",
                           ifelse(rs.laker$Month %in% c(6,7,8), "Summer", "Fall")))
# Making categorical variable for water level
observed<-table(rs.laker$wat.lev,rs.laker$Seasons)
observed
       Fall Spring Summer Winter
high   10     14     24      5
low    11      9      5     22

row.total<-apply(observed, 1, sum)
column.total<-apply(observed, 2, sum)
total<-sum(observed)
expected<-outer(row.total, column.total)/total
expected
      Fall Spring Summer Winter
high 11.13  12.19  15.37  14.31
low   9.87  10.81  13.63  12.69

# Checking Assumptions for chi square test
Counted Data Condition: The data are the	of	high	and	low	water	levels by seasons
Independence Assumption: The counts in the cells are independent of each other as these are the 
                         random sample taken from the original data.
Randomization Condition: The counted individuals are a random sample of the lake Erie data. 
Expected Cell Frequency/Sample Size Condition: Each cell has more than 5 counts for expected values.


# Hypotheses
H0: the water levels are independent of the seasons
HA: the water levels are not independent of the seasons


chisq.test(observed)      
p-value = 2.482e-05<alpha=0.05, reject H0
Water level depends on the season or water level changes with the season.
             
# Creating Stacked Bar Plot
library(reshape2)
library(ggplot2)
o<-data.frame(melt(observed, value.name = "Count"), 
              Distribution=rep("Obs", length(observed)))
e<-data.frame(melt(expected, value.name = "Count"), 
              Distribution=rep("Exp", length(observed)))
laker.reshape<-rbind(o, e)
colnames(laker.reshape)<-c("WaterLevels", "Seasons", "Count", "Distribution")
ggplot(laker.reshape, aes(Distribution, Count, fill=WaterLevels))+
  geom_bar(stat="identity")+
  facet_grid(~Seasons)+
  labs(title="Chi-Square Analysis for water levels of Lake Erie")
