install.packages('forecast')
library(forecast)
contributions <- c(5,6,3,4,2,0,0,5,7,5,2,1,2,3,2,4,5,6,5,2,5,8,
                   5,5,0,1,4,5,2,2,3,5,6,8,7,6,5,8,5,7,6,2,1)
contributions.ts <- ts(contributions, frequency = 4, start = c(2010,2))
contributions.ts
autoplot(contributions.ts, xlab = "Year", ylab = "Number of Tips")

# Decompose it
contributions.comp <- decompose(contributions.ts)
contributions.comp
# Access components
contributions.comp$trend
contributions.comp$seasonal
contributions.comp$random
autoplot(contributions.comp)
#The output shows four plots of our closing price data, which are:
#data : original plot of the data
#Trend : long term movements in the mean. 
#Seasonal : repetitive seasonal fluctuation of the data. The number of articles 
#tended to reach the highest in Qtr3 and the lowest in Qtr2. 
# The remainder component shown in the bottom panel is what is left over when 
#the seasonal and trend-cycle components have been subtracted from the data.
#The grey bars to the right of each panel show the relative scales of the 
#components. Each grey bar represents the same length but because the plots 
#are on different scales, the bars vary in size. The large grey bar in the 
#bottom panel shows that the variation in the remainder component is small 
#compared to the variation in the data, which has a bar about one quarter the 
#size. If we shrunk the bottom three panels until their bars became the same 
#size as that in the data panel, then all the panels would be on the same scale.

#Forecast Method
#a. The Mean Method
#Assuming every piece of data in a time series is equally useful to predict 
#all future values, we use the average of the time series to represent the 
#forecasts.
# Use meanf() to forecast quarterly contributions in 2021
contributions.fc <- meanf(contributions.ts, h=4)

# Plot and summarize the forecasts
autoplot(contributions.fc,xlab = "Year", ylab = "Contributions")
summary(contributions.fc)
#The output shows that, Aaron will write four articles every quarter in 2021

#b. The Naïve Method
#When using this method, the forecast for a given period is the value of the 
#previous period. The naïve method seems to work better with data reported on 
#a daily or weekly basis or in situations.
#This method provides a useful benchmark for other forecasting methods.
# Use naive() to forecast contributions in 2021
contributions.fc <- naive(contributions.ts, h=4)

# Plot and summarize the forecasts
autoplot(contributions.fc)
summary(contributions.fc)
#Aaron's forecasted number of articles in the first quarter of 2021 is 
#about one, with a 95% prediction interval [0, 5]. That is to say, we are 
#95% confident that the true number is between 0 and 5.

#c. The Simple Moving Average Method
#We compute an average by adding up a set of data and dividing the sum by 
#the number of items in the set. To "move" the average, we remove the first 
#item in the set, append a new item, and then average the set. When we computer 
#moving averages for a time series, the averages form a new time series. The 
#new time series becomes flat because the calculation process removes the rapid 
#fluctuations. The moving average method works best when the time series is 
#trending.
install.packages('smooth')
install.packages("scales")
library(smooth)
contributions <- c(5,6,3,4,2,0,0,5,7,5,2,1,2,3,2,4,5,6,5,2,5,8,
                   5,5,0,1,4,5,2,2,3,5,6,8,7,6,5,8,5,7,6,2,1)
contributions.ts <- ts(contributions, frequency = 4, start = c(2010,2))
contributions.comp <- decompose(contributions.ts)
autoplot(contributions.comp)

# Use sma() to forecast number of Aaron's contributions in 2021
contributions.fc <- sma(contributions.ts, order=4, h=4,silent=FALSE)
#presents the forecasts of the number of articles Aaron will write in 2021. 
#The solid blue line represents the point forecasts for the four quarters in 
#2021. The plot of the moving averages (i.e., the fitted values) is smoother 
#than the original series. When we increase the value of N, the plot of the 
#moving average becomes flatter. The plot does not illustrate prediction 
#intervals, but we can find the lower bound and upper bound from fc$lower and 
#fc$upper, respectively.

# Print model summary
summary(contributions.fc)
#The model with the minimum value of the AIC is often the best model for 
#forecasting. As with the AIC, the AICc (corrected AIC) should be minimised.
#As with the AIC, minimising the BIC is intended to give the best model.

# Print the forecasts
fc <- forecast(contributions.fc)
print(fc)
#Aaron's forecasted number of articles in the first quarter of 2021 is about 
#four, with a 95% prediction interval [0, 9].

#Assessing Forecast Accuracy
#We used three methods to forecast the numbers of Aaron's contributions in 
#2021. To determine which method made a good forecast, we measure forecast 
#accuracies. We can calculate forecast accuracy by analyzing the forecast 
#errors, which are the differences between the actual future values and the 
#corresponding predicted values. The three widely accepted measures are the 
#mean absolute deviation (MAD), the mean absolute percentage error (MAPE), 
#and the root mean squared error (RMSE). 

#a. The Mean Absolute Deviation (MAD)
#The mean absolute deviation (MAD) measures forecast accuracy by averaging 
#the absolute values of the forecast errors.
#b. The Mean Absolute Percent Error (MAPE)
#Different time series may have different units, for example, million dollars 
#and gallon. When comparing forecasting methods applied to multiple time series 
#with different units, we can use the mean absolute percentage error (MAPE)
#The Root Mean Squared Error (RMSE)
#The root mean squared error (RMSE), one of the most meaningful measures, is 
#the square root of the average of squared errors. We do not want large errors 
#in the forecast; therefore, we want the measure to be sensitive to
#large errors.
contributions <- c(5,6,3,4,2,0,0,5,7,5,2,1,2,3,2,4,5,6,5,2,5,8,
                   5,5,0,1,4,5,2,2,3,5,6,8,7,6,5,8,5,7,6,2,1)
contributions.ts <- ts(contributions, frequency = 4, start = c(2010,2))

# Use meanf() to forecast contributions in 2021
contributions.fc <- meanf(contributions.ts, h=4)
summary(contributions.fc)

# Examining the Residuals
#The "residuals" in a time series model are what is left over after fitting a 
#model. For many (but not all) time series models, the residuals are equal to 
#the difference between the observations and the corresponding fitted values.
#Residuals are useful in checking whether a model has adequately captured the 
#information in the data. A good forecasting method will yield residuals with 
#the following properties:
#The residuals are uncorrelated. If there are correlations between residuals, 
#then there is information left in the residuals which should be used in 
#computing forecasts.
#The residuals have zero mean. If the residuals have a mean other than zero, 
#then the forecasts are biased.
#Any forecasting method that does not satisfy these properties can be improved.
checkresiduals(contributions.fc) 
#presents the time series plot, the corresponding ACF, and the histogram. 
#The time series plot reveals that the residuals are not random. If one value 
#in the series is greater than the mean, the next value is likely greater than 
#the mean. The ACF plot shows the first spike is outside the blue lines, term
#which means some information in the residuals is useful in forecasting. The 
#histogram has two peaks; therefore, the residuals have a bimodal distribution.

#ACF: Autocorrelation occurs when the residuals of a regression model are not 
#independent of each other. The x-axis corresponds to the different lags of 
#the residuals (i.e., lag-0, lag-1, lag-2, etc.). Whereas the y-axis shows 
#the correlation of each lag. Finally, the dashed blue line represents the 
#significance level. After the lag-0 correlation, the subsequent correlations 
#drop quickly to zero and stay (mostly) between the limits of the significance 
#level (dashed blue lines). Therefore, we can conclude that the residuals of 
#this model meet the assumption of no autocorrelation.

#The p-value is 0.03228. If we select a significance level of 0.05, we have 
#much room for improvement to the forecasting method.
