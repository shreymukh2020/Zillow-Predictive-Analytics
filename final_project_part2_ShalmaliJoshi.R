# Loading all libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forecast)
library(tseries)
library(astsa)
library(Metrics)

setwd("C:/Users/shalm/OneDrive - Georgia State University/Documents/R/Predictive_Analytics/Final Projects/Zillow/")

# Reading the data
train <- read.csv("State_time_series.csv")

# Separating the date format
library(lubridate)
train$Date <- as.Date(train$Date, format = "%Y-%m-%d")
train$year <- year(train$Date)
summary(train)
hist(train$ZHVIPerSqft_AllHomes)

# Checking for missing values if any
colSums(is.na(train))
# There are many unnecessary columns with multiple NA values

# Let us do data cleaning

# Dropping all unnecessary columns

train <- subset(train, select = -c(DaysOnZillow_AllHomes,InventorySeasonallyAdjusted_AllHomes,InventoryRaw_AllHomes,MedianListingPricePerSqft_1Bedroom,MedianListingPricePerSqft_2Bedroom,MedianListingPricePerSqft_3Bedroom,MedianListingPricePerSqft_4Bedroom,MedianListingPricePerSqft_5BedroomOrMore,MedianListingPricePerSqft_AllHomes,MedianListingPricePerSqft_CondoCoop,MedianListingPricePerSqft_DuplexTriplex))
train <- subset(train, select = -c(MedianRentalPricePerSqft_4Bedroom,MedianRentalPricePerSqft_5BedroomOrMore,MedianRentalPricePerSqft_AllHomes,MedianRentalPricePerSqft_CondoCoop,MedianRentalPricePerSqft_DuplexTriplex,MedianRentalPricePerSqft_MultiFamilyResidence5PlusUnits,MedianRentalPricePerSqft_SingleFamilyResidence,MedianRentalPricePerSqft_Studio,MedianRentalPrice_1Bedroom,MedianRentalPrice_2Bedroom,MedianRentalPrice_3Bedroom,MedianRentalPrice_4Bedroom,MedianRentalPrice_5BedroomOrMore,MedianRentalPrice_AllHomes,MedianRentalPrice_CondoCoop))
train <- subset(train, select = -c(PctOfHomesDecreasingInValues_AllHomes,PctOfHomesIncreasingInValues_AllHomes,PctOfHomesSellingForLoss_AllHomes,PctOfListingsWithPriceReductionsSeasAdj_AllHomes,PctOfListingsWithPriceReductionsSeasAdj_CondoCoop,PctOfListingsWithPriceReductionsSeasAdj_SingleFamilyResidence,PctOfListingsWithPriceReductions_AllHomes,PctOfListingsWithPriceReductions_CondoCoop,PctOfListingsWithPriceReductions_SingleFamilyResidence,PriceToRentRatio_AllHomes))
train <- subset(train, select =-c(ZRI_AllHomes,ZRI_AllHomesPlusMultifamily,ZriPerSqft_AllHomes,Zri_MultiFamilyResidenceRental,Zri_SingleFamilyResidenceRental))	
train <- subset(train,select=-c(MedianListingPricePerSqft_SingleFamilyResidence,MedianListingPrice_1Bedroom,MedianListingPrice_2Bedroom,MedianListingPrice_3Bedroom,MedianListingPrice_4Bedroom,MedianListingPrice_5BedroomOrMore,MedianListingPrice_AllHomes,MedianListingPrice_CondoCoop,MedianListingPrice_DuplexTriplex,MedianListingPrice_SingleFamilyResidence,MedianPctOfPriceReduction_AllHomes,MedianPctOfPriceReduction_CondoCoop,MedianPctOfPriceReduction_SingleFamilyResidence,MedianPriceCutDollar_AllHomes,MedianPriceCutDollar_CondoCoop,MedianPriceCutDollar_SingleFamilyResidence,MedianRentalPricePerSqft_1Bedroom,MedianRentalPricePerSqft_2Bedroom,MedianRentalPricePerSqft_3Bedroom,MedianRentalPrice_DuplexTriplex,MedianRentalPrice_MultiFamilyResidence5PlusUnits,MedianRentalPrice_SingleFamilyResidence,MedianRentalPrice_Studio))

# Data summary statistics
summary(train)
dim(train)
str(train)
colnames(train)
head(train, n = 10)
tail(train, n = 5)

colSums(is.na(train))

# Omit NA values from ZHVI_AllHomes
train <- na.omit(train, cols=ZHVI_AllHomes)
# Data Visualization 

# Plotting Date against ZHVI_All Homes Data
train %>% group_by(Date) %>% summarise(sqft = sum(ZHVI_AllHomes)) %>% ggplot(aes(x = Date, y = sqft)) + geom_line() + scale_y_log10() + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 weeks") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(RColorBrewer)
Year_state<-aggregate(ZHVI_AllHomes ~year+RegionName,train,mean)
pal<-rep(brewer.pal(10, "BrBG"),5)


ggplot(Year_state, aes(group = RegionName ))+
  geom_line(aes(x=year,y=ZHVI_AllHomes,color=RegionName), alpha=0.5, show.legend=F)+
  scale_color_manual(values=pal)+
  labs(title="The Growth of House Price in US", x=NULL,
       subtitle="\t Increase in ZHVI_Allhomes from 1996 to 2017")+
  theme(panel.background=element_rect(fill = "white"),
        plot.title=element_text(vjust=3, size=15),
        panel.grid.major=element_line(color = "gray90"))

# Observations:
#a) Real estate market was at its peak in 2005 with stability in IT, no recession or wars. 
#b) It crashed miserably in 2008 and continued to remain low till 2011 due to global recession, financial and oil crisis, bankruptcy of major banks like Lehman Brothers
#c) There was a revive in real estate market post 2012 when it again started going upwards till 2017

train <- na.omit(train, cols=ZHVI_BottomTier)
train <- na.omit(train, cols=ZHVI_TopTier)

ggplot(aes(x=ZHVI_BottomTier, y=ZHVI_TopTier), data = train) +
  geom_point(aes(color=RegionName)) +
  labs(x='Bottom Tier', y='Top tier') +
  ggtitle("Cost Per Square Foot in Dollars")

#Observations:
# a. Across all years, Top tier Real estate market has been in California while Colorado prices make up the bootom tier

train <- na.omit(train, cols=ZHVI_5BedroomOrMore)
train <- na.omit(train, cols=ZHVI_1bedroom)

ggplot(aes(x=ZHVI_5BedroomOrMore, y=ZHVI_1bedroom), data = train) +
  geom_point(aes(color=RegionName)) +
  labs(x='5 Bedroom Price', y='1 Bedroom Price') +
  ggtitle("Cost Per Square Foot in Dollars")

sales_agg<-data.frame(train$Date,train$ZHVI_AllHomes)
Aggregate_Sales<-ts(sales_agg[,-1],frequency =12,start=c(1996,4),end=c(2017,11))
head(Aggregate_Sales)
plot(Aggregate_Sales)

# Plotting the monthly sales
Plot_1 <- plot(Aggregate_Sales,xlab="Year",ylab="Real estate (USD)",main="Zillow Data")
# The above plot definitely shows that the time series is not stationary. Need to decompose the time series to deep-dive. 

#Visualizing the data using time-series decomposition to decompose our time series into three distinct components: trend, seasonality, and noise.
sales_a_decomp <- decompose(Aggregate_Sales)
plot(sales_a_decomp, yax.flip = TRUE)
#a) There is definitely seasonality observed in the data - Peaks in the end of the year and small peaks in the summer months. 
#b) The trend in home prices was at highest in 2005, decreased rapidly from 2008 to 2011 and increased again from 2014 to 2017.

# Checking if the time series is stationary
adf.test(Aggregate_Sales)
# The p value is very high, i.e., 0.5601. Hence, we fail to reject the null hypothesis or can say that the 'series is non-stationary'. 

# We only take difference to take care of the trend 
# Log and Differencing the time series
sales_log <- log(Aggregate_Sales)
sales_diff <- diff(sales_log)
plot(sales_diff)
adf.test(sales_diff)
# As we can see from the plot, we have successfully stationrized the time series. Let's re-confirm with Dickey-Fuller Test. 
# P value is 0.82 indicating we can reject the null hypothesis 'series is non-stationary'. Thus, the time series is stationary after the transformation. 

# Plotting the ACF and PACF using the differenced series find the AR and MA terms.
acf2(sales_diff)
#a) This seems to be a seasonal arima model. The ACF spikes at 2 places, while, the PACF has a spike at lag 1.

# Plotting the ACF and PACF using the differenced series find the AR and MA terms.
acf(sales_diff,lag.max = 12)
pacf(sales_diff,lag.max = 12)

#a) As per the ACF plot, it seems to be an AR(1) model. PACF spikes at 0 position, indicating an MA(0) model
# Further the seasonal pattern is 12 months. Hence, the order of the model as as below; 

fit <- arima(sales_diff, c(1, 0, 0),seasonal = list(order = c(0, 1, 0), period = 12))
fit

fit2 <- sarima(sales_diff,2,1,0)
fit2
#a. The Standardized residuals show some pattern until end of 2005 where the sales are highest. They again decrease at start of 2011 and then remain constant. Since there is no continuous pattern, it resembles white noise.
#b. Similarly, ACF also resembles the Standardized Residuals in terms of white noise.
#c. QQ plot has few outliers in the beginning but otherwise has a good fit
#d. The P-values are below the significance level.

tsdiag(fit,gof.lag = 52)
#a) There is no clear pattern for standardized residuals (resemble white noise).
#b) ACF of the residuals (are within the limits) and resemble ACF of white noise.
#c) The p-values are above the significance levels. 

# Predicting the sales on training data
pred_y <- forecast(fit,12)
pred_y
plot(pred_y)
print('Non-stationary series: Yearly')

pred_m <- forecast(fit,52)
pred_m
plot(pred_m)


print('Non-stationary series')
      
#Sales forecasting on training data
sarima.for(sales_diff, 12, 2, 1, 2)




