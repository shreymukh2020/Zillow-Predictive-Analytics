
# Using Linear model and PCA to model
# Will do for one state at a time starting with Georgia then New york and the California
# For each of the state we will use Bottom tier, medium tier and top tier as dependant variables  



library(tidyverse)
library(timetk)
library(ggplot2)
library(lubridate)
library(broom)
library(lmtest)
state<- read.csv('/Users/namitsrivastava/Documents/Documents/GSU/Predictive Analysis/Final Project/State_time_series.csv')
state_new_york <- state %>%
  filter(RegionName=='Georgia') 

## How is the dependent variable distributed?



state_new_york %>%
  ggplot(aes(x=ZHVI_BottomTier))+
  geom_histogram()




# The dependent variable is bi-modal in distribution.


## Looking at the number of NA values in the data

{r message=FALSE}
na_count <- function(x){sum(is.na(x))}
na_count

names_of_columns_to_keep <- unlist(apply(X=state_new_york,FUN = na_count,MARGIN = 2))
names_of_columns_to_keep <- names_of_columns_to_keep[names_of_columns_to_keep==0]
names_of_columns_to_keep <- names(names_of_columns_to_keep)

names_of_columns_to_keep

## Simple Linear Regression


state_new_york_lm <- state_new_york[names_of_columns_to_keep]

# Remove the RegionName

state_new_york_lm <- state_new_york_lm %>% select(-RegionName)

state_new_york_lm_date <- state_new_york_lm %>% 
  select(-Date)

mod_lm <-lm(data=state_new_york_lm_date,ZHVI_BottomTier~.)





### What are the estimates of these independent variables?

{r message=FALSE}
broom::tidy(mod_lm)


# All the variables in the model are significant in nature.
# This means that all the variables explain the variation in data.
# This can be seen by the p values of each variable in the model.

The independent variables ZHVI_2bedroom and ZHVI_5BedroomOrMore cause a negative effect in the price of lower tiered houses in New York. The variable ZHVI_3bedroom cause a positive effect on the price of lower tiered houses.





### R Squared Values


broom::glance(mod_lm)


# This model explains 99% of the variation in the data as seen by the adjusted r squared metric.


### Collinearity




# Linearity with the dependent variable and collinearity



state_new_york_lm_date %>%
  GGally::ggpairs()+theme(panel.background = element_blank(),panel.grid.major = element_blank())


The variables are highly correlated to each other and with the
dependent variable. This collinearity will cause the variance of the regression coefficients to be high thus making the model
unstable.The failure of this naive model can be estimated by the Variance Inflation Factor.




car::vif(mod_lm)
#car::ncvTest(mod_lm)



#The VIF values are a lot greater than 10. The regression coefficients are poorly estimated due to multicollinearity.#

One way to reduce this collinearity is to decorrelate the variables. One such method is the Principal Component Analysis.



### Fitted values VS Residuals




broom::augment(mod_lm) %>%
  ggplot(aes(x=.fitted,y=.resid))+geom_point(alpha=0.1)+ geom_smooth(method='loess')





lmtest::bptest(mod_lm)




# The p-value is less than 0.05
# We can reject the null hypothesis that the variance of the residuals is constant.


# One of the disadvantages of this model was that the independent variables were highly collinear. We can look at another model where these features are decorrelated.#



## Linear Regression with PCA features


# As the Variance Inflation factor of the previous model was high, we will try to lower it by decorrelating the variables. One way to do that is to use th Principal Component Analysis.



log_data <- log(state_new_york_lm_date[,1:3])
pr.log_data <- prcomp(log_data,scale=T,center=F)
eig <- (pr.log_data$sdev)^2
variance <- eig#100/sum(eig)
cumvar <- cumsum(variance)
eig.data <- data.frame(eig=eig,variance,cumvariance=cumvar)
print(eig.data)






# From the table above, we see that the first two principal components explain about 99% of the variation in the data


# As the number of variables that we have is less, we will use all the three variables that are available.



data_pca <- as.data.frame(pr.log_data$x[,1:3])
data_pca$ZHVI_BottomTier <- state_new_york_lm_date$ZHVI_BottomTier

#car::vif(lm(data=data_pca,ZHVI_BottomTier~.))
summary(lm(data=data_pca,ZHVI_BottomTier~.))




# These variables are very highly significant.
# The R squared values are really high. This model explains 99% of the variation in the data. One problem of this model is that it will tend to overfit and would fail to generalize to future data.

To get a better picture of this model, we will conduct some diagnostic tests.



### Distribution of Error




mod_lm_pca <- lm(data=data_pca,ZHVI_BottomTier~.)


broom::augment(mod_lm_pca) %>%
  ggplot(aes(x=.resid))+geom_histogram()+
  labs(x='Residuals',title=stringr::str_c('Error Mean =',mean(broom::augment(mod_lm_pca)$.resid)))







# The distribution of the errors is nearly normal with a mean of 0.


### Fitted Values vs Residuals

# One way to evaluate the model is to look at the relationship between the model residuals and the fitted values.

# A good model would have a constant error terms throughout.

# Fitting a line through this scatter plot will give us an idea about the trend seen.



broom::augment(mod_lm_pca) %>%
  mutate(Date=ymd(state_new_york$Date)) %>%
  ggplot(aes(x=.fitted,y=.resid))+geom_point()+geom_smooth(method = 'loess')





# The error terms have a trend . They decrease in the middle and then they suddenly decrease.

### Breush Pagan Test





lmtest::bptest(mod_lm_pca)


#The p value of this test is less than 0.05. This means that the test for homoskedascity has failed.

### Collinearity of variables



data_pca %>%
  GGally::ggpairs()




# Although , there is some correlation, it is lesser than it used to be before.

#In the next iteration of model building we need to improve homoskedascity of the linear model.#


price_mod <- caret::BoxCoxTrans(data_pca$ZHVI_BottomTier)
print(price_mod)









data_pca_new <- cbind(data_pca,price_new=predict(price_mod,data_pca$ZHVI_BottomTier))


mod_new <- lm(ZHVI_BottomTier~PC1+PC2+PC3+price_new,data=data_pca_new)

broom::augment(mod_new) %>% ggplot(aes(x=.fitted,y=.resid))+geom_point()+geom_smooth()

lmtest::bptest(mod_new)




data_pca_new %>%
  GGally::ggpairs()



# The correlations amongst the variables remain the same as before.





broom::augment(mod_new) %>% ggplot(aes(x=.fitted,y=.resid))+geom_point()+geom_smooth()






#One observation here is that, the error terms change in a skewed sinusoidal manner. This variation could mean one thing: the model is not explaining a sinusoidal behaviour in the data.

## Sinusoidal Model


#One way to tackle this would be to investigate the data using a sinusoidal model.We will convert the features into sinusoidal components. The first harmonics are used for simplicity.

#Let `f(X)` be a function of `X` where `X` is a feature matrix. The model will be the follows.

#f(X) = A#(sin(2#pi#X)+cos(2#pi#X))+ eta` where `eta` is normally distributed error.

#


data_pca_sine <- data_pca %>% select(-ZHVI_BottomTier) %>% mutate_all(funs(sin(2#pi#.)+cos(2#pi#.)))
                                                                               data_pca_sine$ZHVI_BottomTier <- data_pca$ZHVI_BottomTier
                                                                               
                                                                               model_sine <- lm(ZHVI_BottomTier~.,data=data_pca_sine)
                                                                               
                                                                               
                                                                               
                                                                               broom::augment(model_sine) %>% ggplot(aes(x=.fitted,y=.resid))+geom_point()+geom_smooth()
                                                                               
                                                                               
                                                                               
                                                                               
                                                                               
                                                                               
                                                                               
                                                                               
                                                                               
                                                                               
                                                                               #We do  still see a pattern in the error terms when they are plotted against the fitted values. These error terms are sinusoidal in nature. One way to find out how these terms change with fitted values, is to create a model.We will use another sinusoidal model to look at the error terms. This model will use the fitted values as the dependent variable.
                                                                               
                                                                               
                                                                               ### Sinusoidal error
                                                                               
                                                                               
                                                                               
                                                                               
                                                                               
                                                                               error_sine <- augment(model_sine) %>% select(.fitted,.resid) %>% rename(residuals=.resid,fitted_values=.fitted) %>%
                                                                                 mutate(fitted_values_sine=sin(2#pi#fitted_values)
                                                                                 ) %>% select(-fitted_values)
                                                                                 
                                                                                 error_sine_model <- lm(residuals~.,data=error_sine)
                                                                                 
                                                                                 augment(error_sine_model) %>%
                                                                                   ggplot(aes(x=.fitted,y=.resid))+geom_point()+geom_smooth()
                                                                                 
                                                                                 
                                                                                 
                                                                                 bptest(error_sine_model)
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 ## Overall Model
                                                                                 
                                                                                 
                                                                                 
                                                                                 fitted_1 <- broom::augment(model_sine) %>% select(.fitted) %>% pull(.fitted)
                                                                                 fitted_2 <- broom::augment(error_sine_model) %>% select(.fitted) %>% pull(.fitted)
                                                                                 
                                                                                 actual <- fitted_1+fitted_2
                                                                                 
                                                                                 data_pca$predicted <- actual
                                                                                 
                                                                                 data_pca %>% mutate(Date=ymd(state_new_york_lm$Date)) %>% select(Date,ZHVI_BottomTier,predicted) %>% tidyr::gather(type,value,2:3) %>% ggplot(aes(x=Date,y=value,color=type))+geom_line()
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 ## Comparing the model fits
                                                                                 
                                                                                 
                                                                                 
                                                                                 options(scipen=999)
                                                                                 predicted_naive <- broom::augment(mod_lm) %>% select(.fitted) %>% pull(.fitted)
                                                                                 predicted_pca <- broom::augment(mod_lm_pca) %>% select(.fitted) %>% pull(.fitted)
                                                                                 #predicted_sine <- broom::augment(model_sine) %>% select(.fitted) %>% pull(.fitted)
                                                                                 
                                                                                 data_pca$predicted_sine <- actual
                                                                                 data_pca$predicted_pca <- predicted_pca
                                                                                 data_pca$predicted_naive <- predicted_naive
                                                                                 
                                                                                 data_pca %>% mutate(Date=ymd(state_new_york_lm$Date)) %>%
                                                                                   select(ZHVI_BottomTier,predicted_naive,predicted_pca,predicted_sine,Date) %>%
                                                                                   tidyr::gather(type,value,2:4) %>%
                                                                                   mutate(type=ifelse(grepl('naive',type),'Naive Linear Model',type),
                                                                                          type=ifelse(grepl('pca',type),'Linear Model With PCA Features',type),type=ifelse(grepl('sine',type),'Sinusoidal Model',type)) %>%
                                                                                   ggplot(aes(x=Date,y=value,color=type))+geom_line()+facet_wrap(~type)+geom_line(aes(x=Date,y=ZHVI_BottomTier),color='black')+theme(panel.background = element_blank(),legend.position = 'none',strip.background = element_blank())
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 ## Comparing the R Squared metrics
                                                                                 
                                                                                 
                                                                                 
                                                                                 broom::glance(mod_lm)
                                                                                 broom::glance(mod_lm_pca)
                                                                                 broom::glance(model_sine)
                                                                                 
                                                                                 