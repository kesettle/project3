ST 558 Project 3
================
Katelyn Settlemyre, Xi Zeng
2022-10-13

# Package List

Below is the package list needed for this project.

``` r
library(tidyverse)
library(caret)
library(MASS)
library(randomForest)
```

# Introduction

The data set used for analysis is the `OnlineNewsPopularity` data set
from UCI Machine Learning Repository. The data set has 61 variables in
total, as well as 39644 observations. First, we will take a subset data
set by filter according to the data channel. There are 6 data
channels,which are `lifestyle`, `entertainment`, `bus`, `socmed`,`tech`,
`world`, .The lifestyle channel is first chosen for analysis.  
For our project, we will take variables(With description of each
variable):  
+ `n_tokens_title`: Number of words in the title  
+ `n_tokens_content`: Number of words in the content  
+ `num_imgs`: Number of images  
+ `average_token_length`: Average length of the words in the content  
+ `is_weekend`: Was the article published on the weekend?  
+ `global_rate_positive_words`: Rate of positive words in the content  
+ `num_keywords`: Number of keywords in the metadata

The response variable that we are interested in is the `shares`
variable, which stands for the number of sales in social network. A
basic EDA will be performed at first to examine the relationship between
covariates and `shares`. Then predicted modeling is applied to predict
the number of sales, which will be illustrated in detail below.

# Data read in

``` r
#Read in data
news <- readr::read_csv("OnlineNewsPopularity.csv", show_col_types = FALSE)

#Subset data with a single data channel
currentChannel <- params$Channels
subnews <- news[news[, currentChannel] == 1, ]

#Select variables for modeling and EDA
subnews <- subnews %>% dplyr::select(n_tokens_title,n_tokens_content,num_imgs,average_token_length,is_weekend,global_rate_positive_words,shares,num_keywords)
subnews$is_weekend <- as.factor(subnews$is_weekend)
```

# Data Split

In this part, our data set is split into a training set and a test set,
with training set contains 70% of the data and the test set contains the
other 30%.

``` r
#Set seed for reproduction
set.seed(123)

#Split data
index <- createDataPartition(subnews$shares,p = 0.7,list = FALSE)
train <- subnews[index,]
test <- subnews[-index,]
```

# Summarizations and EDA

In this part, basic summary statistics for the predictors are
calculated. Also, some plots,including scatter plots, barplots, and
boxplots, as well as frequency tables are generated to examine the
relationship between the variables. Since the EDA will be produced for
different channels, thus the interpretation of the plot will be less
detailed, but focus more on the trend.

``` r
#Define function for producing summary statistics
sum <- function(x){
  min <- min(x)
  mean <- mean(x)
  median <- median(x)
  max <- max(x)
  sd <- sd(x)
  return(list(Minimum = min, Mean = mean, Median = median, Maximum = max, `Standard Deviation` = sd(x)))
}

#Calculate summary statistic for variables
sum(subnews$n_tokens_title)
```

    ## $Minimum
    ## [1] 3
    ## 
    ## $Mean
    ## [1] 10.28204
    ## 
    ## $Median
    ## [1] 10
    ## 
    ## $Maximum
    ## [1] 19
    ## 
    ## $`Standard Deviation`
    ## [1] 2.160168

``` r
sum(subnews$n_tokens_content)
```

    ## $Minimum
    ## [1] 0
    ## 
    ## $Mean
    ## [1] 539.8714
    ## 
    ## $Median
    ## [1] 400
    ## 
    ## $Maximum
    ## [1] 6336
    ## 
    ## $`Standard Deviation`
    ## [1] 439.8421

We also show correlations between our selected variables. We show both a
correlation matrix and correlation plot.

``` r
#correlation matrix
(corrs <- cor(subnews[,-5]))
```

    ##                            n_tokens_title n_tokens_content    num_imgs average_token_length
    ## n_tokens_title                1.000000000      -0.00451809 -0.01831030          -0.09314079
    ## n_tokens_content             -0.004518090       1.00000000  0.24186002           0.02488826
    ## num_imgs                     -0.018310298       0.24186002  1.00000000           0.02750335
    ## average_token_length         -0.093140791       0.02488826  0.02750335           1.00000000
    ## global_rate_positive_words   -0.009078933       0.17965058 -0.01946145           0.12881003
    ## shares                        0.010673472       0.02794650  0.02754498          -0.02672607
    ## num_keywords                  0.003439601       0.16821716  0.07562007          -0.02476354
    ##                            global_rate_positive_words      shares num_keywords
    ## n_tokens_title                           -0.009078933  0.01067347  0.003439601
    ## n_tokens_content                          0.179650578  0.02794650  0.168217160
    ## num_imgs                                 -0.019461452  0.02754498  0.075620068
    ## average_token_length                      0.128810029 -0.02672607 -0.024763538
    ## global_rate_positive_words                1.000000000  0.01556270  0.128170511
    ## shares                                    0.015562700  1.00000000  0.025673955
    ## num_keywords                              0.128170511  0.02567395  1.000000000

``` r
#correlation plot
corrplot::corrplot(corrs)
```

![](BusinessChannelAnalysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Our correlation matrix and plot shows that we don’t have strong
relationships between variables. The strongest relationship is a
correlation of 0.46 between `n_tokens_content` and `num_imgs`. Thus, we
consider removing `num_imgs` in our model fitting to avoid collinearity.

After producing summary statistics, some plots and tables are shown
below:

## Frequency tables

Below shows the frequency table of number of keywords.

``` r
 table(train$num_keywords)
```

    ## 
    ##   2   3   4   5   6   7   8   9  10 
    ##  12 183 498 889 841 650 466 328 515

According to the table, we will see the frequency count of the number of
key words in each channel.

Here we have a frequency table of number of articles published on a
weekend or not.

``` r
table(subnews$is_weekend)
```

    ## 
    ##    0    1 
    ## 5672  586

According to the table, we will see the frequency count of the articles
published during weekdays or on weekend for each channel.

## Scatter plots

Below are the scatter plots for the chosen variables.

``` r
ggplot(data = train, aes(x = global_rate_positive_words, y = shares))+
geom_point(aes(color = is_weekend)) +
  labs(x = "Rate of positive words in the content",
       y = "Number of Shares",
       title = "Scatter Plot of Shares vs Rate of positive words in the content") +
  scale_color_discrete(name = "Weekend Published", labels = c("No", "Yes"))
```

![](BusinessChannelAnalysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

According to the scatter plot above, it seems that when the rate of
positive words exceeds 0.075, the number of shares of the article is
relatively small. Also, those articles with high shares are mostly
published during weekdays rather than weekend.

``` r
ggplot(data = train, aes(x= n_tokens_content,y = shares)) + 
  geom_point() + 
  labs(x = "Number of Words",
       y = "Number of Shares",
       title = "Scatter Plot of Shares vs Number of Words in the Content")
```

![](BusinessChannelAnalysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The above plot will show the scatter plot between the number of shares
and rate of positive words in the content. Also, the point is colored by
if the article is published on weekend. There may be difference between
the publish date or there may be curvature or linear relationship
between number of shares and rate of positive words in the content，
depending on how the scatter plot looks like.

## Barplots

Below is the bar plot for the number of keywords：

``` r
# Create bar plot of predictor "is_weekend"
g <- ggplot(data = train, aes(x = num_keywords, fill= is_weekend))
g + geom_bar(aes(fill = is_weekend),position = "dodge") +
  xlab("Number of keywords")+
  ggtitle("Bar Plot of Nmber of Keywords")+
  scale_fill_discrete(name = "Weekend Published", labels = c("No", "Yes"))
```

![](BusinessChannelAnalysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

According to the bar plot, the frequency count of the each number of
keywords is shown, also ,it will be group into weekday and weekend to
see if there is any difference between them.

``` r
ggplot(data = train, aes(x= n_tokens_title)) + 
  geom_bar() + labs(x="Words in Title", title = "Bar Plot of Number of Words in Title")
```

![](BusinessChannelAnalysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

This bar plot shows counts of articles by number of words in the title
for each channel.

## Boxplots and Jitter Plots

Below are the box plot for sales:

``` r
g <- ggplot(train, aes(x = is_weekend, y = shares))
g + geom_point(aes(color = is_weekend), position = "jitter")+
  ggtitle("Jitter Plot for shares on weekday/weekend") + xlab("is_weekend")+
  scale_color_discrete(name = "Weekend Published", labels = c("No", "Yes"))
```

![](BusinessChannelAnalysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

A jitter plot is generated to see the spread of shares data in weekdays
and weekends for each channel to see if publish data has an effect on
the shares.

Here, we show boxplots for number of words in the article by weekend and
number of keywords by weekend.

``` r
ggplot(data=train, aes(x=n_tokens_content)) +
  geom_boxplot(aes(fill=is_weekend)) +
  labs(x = "Number of Words in Content")+
  ggtitle("Boxplot for number of words in content on weekday/weekend")+
  scale_fill_discrete(name = "Weekend Published", labels = c("No", "Yes"))
```

![](BusinessChannelAnalysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

This pair of plots would suggest if there’s a difference between
weekends and weekdays for number of words in content.

``` r
ggplot(data=train, aes(x=num_keywords)) +
  geom_boxplot(aes(fill=is_weekend)) +
  labs(x = "Number of Keywords") +
  ggtitle("Boxplot for number of keywords on weekday/weekend")+
  scale_fill_discrete(name = "Weekend Published", labels = c("No", "Yes"))
```

![](BusinessChannelAnalysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

This pair of plots will suggests if articles published on the weekend
use the same number of keywords than those published on a weekday.

# Model fitting

Below is the model fitting part. In this part, four models are fitted,
those being two linear regression models, a random forest model, and a
boosted tree model. For testing goodness of fit, we will be using the
root mean squared error (RMSE).

## Linear Regression

We fit two different linear regression models here. Linear regression is
a basic method to find a linear relationship between a response variable
and one or more predictor variables. Here we will fit models using only
the main effect of predictor variables as well as adding interaction
terms of the linear models.  
First we fit the model with only first-order terms.

``` r
#Use predictors used for the model
lmod_1 <- train(shares~  n_tokens_title + n_tokens_content + average_token_length + is_weekend +global_rate_positive_words + num_keywords,
                    data = train,
                    method = "lm",
                    trControl = trainControl("cv",number=10))

summary(lmod_1)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7151  -2125  -1450   -551 686893 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                 6632.101   3475.084   1.908   0.0564 .
    ## n_tokens_title               -27.646    118.842  -0.233   0.8161  
    ## n_tokens_content               1.010      0.596   1.694   0.0904 .
    ## average_token_length       -1184.265    647.853  -1.828   0.0676 .
    ## is_weekend1                  781.477    891.656   0.876   0.3808  
    ## global_rate_positive_words 15730.634  16282.380   0.966   0.3340  
    ## num_keywords                 161.530    130.922   1.234   0.2173  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16900 on 4375 degrees of freedom
    ## Multiple R-squared:  0.002692,   Adjusted R-squared:  0.001325 
    ## F-statistic: 1.968 on 6 and 4375 DF,  p-value: 0.06658

``` r
#Fit model with selected variables on test data
lmod_pred <- predict(lmod_1, newdata = test)

#Calculate test error
lmod1_RMSE <- RMSE(lmod_pred,test$shares)
lmod1_RMSE
```

    ## [1] 9367.878

Now we fit the linear model with a polynomial term. Since in our EDA
there seems to be a curvature relationship between
`global_rate_positive_words` and `shares`, thus, a polynomial term is
added here.

``` r
#Fit linear model using different predictors with polynomial term
lmod2 <- train(shares~ n_tokens_title + n_tokens_content + average_token_length + is_weekend +global_rate_positive_words + num_keywords + I(global_rate_positive_words^2),
               data = train,
               method = "lm",
               trControl = trainControl("cv",number= 10))

summary(lmod2)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -6613  -2161  -1460   -482 686774 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                        6.114e+03  3.530e+03   1.732   0.0834 .
    ## n_tokens_title                    -2.986e+01  1.189e+02  -0.251   0.8017  
    ## n_tokens_content                   9.061e-01  6.088e-01   1.488   0.1367  
    ## average_token_length              -1.276e+03  6.571e+02  -1.942   0.0522 .
    ## is_weekend1                        7.780e+02  8.917e+02   0.872   0.3830  
    ## global_rate_positive_words         6.615e+04  6.245e+04   1.059   0.2895  
    ## num_keywords                       1.618e+02  1.309e+02   1.236   0.2167  
    ## `I(global_rate_positive_words^2)` -5.408e+05  6.466e+05  -0.836   0.4030  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16900 on 4374 degrees of freedom
    ## Multiple R-squared:  0.002852,   Adjusted R-squared:  0.001256 
    ## F-statistic: 1.787 on 7 and 4374 DF,  p-value: 0.08531

``` r
#fit with test data
lmod2_pred <- predict(lmod2, newdata = test)

#test error:
(lmod2_RMSE <- RMSE(lmod2_pred,test$shares))
```

    ## [1] 9370.894

## Random Forest Model

Here we fit a random forest model. A random forest is an method where
multiple tree models are fit from bootstrap samples using a subset of
predictor variables for each bootstrap sample. The final prediction is
an average of the bootstrap predictions. We use the tuning parameter
`mtry`, the number of randomly selected predictors, using values 1
through 3 to fit the best tune.

``` r
#set tuning parameters
rand_grid <- data.frame(mtry=1:3)

#train model
rand_fit <- train(shares~n_tokens_title + n_tokens_content + average_token_length + is_weekend +global_rate_positive_words + num_keywords,
                  data = train,
                  method = "rf",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                  tuneGrid = rand_grid)
rand_fit$bestTune
rand_fit$finalModel
```

    ## 
    ## Call:
    ##  randomForest(x = x, y = y, mtry = param$mtry) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 1
    ## 
    ##           Mean of squared residuals: 289493935
    ##                     % Var explained: -1.29

``` r
#fit with test data
rand_pred <- predict(rand_fit, newdata = test)

#test error:
(rand_RMSE <- RMSE(rand_pred, test$shares))
```

    ## [1] 9463.229

## Boosted Tree Model

Below is the process of fitting a boosted tree model. A boosted tree is
a method that builds sequentially. It’s a slow-building method, that
builds a new tree considering the error of the previous fit, updating
predictions each new fit. Model performance is tested by fitting the
final tuned model on test set and calculate the test RMSE. Here, we use
combinations of the following tuning parameters:  
+ `n.trees`, the number of boosting iterations, with values 25, 50, 100,
150, and 200  
+ `interaction.depth`, the maximum tree depth, with values 1 through 5  
+ `shrinkage`, the learning rate of the model, with values 0.1, 0.2,
0.3, 0.4, and 0.5, and  
+ `n.minobsinnode`, the minimum node size, here using 10.

``` r
#Set tuning grid for boosted tree model
boost_grid <- expand.grid(n.trees = c(25,50,100,150,200),
                          interaction.depth = c(1:5),
                          shrinkage = c(0.1,0.2,0.3,0.4,0.5),
                          n.minobsinnode = 10)
#boost_grid

#Train the model
boost_fit <- train(shares ~n_tokens_title + n_tokens_content + average_token_length + is_weekend +global_rate_positive_words + num_keywords,
                   data = train,
                   method = "gbm",
                   trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                   preProcess = c("center", "scale"),
                   tuneGrid = boost_grid,
                   verbose = FALSE)

boost_fit$bestTune

#Fit the final boosted tree model with test data
boost_pred <- predict(boost_fit, newdata = test)

#Calculate test RMSE for model performance
boost_RMSE <- RMSE(boost_pred,test$shares)
boost_RMSE
```

    ## [1] 9457.328

# Comparison

Though the RMSE for the testing data has been given for each model in
the previous section, we shall display them here and compare the models’
performances.

``` r
#create and print data frame of models and RMSE's
(fit_RMSE <- data.frame(Model = c("Linear Regression, first-order", "Linear Regression, polynomial", "Random Forest", "Boosted Tree"), RMSE = c(lmod1_RMSE, lmod2_RMSE, rand_RMSE, boost_RMSE)))
```

The lower the RMSE, the better the fit. Therefore, we choose the model
with the lowest RMSE for each channel, printed below:

``` r
#Select row with lowest RMSE value and print
min_val <- min(fit_RMSE$RMSE)
fit_RMSE[fit_RMSE$RMSE == min_val,]
```

# Automation

Below is the part for automating the output for each channel:

``` r
channels <- c("data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world")
# Create file names
name <- c("Lifestyle", "Entertainment", "Business", "SocialMedia",
          "Technology", "World")
output_file <- paste0(name, "ChannelAnalysis.md")
# Create a list for each channel with just channel name parameter
parameters = lapply(channels, FUN = function(x){
  list(Channels = x)
})
# Put into a data frame
reports <- tibble::tibble(output_file, parameters)
#options(knitr.duplicate.label = "allow")
# Automation
apply(reports, MARGIN = 1, FUN = function(x) {
  rmarkdown::render(input = "project3.Rmd", 
                    output_format = "github_document", 
                    output_file = x[[1]], 
                    params = x[[2]], 
                    output_options = list(html_preview = FALSE)) 
})
```
