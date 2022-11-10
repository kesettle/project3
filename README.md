ST 558 Project 3
================
Katelyn Settlemyre, Xi Zeng
2022-10-13

# Package List

Below is the package list needed for this project.

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.2.1

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.1

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ dplyr   1.0.10
    ## ✔ tibble  3.1.8      ✔ stringr 1.4.1 
    ## ✔ tidyr   1.2.1      ✔ forcats 0.5.2 
    ## ✔ purrr   0.3.5

    ## Warning: package 'ggplot2' was built under R version 4.2.1

    ## Warning: package 'tibble' was built under R version 4.2.1

    ## Warning: package 'tidyr' was built under R version 4.2.1

    ## Warning: package 'purrr' was built under R version 4.2.1

    ## Warning: package 'dplyr' was built under R version 4.2.1

    ## Warning: package 'stringr' was built under R version 4.2.1

    ## Warning: package 'forcats' was built under R version 4.2.1

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.2.1

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(ggplot2)  
```

# Introduction

The data set used for analysis is the `OnlineNewsPopularity` data set
from UCI Machine Learning Repository. The data set has 61 variables in
total, as well as 39644 observations. First, we will take a subset data
set by filter according to the data channel. The lifestyle channel is
first chosen for analysis.  
For our project, we will take variables(With description of each
variable): + n_tokens_title: Number of words in the title  
+ n_tokens_content: Number of words in the content  
+ num_imgs: Number of images  
+ average_token_length: Average length of the words in the content  
+ is_weekend: Was the article published on the weekend?  
+ global_rate_positive_words: Rate of positive words in the content  
+ num_keywords: Number of keywords in the metadata

The response variable that we are interested in is the `shares`
variable, which stands for the number of sales in social network. A
basic EDA will be performed at first to examine the relationship between
covariates and `shares`. Then predicted modeling is applied to predict
the number of sales, which will be illustrated in detail below.

# Data read in

``` r
#Read in data
news <- read_csv("OnlineNewsPopularity.csv",show_col_types = FALSE)

#Subset data with a single data channel
subnews <- subset(news, data_channel_is_lifestyle == 1)

#Select variables for modeling and EDA
subnews <- subnews%>% select(n_tokens_title,n_tokens_content,num_imgs,average_token_length,is_weekend,global_rate_positive_words,shares,num_keywords)
subnews$is_weekend <- as.factor(subnews$is_weekend)
```

# Data Split

In this part, our data set is split into training set and test set, with
training set contains 70% of the data and test set contains the other
30%.

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
calculated. Also, some plots,including scatter plots, barplots,
boxplots, frequency tables are generated to examine the relationship
between the variables.

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
    ## [1] 9.765603
    ## 
    ## $Median
    ## [1] 10
    ## 
    ## $Maximum
    ## [1] 18
    ## 
    ## $`Standard Deviation`
    ## [1] 1.909371

``` r
sum(subnews$n_tokens_content)
```

    ## $Minimum
    ## [1] 0
    ## 
    ## $Mean
    ## [1] 621.3273
    ## 
    ## $Median
    ## [1] 502
    ## 
    ## $Maximum
    ## [1] 8474
    ## 
    ## $`Standard Deviation`
    ## [1] 566.0532

After producing summary statistics, some plots and tables are shown
below:

## Frequency table

Below shows the frequency table of number of keywords.

``` r
 table(train$num_keywords)
```

    ## 
    ##   3   4   5   6   7   8   9  10 
    ##   5  26  69 146 228 272 250 476

According to the table, most of the articles have more than 5 keywords.
We assume that more keywords in the metadata will increase shares.

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

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

According to the scatter plot above, it seems that when the rate of
positive words exceeds 0.075, the number of shares of the article is
relatively small. Also, those articles with high shares are mostly
published during weekdays rather than weekend.

## Barplots

Below are the bar plot for the number of keywords：

``` r
# Create bar plot of predictor "is_weekend"
g <- ggplot(data = train, aes(x = num_keywords, fill= is_weekend))
g + geom_bar(aes(fill= is_weekend),position = "dodge") +
  xlab("Number of keywords")+
  ggtitle("Bar Plot of number of keyword")+
  scale_fill_discrete(name = "Weekend Published", labels = c("No", "Yes"))
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

According to the bar plot, the frequency count of the each number of
keywords is shown, also ,it will be group into weekday and weekend to
see if there is any difference between them.

## Boxplots

Below are the box plot for sales:

``` r
g <- ggplot(train, aes(x = is_weekend, y = shares))
g + geom_point(aes(color = is_weekend), position = "jitter")+
  ggtitle("Jitter Plot for shares on weekday/weekend") + xlab("is_weekend")+
  scale_color_discrete(name = "Weekend Published", labels = c("No", "Yes"))
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

A jitter plot is generated to see the spread of shares data in weekdays
and weekends.

# Model fitting

Below is the model fitting part. In this part, 4 models are fitted. They
are: linear regression model, random forest model and boosted tree
model.

## Linear Regression

``` r
#Use forward selection to determine the predictors used for the model
lm_forward <- train(shares~ .,
                    data = train,
                    method = "glmStepAIC",
                    direction = "forward",
                    trControl = trainControl("cv",number=10),
                    trace = FALSE)
lm_forward$finalModel
```

    ## 
    ## Call:  NULL
    ## 
    ## Coefficients:
    ##          (Intercept)      n_tokens_content  average_token_length  
    ##             5908.456                 1.196              -638.876  
    ## 
    ## Degrees of Freedom: 1471 Total (i.e. Null);  1469 Residual
    ## Null Deviance:       1.138e+11 
    ## Residual Deviance: 1.129e+11     AIC: 30910

``` r
#Fit model with selected variables on test data
lmod_pred <- predict(lm_forward, newdata = test)

#Calculate test error
lmod1_RMSE <- RMSE(lmod_pred,test$shares)
lmod1_RMSE
```

    ## [1] 9078.315

## Random Forest Model

## Boosted Tree Model

Below is the process of fitting a boosted tree model, also model
performance is tested by fitting the final tuned model on test set and
calculate the test RMSE. Here, I use the

``` r
#Set tuning grid for boosted tree model
boost_grid <- expand.grid(n.trees = c(25,50,100,150,200),
                          interaction.depth = c(1:5),
                          shrinkage = c(0.1,0.2,0.3,0.4,0.5),
                          n.minobsinnode = 10)
boost_grid
```

    ##     n.trees interaction.depth shrinkage n.minobsinnode
    ## 1        25                 1       0.1             10
    ## 2        50                 1       0.1             10
    ## 3       100                 1       0.1             10
    ## 4       150                 1       0.1             10
    ## 5       200                 1       0.1             10
    ## 6        25                 2       0.1             10
    ## 7        50                 2       0.1             10
    ## 8       100                 2       0.1             10
    ## 9       150                 2       0.1             10
    ## 10      200                 2       0.1             10
    ## 11       25                 3       0.1             10
    ## 12       50                 3       0.1             10
    ## 13      100                 3       0.1             10
    ## 14      150                 3       0.1             10
    ## 15      200                 3       0.1             10
    ## 16       25                 4       0.1             10
    ## 17       50                 4       0.1             10
    ## 18      100                 4       0.1             10
    ## 19      150                 4       0.1             10
    ## 20      200                 4       0.1             10
    ## 21       25                 5       0.1             10
    ## 22       50                 5       0.1             10
    ## 23      100                 5       0.1             10
    ## 24      150                 5       0.1             10
    ## 25      200                 5       0.1             10
    ## 26       25                 1       0.2             10
    ## 27       50                 1       0.2             10
    ## 28      100                 1       0.2             10
    ## 29      150                 1       0.2             10
    ## 30      200                 1       0.2             10
    ## 31       25                 2       0.2             10
    ## 32       50                 2       0.2             10
    ## 33      100                 2       0.2             10
    ## 34      150                 2       0.2             10
    ## 35      200                 2       0.2             10
    ## 36       25                 3       0.2             10
    ## 37       50                 3       0.2             10
    ## 38      100                 3       0.2             10
    ## 39      150                 3       0.2             10
    ## 40      200                 3       0.2             10
    ## 41       25                 4       0.2             10
    ## 42       50                 4       0.2             10
    ## 43      100                 4       0.2             10
    ## 44      150                 4       0.2             10
    ## 45      200                 4       0.2             10
    ## 46       25                 5       0.2             10
    ## 47       50                 5       0.2             10
    ## 48      100                 5       0.2             10
    ## 49      150                 5       0.2             10
    ## 50      200                 5       0.2             10
    ## 51       25                 1       0.3             10
    ## 52       50                 1       0.3             10
    ## 53      100                 1       0.3             10
    ## 54      150                 1       0.3             10
    ## 55      200                 1       0.3             10
    ## 56       25                 2       0.3             10
    ## 57       50                 2       0.3             10
    ## 58      100                 2       0.3             10
    ## 59      150                 2       0.3             10
    ## 60      200                 2       0.3             10
    ## 61       25                 3       0.3             10
    ## 62       50                 3       0.3             10
    ## 63      100                 3       0.3             10
    ## 64      150                 3       0.3             10
    ## 65      200                 3       0.3             10
    ## 66       25                 4       0.3             10
    ## 67       50                 4       0.3             10
    ## 68      100                 4       0.3             10
    ## 69      150                 4       0.3             10
    ## 70      200                 4       0.3             10
    ## 71       25                 5       0.3             10
    ## 72       50                 5       0.3             10
    ## 73      100                 5       0.3             10
    ## 74      150                 5       0.3             10
    ## 75      200                 5       0.3             10
    ## 76       25                 1       0.4             10
    ## 77       50                 1       0.4             10
    ## 78      100                 1       0.4             10
    ## 79      150                 1       0.4             10
    ## 80      200                 1       0.4             10
    ## 81       25                 2       0.4             10
    ## 82       50                 2       0.4             10
    ## 83      100                 2       0.4             10
    ## 84      150                 2       0.4             10
    ## 85      200                 2       0.4             10
    ## 86       25                 3       0.4             10
    ## 87       50                 3       0.4             10
    ## 88      100                 3       0.4             10
    ## 89      150                 3       0.4             10
    ## 90      200                 3       0.4             10
    ## 91       25                 4       0.4             10
    ## 92       50                 4       0.4             10
    ## 93      100                 4       0.4             10
    ## 94      150                 4       0.4             10
    ## 95      200                 4       0.4             10
    ## 96       25                 5       0.4             10
    ## 97       50                 5       0.4             10
    ## 98      100                 5       0.4             10
    ## 99      150                 5       0.4             10
    ## 100     200                 5       0.4             10
    ## 101      25                 1       0.5             10
    ## 102      50                 1       0.5             10
    ## 103     100                 1       0.5             10
    ## 104     150                 1       0.5             10
    ## 105     200                 1       0.5             10
    ## 106      25                 2       0.5             10
    ## 107      50                 2       0.5             10
    ## 108     100                 2       0.5             10
    ## 109     150                 2       0.5             10
    ## 110     200                 2       0.5             10
    ## 111      25                 3       0.5             10
    ## 112      50                 3       0.5             10
    ## 113     100                 3       0.5             10
    ## 114     150                 3       0.5             10
    ## 115     200                 3       0.5             10
    ## 116      25                 4       0.5             10
    ## 117      50                 4       0.5             10
    ## 118     100                 4       0.5             10
    ## 119     150                 4       0.5             10
    ## 120     200                 4       0.5             10
    ## 121      25                 5       0.5             10
    ## 122      50                 5       0.5             10
    ## 123     100                 5       0.5             10
    ## 124     150                 5       0.5             10
    ## 125     200                 5       0.5             10

``` r
#Train the model
boost_fit <- train(shares ~., data = train,
                   method = "gbm",
                   trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                   preProcess = c("center", "scale"),
                   tuneGrid = boost_grid,
                   verbose = FALSE)

boost_fit$bestTune
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode
    ## 1      25                 1       0.1             10

``` r
#Fit the final boosted tree model with test data
boost_pred <- predict(boost_fit, newdata = test)

#Calculate test RMSE for model performance
boost_RMSE <- RMSE(boost_pred,test$shares)
boost_RMSE
```

    ## [1] 9060.891
