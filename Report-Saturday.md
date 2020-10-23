Saturday
================
Shih-Ni Prim
2020-10-16

  - [Introduction](#introduction)
  - [Setting the Value for the
    Parameter](#setting-the-value-for-the-parameter)
  - [Data](#data)
  - [Splitting Data](#splitting-data)
  - [Summaries and Exploratory Data
    Analysis](#summaries-and-exploratory-data-analysis)
  - [Fitting models](#fitting-models)
      - [Regression tree](#regression-tree)
      - [Boosted Tree](#boosted-tree)
      - [Comparison](#comparison)
      - [Final Model](#final-model)

## Introduction

Now we take a look at Saturday’s analysis. This dataset contains
information about [bike
sharing](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset).
We have a variety of predictors, including hours, temperature, humidity,
weekday, holiday/workday or not, etc. In our analysis, We will use two
statistical learning models–regression tree and boosted tree–to predict
the count of total rental bikes `cnt`.

## Setting the Value for the Parameter

Since the current analysis is on Saturday, we first find the
corresponding value for it.

``` r
set.seed(7777)
i <- 0:6
dayz <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
df <- as.data.frame(cbind(i, dayz))
weekdayNum <- df$i[df$dayz == params$weekday]
print(weekdayNum)
```

    ## [1] "6"

## Data

Now we read in the data. Two datasets are listed on [the
link](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset), one
including the `hr` variable, and one treating each day as one
observation and thus not including the `hr` variable. Since hours–the
time in the day–should be a meaningful predictor for the number of bike
rentals, we use the dataset with the `hr` variable

``` r
bikes <- read_csv("../Bike-Sharing-Dataset/hour.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   instant = col_double(),
    ##   dteday = col_date(format = ""),
    ##   season = col_double(),
    ##   yr = col_double(),
    ##   mnth = col_double(),
    ##   hr = col_double(),
    ##   holiday = col_double(),
    ##   weekday = col_double(),
    ##   workingday = col_double(),
    ##   weathersit = col_double(),
    ##   temp = col_double(),
    ##   atemp = col_double(),
    ##   hum = col_double(),
    ##   windspeed = col_double(),
    ##   casual = col_double(),
    ##   registered = col_double(),
    ##   cnt = col_double()
    ## )

``` r
# head(bikes)
analysis <- bikes %>% filter(weekday == weekdayNum) %>% select(-casual, -registered) %>% select(dteday, weekday, everything()) 
# head(analysis)
```

## Splitting Data

We first split up the data into two sets: training and test sets. The
training set has about 70% of the data, and the test set has about 30%.
Splitting up the data is important, because we want to test the model on
a set that is not used in training, otherwise we risk overfitting.

``` r
train <- sample(1:nrow(analysis), size = nrow(analysis)*0.7)
test <- setdiff(1:nrow(analysis), train)

bikeTrain <- analysis[train,]
bikeTest <- analysis[test,]
```

## Summaries and Exploratory Data Analysis

To decide which variables to include in our models, we first take a
quick look at the data. We can look at summaries of numerical variables.

``` r
summary(bikeTrain)
```

    ##      dteday              weekday     instant          season            yr        
    ##  Min.   :2011-01-01   Min.   :6   Min.   :    1   Min.   :1.000   Min.   :0.0000  
    ##  1st Qu.:2011-06-25   1st Qu.:6   1st Qu.: 4128   1st Qu.:1.000   1st Qu.:0.0000  
    ##  Median :2011-12-31   Median :6   Median : 8622   Median :2.000   Median :0.0000  
    ##  Mean   :2011-12-26   Mean   :6   Mean   : 8540   Mean   :2.464   Mean   :0.4898  
    ##  3rd Qu.:2012-06-23   3rd Qu.:6   3rd Qu.:12834   3rd Qu.:3.000   3rd Qu.:1.0000  
    ##  Max.   :2012-12-29   Max.   :6   Max.   :17331   Max.   :4.000   Max.   :1.0000  
    ##       mnth             hr           holiday    workingday   weathersit         temp       
    ##  Min.   : 1.00   Min.   : 0.00   Min.   :0   Min.   :0    Min.   :1.000   Min.   :0.0200  
    ##  1st Qu.: 3.00   1st Qu.: 6.00   1st Qu.:0   1st Qu.:0    1st Qu.:1.000   1st Qu.:0.3200  
    ##  Median : 6.00   Median :12.00   Median :0   Median :0    Median :1.000   Median :0.4800  
    ##  Mean   : 6.48   Mean   :11.67   Mean   :0   Mean   :0    Mean   :1.409   Mean   :0.4848  
    ##  3rd Qu.: 9.00   3rd Qu.:18.00   3rd Qu.:0   3rd Qu.:0    3rd Qu.:2.000   3rd Qu.:0.6400  
    ##  Max.   :12.00   Max.   :23.00   Max.   :0   Max.   :0    Max.   :3.000   Max.   :1.0000  
    ##      atemp             hum           windspeed           cnt       
    ##  Min.   :0.0000   Min.   :0.1200   Min.   :0.0000   Min.   :  1.0  
    ##  1st Qu.:0.3182   1st Qu.:0.4500   1st Qu.:0.1045   1st Qu.: 43.0  
    ##  Median :0.4697   Median :0.6200   Median :0.1940   Median :133.0  
    ##  Mean   :0.4646   Mean   :0.6197   Mean   :0.1951   Mean   :190.8  
    ##  3rd Qu.:0.6212   3rd Qu.:0.7900   3rd Qu.:0.2836   3rd Qu.:300.0  
    ##  Max.   :0.8939   Max.   :1.0000   Max.   :0.8358   Max.   :783.0

Below we look at three plots. The first plot shows the histogram of bike
rentals (`cnt`) on Saturday. The second plot shows that `cnt` does vary
in different hours. The third plot shows that `cnt` varies between the
two years. So we know we should keep `hr` and `yr` as predictors.

``` r
ggplot(bikeTrain, mapping = aes(x = cnt)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Report-Saturday_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(bikeTrain, aes(x = hr, y = cnt)) + geom_point() + geom_jitter()
```

![](Report-Saturday_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
ggplot(bikeTrain, aes(x = yr, y = cnt)) + geom_boxplot(aes(group = yr))
```

![](Report-Saturday_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

Next we look at correlations of different variables. Weather and
windspeed do not seem correlate, so we will keep both `weathersit` and
`windspeed`.

``` r
ggplot(bikeTrain, aes(x = weathersit, y = windspeed)) + geom_jitter()
```

![](Report-Saturday_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Several pairs of variables seem highly correlated–`season` and `mnth`,
`holiday` and `workingday`–so we’ll remove one from each pair.

``` r
cor(bikeTrain$season, bikeTrain$mnth)
```

    ## [1] 0.8063518

``` r
cor(bikeTrain$holiday, bikeTrain$workingday)
```

    ## Warning in cor(bikeTrain$holiday, bikeTrain$workingday): the standard deviation is zero

    ## [1] NA

``` r
cor(bikeTrain$temp, bikeTrain$atemp)
```

    ## [1] 0.9922486

The variance of `workingday` and `holiday` are too small and probably
not good predictors.

``` r
var(bikeTrain$holiday)
```

    ## [1] 0

``` r
var(bikeTrain$workingday)
```

    ## [1] 0

Also, `instant` and `dteday` are for record-keeping. Thus, we decide to
keep the following variables as the predictors: `season`, `yr`, `hr`,
`weathersit`, `atemp`, `hum`, and `windspeed`.

``` r
bikeTrain <- select(bikeTrain, season, yr, hr, weathersit, atemp, hum, windspeed, cnt)
bikeTest <- select(bikeTest, season, yr, hr, weathersit, atemp, hum, windspeed, cnt)
```

## Fitting models

Now we have a final training set and have chosen the predictors, we can
use two models–regression tree and boosted tree–to fit the training
data.

### Regression tree

A regression tree is one of the tree based methods for supervised
learning with the goal of predicting a continuous response. It splits up
predictor space into different regions, and the prediction of each
region is often the mean of observations in that region.

For regression tree, we use the `caret` package and apply the
leave-one-out cross validation method (thus the argument `method =
"LOOCV"`). We set the `tuneLength` as 10 and let the model chooses the
best model automatically.

``` r
modelLookup("rpart")

bikeTree <- train(cnt ~ ., data = bikeTrain, method = "rpart", trControl = trainControl(method = "LOOCV"), tuneGrid = expand.grid(cp = seq(0.01, 0.02, 0.001)))
```

Below we can see the final model; the resulting RMSE, Rsquared, and MAE
of different cp; and a plot that shows the relationship between cp and
RMSE.

``` r
bikeTree$finalModel
```

    ## n= 1758 
    ## 
    ## node), split, n, deviance, yval
    ##       * denotes terminal node
    ## 
    ##   1) root 1758 56585800.0 190.77990  
    ##     2) hr< 8.5 642  1374935.0  46.78972 *
    ##     3) hr>=8.5 1116 34242950.0 273.61290  
    ##       6) atemp< 0.4621 507  6924617.0 163.94280  
    ##        12) hr>=18.5 192   693775.0 101.62500 *
    ##        13) hr< 18.5 315  5030727.0 201.92700  
    ##          26) atemp< 0.29545 129   611626.9 125.02330 *
    ##          27) atemp>=0.29545 186  3127042.0 255.26340  
    ##            54) yr< 0.5 110   722442.9 182.97270 *
    ##            55) yr>=0.5 76   997719.2 359.89470 *
    ##       7) atemp>=0.4621 609 16143750.0 364.91460  
    ##        14) hr>=18.5 184  1306232.0 226.83150 *
    ##        15) hr< 18.5 425  9810302.0 424.69650  
    ##          30) yr< 0.5 203  2108084.0 326.00000 *
    ##          31) yr>=0.5 222  3916613.0 514.94590  
    ##            62) hum>=0.685 32   372335.7 353.40620 *
    ##            63) hum< 0.685 190  2568597.0 542.15260  
    ##             126) hr< 10.5 25   113242.2 391.44000 *
    ##             127) hr>=10.5 165  1801458.0 564.98790 *

``` r
bikeTree
```

    ## CART 
    ## 
    ## 1758 samples
    ##    7 predictor
    ## 
    ## No pre-processing
    ## Resampling: Leave-One-Out Cross-Validation 
    ## Summary of sample sizes: 1757, 1757, 1757, 1757, 1757, 1757, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp     RMSE      Rsquared   MAE     
    ##   0.010  77.43742  0.8137188  58.19536
    ##   0.011  79.03319  0.8061955  59.03842
    ##   0.012  79.83696  0.8019993  59.85002
    ##   0.013  79.83696  0.8019993  59.85002
    ##   0.014  80.01667  0.8011178  59.92878
    ##   0.015  80.01667  0.8011178  59.92878
    ##   0.016  81.23130  0.7951441  60.28603
    ##   0.017  82.92198  0.7870765  61.37413
    ##   0.018  82.88164  0.7865968  61.50423
    ##   0.019  82.72525  0.7873994  61.40857
    ##   0.020  82.72525  0.7873994  61.40857
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.01.

``` r
plot(bikeTree)
```

![](Report-Saturday_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Finally we use the model to predict `cnt` on the test data and calculate
RMSE to check the fit of the model.

``` r
predTree <- predict(bikeTree, newdata = bikeTest)
treeResult <- postResample(predTree, bikeTest$cnt)
```

### Boosted Tree

A boosted tree is one of the ensemble learning methods, in which the
tree grows sequentially. Each subsequent tree is combined into the
previous model to produce a modified model. The predictions are updated
as the tree grows.

We again use `caret` package and set the method as `gbm`. We use
repeated cross validation (`repeatedcv`) and set the `tuneLength` as 10
and let the model chooses the best model automatically.

``` r
modelLookup("gbm")

grid <- expand.grid(n.trees = c(50, 100, 150), interaction.depth = 1:4, shrinkage = c(0.1, 0.01), n.minobsinnode = c(10, 15, 20))

boostedBike <- train(cnt ~  season + yr + hr + weathersit + atemp + hum + windspeed, data = bikeTrain, method = "gbm", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), tuneGrid = grid, verbose = FALSE)
```

Below we can see some information about the final model, the predictors
chosen and their importance, and a plot that shows how RMSE changes with
different numbers of boosting iterations and tree depths.

``` r
boostedBike$finalModel
```

    ## A gradient boosted model with gaussian loss function.
    ## 150 iterations were performed.
    ## There were 7 predictors of which 7 had non-zero influence.

``` r
summary(boostedBike)
```

![](Report-Saturday_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
plot(boostedBike)
```

![](Report-Saturday_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

Finally, we use the model to predict `cnt` on the test data and
calculate RMSE to check the fit of the model.

``` r
predBoostedBike <- predict(boostedBike, newdata = select(bikeTest, -cnt))
boostedResult <- postResample(predBoostedBike, bikeTest$cnt)
```

### Comparison

We can put the testing RMSE from the two models together for comparison.

``` r
comparison <- data.frame(rbind(t(treeResult), t(boostedResult)))
colnames(comparison) <- c("RMSE", "Rsquared", "MAE")
rownames(comparison) <- c("Regression Tree", "Boosted Tree")
knitr::kable(comparison)
```

|                 |     RMSE |  Rsquared |      MAE |
| :-------------- | -------: | --------: | -------: |
| Regression Tree | 80.82609 | 0.8006307 | 60.47904 |
| Boosted Tree    | 45.72799 | 0.9360725 | 31.24816 |

### Final Model

``` r
# a function to generate the name of the best model
model <- function(x, y){
  xscore <- 0
  if (x[[1]] < y[[1]]) {
    xscore = xscore + 1
  }
  if (x[[2]] > y[[2]]){
    xscore = xscore + 1
  }
  if (x[[3]] < y[[3]]){
    xscore = xscore + 1
  }
  if (xscore == 2 || xscore == 3){
    final <- c("regression tree")
  } else {
    final <- c("boosted tree")
  }
  return(final)
}
# model(treeResult, boostedResult)
```

From the output, we can conclude that the boosted tree is the better
model for Saturday data, because it has better performance in terms of
RMSE, Rsquared, and MAE.
