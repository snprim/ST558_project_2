Monday
================
Shih-Ni Prim
2020-10-11

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

Now we take a look at Monday’s analysis. This dataset contains
information about [bike
sharing](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset).
We have a variety of predictors, including hours, temperature, humidity,
weekday, holiday/workday or not, etc. In our analysis, We will use two
statistical learning models–regression tree and boosted tree–to predict
the count of total rental bikes `cnt`.

## Setting the Value for the Parameter

Since the current analysis is on Monday, we first find the corresponding
value for it.

``` r
set.seed(7777)
i <- 0:6
dayz <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
df <- as.data.frame(cbind(i, dayz))
weekdayNum <- df$i[df$dayz == params$weekday]
print(weekdayNum)
```

    ## [1] "1"

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
    ##  Min.   :2011-01-03   Min.   :1   Min.   :   48   Min.   :1.000   Min.   :0.0000  
    ##  1st Qu.:2011-06-27   1st Qu.:1   1st Qu.: 4170   1st Qu.:2.000   1st Qu.:0.0000  
    ##  Median :2011-12-19   Median :1   Median : 8359   Median :2.000   Median :0.0000  
    ##  Mean   :2011-12-26   Mean   :1   Mean   : 8542   Mean   :2.481   Mean   :0.4916  
    ##  3rd Qu.:2012-06-25   3rd Qu.:1   3rd Qu.:12872   3rd Qu.:3.000   3rd Qu.:1.0000  
    ##  Max.   :2012-12-31   Max.   :1   Max.   :17379   Max.   :4.000   Max.   :1.0000  
    ##       mnth             hr           holiday         workingday       weathersit   
    ##  Min.   : 1.00   Min.   : 0.00   Min.   :0.0000   Min.   :0.0000   Min.   :1.000  
    ##  1st Qu.: 4.00   1st Qu.: 6.00   1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:1.000  
    ##  Median : 7.00   Median :12.00   Median :0.0000   Median :1.0000   Median :1.000  
    ##  Mean   : 6.46   Mean   :11.52   Mean   :0.1452   Mean   :0.8548   Mean   :1.435  
    ##  3rd Qu.: 9.00   3rd Qu.:17.00   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:2.000  
    ##  Max.   :12.00   Max.   :23.00   Max.   :1.0000   Max.   :1.0000   Max.   :3.000  
    ##       temp            atemp             hum          windspeed           cnt       
    ##  Min.   :0.0200   Min.   :0.0606   Min.   :0.150   Min.   :0.0000   Min.   :  1.0  
    ##  1st Qu.:0.3400   1st Qu.:0.3333   1st Qu.:0.490   1st Qu.:0.1045   1st Qu.: 36.0  
    ##  Median :0.5200   Median :0.5000   Median :0.640   Median :0.1642   Median :136.0  
    ##  Mean   :0.4948   Mean   :0.4751   Mean   :0.635   Mean   :0.1888   Mean   :182.1  
    ##  3rd Qu.:0.6600   3rd Qu.:0.6212   3rd Qu.:0.780   3rd Qu.:0.2537   3rd Qu.:268.0  
    ##  Max.   :0.9200   Max.   :0.8485   Max.   :1.000   Max.   :0.7164   Max.   :968.0

Below we look at three plots. The first plot shows the histogram of bike
rentals (`cnt`) on Monday. The second plot shows that `cnt` does vary in
different hours. The third plot shows that `cnt` varies between the two
years. So we know we should keep `hr` and `yr` as predictors.

``` r
ggplot(bikeTrain, mapping = aes(x = cnt)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Report-Monday_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(bikeTrain, aes(x = hr, y = cnt)) + geom_point() + geom_jitter()
```

![](Report-Monday_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
ggplot(bikeTrain, aes(x = yr, y = cnt)) + geom_boxplot(aes(group = yr))
```

![](Report-Monday_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

Next we look at correlations of different variables. Weather and
windspeed do not seem correlate, so we will keep both `weathersit` and
`windspeed`.

``` r
ggplot(bikeTrain, aes(x = weathersit, y = windspeed)) + geom_jitter()
```

![](Report-Monday_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Several pairs of variables seem highly correlated–`season` and `mnth`,
`holiday` and `workingday`–so we’ll remove one from each pair.

``` r
cor(bikeTrain$season, bikeTrain$mnth)
```

    ## [1] 0.840952

``` r
cor(bikeTrain$holiday, bikeTrain$workingday)
```

    ## [1] -1

``` r
cor(bikeTrain$temp, bikeTrain$atemp)
```

    ## [1] 0.9931593

The variance of `workingday` and `holiday` are too small and probably
not good predictors.

``` r
var(bikeTrain$holiday)
```

    ## [1] 0.1242205

``` r
var(bikeTrain$workingday)
```

    ## [1] 0.1242205

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
best model automatically. Below we can see the resulting RMSE, Rsquared,
and MAE of different cp as well as a plot that visualizes this
information.

Finally we use the model to predict `cnt` on the test data and calculate
RMSE to check the fit of the model.

``` r
modelLookup("rpart")

bikeTree <- train(cnt ~ ., data = bikeTrain, method = "rpart", trControl = trainControl(method = "LOOCV"), tuneLength = 10)
bikeTree
```

    ## CART 
    ## 
    ## 1735 samples
    ##    7 predictor
    ## 
    ## No pre-processing
    ## Resampling: Leave-One-Out Cross-Validation 
    ## Summary of sample sizes: 1734, 1734, 1734, 1734, 1734, 1734, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp          RMSE       Rsquared    MAE      
    ##   0.01188656   96.13808  0.70554890   66.64913
    ##   0.01298526   96.95736  0.70019674   66.20332
    ##   0.01654438  100.32987  0.67912573   68.30139
    ##   0.01745928  103.95538  0.65585348   73.33833
    ##   0.02160623  106.04141  0.64146920   72.92370
    ##   0.03420747  114.08459  0.58599425   79.48812
    ##   0.04435749  118.75724  0.55160517   81.65378
    ##   0.08782336  145.20871  0.34067665  103.01808
    ##   0.09751479  157.86921  0.22403024  116.46327
    ##   0.30887616  181.77615  0.00113738  156.94825
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.01188656.

``` r
plot(bikeTree)
```

![](Report-Monday_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

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
and let the model chooses the best model automatically. We can then see
the importance of each variable and a plot that shows how RMSE changes
with different numbers of boosting iterations and tree depths.

Finally, we use the model to predict `cnt` on the test data and
calculate RMSE to check the fit of the model.

``` r
modelLookup("gbm")

boostedBike <- train(cnt ~  season + yr + hr + weathersit + atemp + hum + windspeed, data = bikeTrain, method = "gbm", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), tuneLength = 10, verbose = FALSE)
summary(boostedBike)
```

![](Report-Monday_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
plot(boostedBike)
```

![](Report-Monday_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

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
| Regression Tree | 92.99203 | 0.7504531 | 62.64449 |
| Boosted Tree    | 54.38211 | 0.9143932 | 35.56704 |

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
model for Monday data, because it has better performance in terms of
RMSE, Rsquared, and MAE.
