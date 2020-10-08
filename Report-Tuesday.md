
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

## Introduction

Now we take a look at Tuesday’s analysis. This dataset contains
information about bike sharing. We have a variety of predictors,
including hours, temperature, humidity, weekday, holiday/workday or not,
etc. We will use the variable `cnt` as the response variable.

## Setting the Value for the Parameter

Since the current analysis is on Tuesday, we first find the
corresponding value for it.

``` r
set.seed(7777)
i <- 0:6
dayz <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
df <- as.data.frame(cbind(i, dayz))
weekdayNum <- df$i[df$dayz == params$weekday]
print(weekdayNum)
```

    ## [1] "2"

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
head(bikes)
analysis <- bikes %>% filter(weekday == weekdayNum) %>% select(-casual, -registered) %>% select(dteday, weekday, everything()) 
head(analysis)
```

## Splitting Data

We first split up the data into two sets: training and test sets. The
training set has about 70% of the data, and the test set has about 30%.
Splitting up the data is important, because we want to test the model on
a set that is not used in training. Otherwise, we risk overfitting.

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
    ##  Min.   :2011-01-04   Min.   :2   Min.   :   70   Min.   :1.000   Min.   :0.0000  
    ##  1st Qu.:2011-07-05   1st Qu.:2   1st Qu.: 4351   1st Qu.:2.000   1st Qu.:0.0000  
    ##  Median :2011-12-27   Median :2   Median : 8540   Median :3.000   Median :0.0000  
    ##  Mean   :2011-12-29   Mean   :2   Mean   : 8612   Mean   :2.533   Mean   :0.4956  
    ##  3rd Qu.:2012-06-26   3rd Qu.:2   3rd Qu.:12907   3rd Qu.:3.000   3rd Qu.:1.0000  
    ##  Max.   :2012-12-25   Max.   :2   Max.   :17235   Max.   :4.000   Max.   :1.0000  
    ##       mnth              hr          holiday           workingday       weathersit   
    ##  Min.   : 1.000   Min.   : 0.0   Min.   :0.000000   Min.   :0.0000   Min.   :1.000  
    ##  1st Qu.: 4.000   1st Qu.: 6.0   1st Qu.:0.000000   1st Qu.:1.0000   1st Qu.:1.000  
    ##  Median : 7.000   Median :12.0   Median :0.000000   Median :1.0000   Median :1.000  
    ##  Mean   : 6.513   Mean   :11.6   Mean   :0.008154   Mean   :0.9918   Mean   :1.483  
    ##  3rd Qu.: 9.000   3rd Qu.:18.0   3rd Qu.:0.000000   3rd Qu.:1.0000   3rd Qu.:2.000  
    ##  Max.   :12.000   Max.   :23.0   Max.   :1.000000   Max.   :1.0000   Max.   :3.000  
    ##       temp            atemp             hum           windspeed           cnt       
    ##  Min.   :0.1000   Min.   :0.0758   Min.   :0.1600   Min.   :0.0000   Min.   :  1.0  
    ##  1st Qu.:0.3600   1st Qu.:0.3485   1st Qu.:0.4900   1st Qu.:0.1045   1st Qu.: 35.0  
    ##  Median :0.5400   Median :0.5152   Median :0.6600   Median :0.1940   Median :144.0  
    ##  Mean   :0.5115   Mean   :0.4898   Mean   :0.6417   Mean   :0.1933   Mean   :192.7  
    ##  3rd Qu.:0.6600   3rd Qu.:0.6212   3rd Qu.:0.8100   3rd Qu.:0.2836   3rd Qu.:275.0  
    ##  Max.   :0.9400   Max.   :0.8485   Max.   :1.0000   Max.   :0.7761   Max.   :970.0

Below we look at three plots. The first plot shows the histogram of bike
rentals (`cnt`) on Tuesday. The second plot shows that `cnt` does vary
in different hours. The third plot shows that `cnt` varies between the
two years. So we know we should keep `hr` and `yr` as predictors.

``` r
ggplot(bikeTrain, mapping = aes(x = cnt)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Report-Tuesday_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(bikeTrain, aes(x = hr, y = cnt)) + geom_point() + geom_jitter()
```

![](Report-Tuesday_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
ggplot(bikeTrain, aes(x = yr, y = cnt)) + geom_boxplot(aes(group = yr))
```

![](Report-Tuesday_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

Next we look at correlations of different variables. Weather and
windspeed do not seem correlate, so we will keep both `weathersit` and
`windspeed`.

``` r
ggplot(bikeTrain, aes(x = weathersit, y = windspeed)) + geom_jitter()
```

![](Report-Tuesday_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Several pairs of variables seem highly correlated–`season` and `mnth`,
`holiday` and `workingday`–so we’ll remove one from each pair.

``` r
cor(bikeTrain$season, bikeTrain$mnth)
```

    ## [1] 0.8843931

``` r
cor(bikeTrain$holiday, bikeTrain$workingday)
```

    ## [1] -1

``` r
cor(bikeTrain$temp, bikeTrain$atemp)
```

    ## [1] 0.9925321

The variance of `workingday` and `holiday` is 0 or too small.

``` r
var(bikeTrain$holiday)
```

    ## [1] 0.008091986

``` r
var(bikeTrain$workingday)
```

    ## [1] 0.008091986

Also, `instant` and `dteday` are for record-keeping. Thus, we decide to
keep these as the predictors: `season`, `yr`, `hr`, `weathersit`,
`atemp`, `hum`, and `windspeed`.

``` r
bikeTrain <- select(bikeTrain, season, yr, hr, weathersit, atemp, hum, windspeed, cnt)
bikeTest <- select(bikeTest, season, yr, hr, weathersit, atemp, hum, windspeed, cnt)
```

## Fitting models

Now we have a training set and chose the predictors, we can use two
models–regression tree and boosted tree–to fit the training data.

### Regression tree

For regression tree, we use the `caret` package and apply the
leave-one-out cross validation method (thus the argument `method =
"LOOCV"`). We set the `tuneLength` as 10 and let the model chooses the
best model automatically. Then we use the model to predict `cnt` on the
test data. Finally, we calculate RMSE to see the fit of the model and
for comparison.

``` r
modelLookup("rpart")

bikeTree <- train(cnt ~ ., data = bikeTrain, method = "rpart", trControl = trainControl(method = "LOOCV"), tuneLength = 10)

predTree <- predict(bikeTree, newdata = bikeTest)
treeRMSE <- sqrt(mean((predTree - bikeTest$cnt)^2))
postResample(predTree, bikeTest$cnt)
```

    ##       RMSE   Rsquared        MAE 
    ## 95.9662436  0.7121632 67.0679383

### Boosted Tree

Now we use one of the ensemble method, boosted tree. We again use
`caret` package and set the method as `gbm`. We use repeated cross
validation (`repeatedcv`) and again set the `tuneLength` as 10 and let
the model chooses the best model automatically. Then we use the model to
predict `cnt` on the test data. Finally, we calculate RMSE to see the
fit of the model and for comparison.

``` r
modelLookup("gbm")

boostedBike <- train(cnt ~  season + yr + hr + weathersit + atemp + hum + windspeed, data = bikeTrain, method = "gbm", preProcess = c("center", "scale"), trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), tuneLength = 10, verbose = FALSE)
predBoostedBike <- predict(boostedBike, newdata = select(bikeTest, -cnt))
boostedRMSE <- sqrt(mean((predBoostedBike - bikeTest$cnt)^2))
postResample(predBoostedBike, bikeTest$cnt)
```

    ##       RMSE   Rsquared        MAE 
    ## 40.8048393  0.9474448 24.8866363

### Comparison

We can put the RMSE from the two models together for comparison.

``` r
comparison <- data.frame(treeRMSE, boostedRMSE)
colnames(comparison) <- c("Regression Tree", "Boosted Tree")
rownames(comparison) <- c("RMSE")
knitr::kable(comparison, caption = "Comparison between Regression Tree and Boosted Tree")
```

|      | Regression Tree | Boosted Tree |
| :--- | --------------: | -----------: |
| RMSE |        95.96624 |     40.80484 |

Comparison between Regression Tree and Boosted Tree

``` r
# a function to generate the name of the best model
model <- function(x, y){
  if (x > y) {
    final <- c("boosted tree")
  }
  else {
    final <- c("regression tree")
  }
  return(final)
}
```

From the output, we can conclude that boosted tree is the better model.
