Model Fitting and Report Automation with Bike Sharing Data
================
Shih-Ni Prim
2020-10-16

  - [Description](#description)
  - [Sub-documents](#sub-documents)
  - [Required Packages](#required-packages)
  - [How to automate the reports](#how-to-automate-the-reports)

## Description

This repository uses data from [a bike sharing
dataset](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset)
to predict the number of bike rentals in a certain hour of a certain
day.

## Sub-documents

  - The analysis for [Monday is available here](Report-Monday.md)
  - The analysis for [Tuesday is available here](Report-Tuesday.md)
  - The analysis for [Wednesday is available here](Report-Wednesday.md)
  - The analysis for [Thursday is available here](Report-Thursday.md)
  - The analysis for [Friday is available here](Report-Friday.md)
  - The analysis for [Saturday is available here](Report-Saturday.md)
  - The analysis for [Sunday is available here](Report-Sunday.md)

## Required Packages

  - `tidyverse`
  - `caret`
  - `rmarkdown`

## How to automate the reports

To generate reports for each of the seven days in a week, we can simply
change the parameter and render a different file. In the code chunks, we
would use `params$weekday` instead of the values for the given day. The
code chunk below offers two methods; the first one creates a data frame
for each file name and parameter, and the second one simply lists all
the code for rendering each file.

``` r
# method 1
dayz <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
paramz <- lapply(dayz, FUN = function(x){list(weekday = x)})
output_file <- paste0("Report-", dayz, ".md")
reports <- tibble(output_file, paramz)

library(rmarkdown)
apply(reports, MARGIN = 1,
      FUN = function(x){
          render(input = "analysis.Rmd",
                 output_format = "github_document",
                 output_file = x[[1]],
                 params = x[[2]])
          })

# method 2
rmarkdown::render(input = "analysis.Rmd", output_format = "github_document", output_file = "Report-Sunday.md", params = list(weekday = "Sunday"))
rmarkdown::render(input = "analysis.Rmd", output_format = "github_document", output_file = "Report-Monday.md", params = list(weekday = "Monday"))
rmarkdown::render(input = "analysis.Rmd", output_format = "github_document", output_file = "Report-Tuesday.md", params = list(weekday = "Tuesday"))
rmarkdown::render(input = "analysis.Rmd", output_format = "github_document", output_file = "Report-Wednesday.md", params = list(weekday = "Wednesday"))
rmarkdown::render(input = "analysis.Rmd", output_format = "github_document", output_file = "Report-Thursday.md", params = list(weekday = "Thursday"))
rmarkdown::render(input = "analysis.Rmd", output_format = "github_document", output_file = "Report-Friday.md", params = list(weekday = "Friday"))
rmarkdown::render(input = "analysis.Rmd", output_format = "github_document", output_file = "Report-Saturday.md", params = list(weekday = "Saturday"))
```
