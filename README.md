# TemporalIndicators

<!-- badges: start -->
<!-- badges: end -->

The goal of TemporalIndicators is to calculate various indicators from dates, such as which day of the week and year it is, is it a national holiday, is there a national medical congress on that day etc. Contains the necessary information for Hungary, from 2004 to 2019.

## Installation

You can install the TemporalIndicators with:

``` r
devtools::install_github("tamas_ferenci/TemporalIndicators")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(TemporalIndicators)
TemporalIndicators( seq( as.Date( "2018-01-01" ), as.Date( "2019-03-31" ), by = "days" ) )
```

