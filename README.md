
# ffData

<!-- badges: start -->
<!-- badges: end -->

ffData is an R package which allows the automatic download of financial returns data from the [Kenneth R. French Data Library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html). For now, the implemented datasets include U.S. Research Returns Data, Bivariate sorts, Three-way sorts and Industry Portfolios. 

## Installation

You can install this version of the package from Github with:

``` r
install.packages("devtools")
library(devtools)
install_github("antshi/ffData")
library(ffData)
```

## Examples

These are basic examples which show you how to use the main function ffdata.download()

### Example 1 

Everything is set to default. 
The function automatically downloads cleaned for NAs, monthly returns of 3-Fama-French factor portfolios with dividends from 1975/01 till today.

``` r
library(ffData)
ffDataDownload()
```

### Example 2

Download monthly returns with dividends for the 48 Industry Portfolios from January 1990 till December 2018.

```r
library(ffData)
ffDataDownload(freq="m", type="Industry", factors.n=48, start="199001", end="201812")
```
