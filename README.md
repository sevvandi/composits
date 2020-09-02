
<!-- README.md is generated from README.Rmd. Please edit that file -->

# composits

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/sevvandi/composits.svg?branch=master)](https://travis-ci.org/sevvandi/composits)
<!-- badges: end -->

The goal of *composits* is to find outliers in compositional,
multivariate and univariate time series. It is an outlier ensemble
method that uses the packages `forecast`, `tsoutliers`, `anomalize` and
`otsad`.

## Installation

<!-- You can install the released version of composits from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("composits") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sevvandi/composits")
```

## Example

``` r
library(composits)
set.seed(100)
n <- 600
x <- sample(1:100, n, replace=TRUE)
x[25] <- 200
x[320] <- 300
x2 <- sample(1:100, n, replace=TRUE)
x3 <- sample(1:100, n, replace=TRUE)
X <- cbind.data.frame(x, x2, x3)
x4 <- sample(1:100, n, replace=TRUE)
X <- cbind.data.frame(x, x2, x3, x4)
out <- mv_tsout_ens(X)
#> Registered S3 method overwritten by 'xts':
#>   method     from
#>   as.zoo.xts zoo
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#> Registered S3 methods overwritten by 'forecast':
#>   method             from    
#>   fitted.fracdiff    fracdiff
#>   residuals.fracdiff fracdiff
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Converting from tbl_df to tbl_time.
#> Auto-index message: index = date
#> frequency = 7 days
#> trend = 91 days
#> Warning in outthres1$gapscore1 <- rep(0, length(indthres1)): Coercing LHS
#> to a list
out$all
#>     Indices Total_Score Num_Coords Num_Methods     DOBIN       PCA
#> res     320           2          3           3 0.6082453 0.7074787
#>          ICA forecast tsoutliers otsad anomalize
#> res 0.684276      0.5       0.75     0      0.75
out$outliers
#>   Indices Total_Score Num_Coords Num_Methods     DOBIN       PCA      ICA
#> 1     320           2          3           3 0.6082453 0.7074787 0.684276
#>   forecast tsoutliers otsad anomalize Gap_Score_1
#> 1      0.5       0.75     0      0.75           3
```

See our [website](https://sevvandi.github.io/composits/index.html) or
our paper (Kandanaarachchi et al. 2020) for more examples.

# References

<div id="refs" class="references hanging-indent">

<div id="ref-composits">

Kandanaarachchi, Sevvandi, Patricia Menendez, Ruben Loaiza-Maya, and
Ursula Laa. 2020. “Outliers in Compositional Time Series Data.” Working
Paper.
<https://www.researchgate.net/publication/343712288_Outliers_in_compositional_time_series_data>.

</div>

</div>
