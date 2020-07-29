
<!-- README.md is generated from README.Rmd. Please edit that file -->

# composits

<!-- badges: start -->

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

As this is a private repo you need to generate a token (PAT) to install
this using *install\_github*. This is explained at
<http://devtools.r-lib.org/reference/install_github>. And the
development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sevvandi/composits", auth_token ='yourtoken')
```

Or you can do the following on your RStudio Terminal after you pull the
repo.

``` r
cd ..
R CMD build composits
R CMD INSTALL composits_0.0.0.9000.tar.gz
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
