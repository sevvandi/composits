
<!-- README.md is generated from README.Rmd. Please edit that file -->

# composits

<!-- badges: start -->

<!-- badges: end -->

The goal of *composits* is to find outliers in compositional,
multivariate and univariate time series. It is an ensemble method that
uses the packages *forecast*, *tsoutliers*, *anomalize* and *otsad*.

## Installation

<!-- You can install the released version of composits from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("composits") -->

<!-- ``` -->

And the development version from [GitHub](https://github.com/) with:

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
#> $Indices
#> [1] 320
#> 
#> $Total_Score
#> [1] 2
#> 
#> $Num_Coords
#> [1] 3
#> 
#> $Num_Methods
#> [1] 3
#> 
#> $DOBIN
#> [1] 0.6082453
#> 
#> $PCA
#> [1] 0.7074787
#> 
#> $ICA
#> [1] 0.684276
#> 
#> $forecast
#> [1] 0.5
#> 
#> $tsoutliers
#> [1] 0.75
#> 
#> $otsad
#> [1] 0
#> 
#> $anomalize
#> [1] 0.75
#> 
#> $gapscore1
#> [1] 2
```
