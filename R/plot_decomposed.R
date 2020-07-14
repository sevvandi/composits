#' Plot decomposed time series from comp_tsout_ens or mv_tsout_ens output.
#'
#' @param obj The output from \code{comp_tsout_ens} or \code{mv_tsout_ens} functions.
#' @param method The decomposition method, choose between "pca" (default), "dobin", "ics" or "ica".
#' @return A ggplot showing the time series from the selected decomposition method.
#'
#' @examples
#' set.seed(100)
#' n <- 600
#' x <- sample(1:100, n, replace=TRUE)
#' x[25] <- 200
#' x[320] <- 300
#' x2 <- sample(1:100, n, replace=TRUE)
#' x3 <- sample(1:100, n, replace=TRUE)
#' x4 <- sample(1:100, n, replace=TRUE)
#' X <- cbind.data.frame(x, x2, x3, x4)
#' out <- mv_tsout_ens(X, compr=2, fast=FALSE)
#' plot_decomposed(out, X, method = "pca")
#'
#' @export
plot_decomposed <- function(obj, X, method = "pca"){
  loading_mat <- out[[paste0(method, "_loadings")]]
  if(! is.matrix(loading_mat)){
    print(paste0("Error: loadings for method ", method, " not found!"))
    return(NULL)
  }
  ts_proj <- as.matrix(X) %*% loading_mat
  colnames(ts_proj) <- paste0(method, "_", 1:ncol(loading_mat))
  ts_proj <- tibble::as_tibble(ts_proj) %>%
    dplyr::mutate(t = 1:nrow(X)) %>%
    tidyr::pivot_longer(-t)

  ggplot2::ggplot(ts_proj, ggplot2::aes(x=t, y=value)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = obj$outliers[,"Indices"], color="red", alpha=0.8, size=0.1) +
    ggplot2::facet_wrap(~name, ncol = 2) +
    ggplot2::theme_bw()
}
