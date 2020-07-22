#' Plot decomposed time series from comp_tsout_ens or mv_tsout_ens output.
#'
#' @param obj The output from \code{comp_tsout_ens} or \code{mv_tsout_ens} functions.
#' @param X The data matrix used as input to \code{mv_tsout_ens} (not needed if \code{obj} is output from \code{comp_tsout_ens}).
#' @param method The decomposition method, choose between "pca" (default), "dobin", "ics", "ica" or "all" for complete set of methods.
#' @return A ggplot showing the time series from the selected decomposition method.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
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
#'
#' out1 <- mv_tsout_ens(X, compr=2, fast=FALSE)
#' plot_decomposed(out1, X = X, method = "pca")
#' plot_decomposed(out1, X = X, method = "all")
#'
#' X <- X/rowSums(X)
#' out2 <- comp_tsout_ens(X, compr=2, fast=FALSE)
#' plot_decomposed(out2)
#'
#' @export
plot_decomposed <- function(obj, X = NULL, method = "pca"){
  if(method == "all"){
    return(plot_decomposed_all(obj, X))
  }
  # initial check that loadings for requested method are in obj
  loading_mat <- obj[[paste0(method, "_loadings")]]
  if(! is.matrix(loading_mat)){
    print(paste0("Error: loadings for method ", method, " not found!"))
    return(NULL)
  }
  # for comp_tsout_ens we can get data from obj$comp_coords
  # otherwise data needs to be passe in as matrix X
  if(is.null(obj$comp_coords)){
    if(is.null(X)){
      print(paste0("Error: need data matrix X to plot mv_tsout_ens output."))
      return(NULL)
    }
  }
  else{
    X <- obj$comp_coords
  }
  ts_proj <- as.matrix(X) %*% loading_mat
  colnames(ts_proj) <- paste0(method, "_", 1:ncol(loading_mat))
  ts_proj <- tibble::as_tibble(ts_proj) %>%
    dplyr::mutate(t = 1:nrow(X)) %>%
    tidyr::pivot_longer(-.data$t, values_to = "value", names_to = "name")

  ggplot2::ggplot(ts_proj, ggplot2::aes(x=.data$t, y=.data$value)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = obj$outliers[,"Indices"], color="red", alpha=0.8, size=0.1) +
    ggplot2::facet_wrap(~name, ncol = 2) +
    ggplot2::theme_bw()
}

#' Plot "biplot" for different decomposition methods.
#'
#' The final graph shows the data points projected onto the
#' first two components, together with the loadings as axes.
#' Note that this only works well for centered and scaled data.
#'
#' @param obj The output from \code{comp_tsout_ens} or \code{mv_tsout_ens} functions.
#' @param X The data matrix used as input to \code{mv_tsout_ens} (not needed if \code{obj} is output from \code{comp_tsout_ens}).
#' @param method The decomposition method, choose between "pca" (default), "dobin", "ics" or "ica".
#' @return A ggplot showing the biplot.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
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
#'
#' out1 <- mv_tsout_ens(X, compr=2, fast=FALSE)
#' X_scaled <- scale(X)
#' plot_biplot(out1, X = X_scaled, method = "pca")
#'
#' X <- X/rowSums(X)
#' out2 <- comp_tsout_ens(X, compr=2, fast=FALSE)
#' plot_biplot(out2)
#'
#' @export
plot_biplot <- function(obj, X = NULL, method = "pca"){
  # initial check that loadings for requested method are in obj
  loading_mat <- obj[[paste0(method, "_loadings")]]
  if(! is.matrix(loading_mat)){
    print(paste0("Error: loadings for method ", method, " not found!"))
    return(NULL)
  }
  # for comp_tsout_ens we can get data from obj$comp_coords
  # otherwise data needs to be passe in as matrix X
  if(is.null(obj$comp_coords)){
    if(is.null(X)){
      print(paste0("Error: need data matrix X to plot mv_tsout_ens output."))
      return(NULL)
    }
  }
  else{
    X <- obj$comp_coords
  }
  ts_proj <- as.matrix(X) %*% loading_mat
  colnames(ts_proj) <- paste0("comp", 1:ncol(loading_mat))
  ts_proj <- tibble::as_tibble(ts_proj) %>%
    dplyr::mutate(t = 1:nrow(X))
  loading_mat <- tibble::tibble(comp1 = loading_mat[,1], comp2 = loading_mat[,2]) %>%
    dplyr::mutate(l = paste0("V", 1:nrow(loading_mat)))

  ggplot2::ggplot(ts_proj,
                  ggplot2::aes(.data$comp1, .data$comp2,
                               color=(.data$t %in% obj$outliers[,"Indices"]))) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = c("grey", "red")) +
    ggplot2::geom_segment(data = loading_mat,
                 mapping = ggplot2::aes(x=0, xend=.data$comp1, y=0, yend=.data$comp2),
                 color="forestgreen") +
    ggplot2::geom_text(data=loading_mat,
              mapping = ggplot2::aes(x=.data$comp1, y=.data$comp2, label=.data$l),
              size=4,
              color="forestgreen") +
    ggplot2::xlab(paste0(method, "_", 1)) +
    ggplot2::ylab(paste0(method, "_", 2)) +
    ggplot2::theme_bw() +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "none")
}

#' Plot all decomposed time series from comp_tsout_ens or mv_tsout_ens output.
#'
#' @param obj The output from \code{comp_tsout_ens} or \code{mv_tsout_ens} functions.
#' @param X The data matrix used as input to \code{mv_tsout_ens} (not needed if \code{obj} is output from \code{comp_tsout_ens}).
#' @return A ggplot showing the time series with facets by decomposition method.
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
#' out <- mv_tsout_ens(X, m1=c(1,2,4), compr=2)
#' plot_decomposed_all(out, X=X)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export
plot_decomposed_all <- function(obj, X = NULL){
  # for comp_tsout_ens we can get data from obj$comp_coords
  # otherwise data needs to be passe in as matrix X
  if(is.null(obj$comp_coords)){
    if(is.null(X)){
      print(paste0("Error: need data matrix X to plot mv_tsout_ens output."))
      return(NULL)
    }
  }
  else{
    X <- obj$comp_coords
  }
  ncomp <- ncol(obj$pca_loadings)
  ts_pca <- as.matrix(X) %*% obj$pca_loadings
  colnames(ts_pca) <- paste0("pca", 1:ncomp)
  ts_dobin <- as.matrix(X) %*% obj$dobin_loadings
  colnames(ts_dobin) <- paste0("dobin", 1:ncomp)
  ts_ica <- as.matrix(X) %*% obj$ica_loadings
  colnames(ts_ica) <- paste0("ica", 1:ncomp)
  if (is.matrix(obj$ics_loadings)){
    ts_ics <- as.matrix(X) %*% obj$ics_loadings
    colnames(ts_ics) <- paste0("ics", 1:ncomp)
  }
  else ts_ics <- NULL

  tibble::as_tibble(cbind(ts_pca, ts_dobin, ts_ica, ts_ics)) %>%
    dplyr::mutate(t = 1:length(ts_pca[,1])) %>%
    tidyr::pivot_longer(-.data$t,
                 names_to = c("method", "component"),
                 names_pattern = "(?<Alpha>[a-zA-Z]*)(?<Numeric>[0-9]*)",
                 values_to = "value") %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$t, y = .data$value, color = .data$component)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = obj$outliers[,"Indices"], color="red", alpha=0.8, size=0.1) +
    ggplot2::facet_wrap(~method, ncol = 2, scales = "free") +
    ggplot2::theme_bw()
}
