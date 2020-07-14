#' Apportions outlier scores to composites.
#'
#' @param obj The output of \code{comp_tsout_ens}
#'
#' @return A list with the following components:
#' \item{\code{scores_ori}}{The apportioned outlier scores.}
#' \item{\code{scores_scaled}}{The apportioned outlier scores rescaled so that the sum of the multivariate outlier scores is equal to the total outlier score at that time point.}
#'
#' @export
apportion_scores_mv <- function(obj){
  # obj is a mv_tsout_ens output
  if(is.null(obj$pca_loadings)){
    return('The input needs to be the object returned by the function mv_tsout_ens!')
  }

  outmat4D <- obj$outmat4D
  decomp_wts <- obj$decomp_wts
  out_wts <- obj$wts
  outmat3D <- out_wts[1]*outmat4D[ ,1, , ] + out_wts[2]*outmat4D[ ,2, , ] + out_wts[3]*outmat4D[ ,3, , ] + out_wts[4]*outmat4D[ ,4, , ]
  scores_dobin <- outmat3D[ , ,1] %*% diag(decomp_wts[ ,1])
  scores_pca <- outmat3D[ , ,2] %*% diag(decomp_wts[ ,2])
  scores_ics <- outmat3D[ , ,3] %*% diag(decomp_wts[ ,3])
  scores_ica <- outmat3D[ , ,4] %*% diag(decomp_wts[ ,4])

  a_dobin <-  obj$dobin_loadings%*% t(scores_dobin)
  a_pca <-  obj$pca_loadings%*% t(scores_pca)
  a_ics <-  obj$ics_loadings%*% t(scores_ics)
  a_ica <-  obj$ica_loadings%*% t(scores_ica)

  a <- abs(a_dobin) + abs(a_pca) + abs(a_ics) + abs(a_ica)

  # Rescaling the value to match the outlier score
  obj$all <- as.data.frame(obj$all)
  inds <- obj$all$Indices
  tscore <- obj$all$Total_Score
  a2 <- a[ ,inds]
  if(is.null(dim(a2))){
    a_sums <- sum(a2)
    ratio <- tscore/a_sums
    a2 <- a2*ratio
  }else{
    a_sums <- apply(a2, 2, sum)
    ratio <- tscore/a_sums
    a2 <- sweep(a2, 2, FUN="*", ratio)
  }


  ascaled <- a
  ascaled[ ,inds] <- a2
  a_ori <- t(a[ ,inds])
  rownames(a_ori) <- paste(inds)

  a_scaled <- t(ascaled[ ,inds])
  rownames(a_scaled) <- paste(inds)

  out <- list()
  out$scores_ori<- a_ori
  out$scores_scaled <- a_scaled
  return(out)
}


