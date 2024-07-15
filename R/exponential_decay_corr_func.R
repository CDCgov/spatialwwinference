#' Generates the correlation matrix for an exponential decay correlation function.
#'
#' @param corr_function_params Sturcuted as follows : {dist.matrix, phi, l}.
#'
#' @return Correlation matrix.
#' 
exponential_decay_corr_func <- function(corr_function_params = list(dist_matrix = NULL, phi = NULL, l = NULL)) {
  
  # error managing
 stopifnot(
    "corr_function_params : NOT STRUCTURED CORRECTLY!!!\n
             List names should be : {dist_matrix, phi, l}" =
      names(corr_function_params) == c("dist_matrix", "phi", "l")
  )
  stopifnot(
    "corr_function_params : DOES NOT CONTAIN VALID VALUES!!!\n
             List should be {matrix, int, int}" =
      is.matrix(corr_function_params$dist_matrix) &
        is.numeric(corr_function_params$phi) &
        is.numeric(corr_function_params$l)
  )
  stopifnot(
    "corr_function_params : DIST.MATRIX NOT SQUARE!!!\n
             Make sure dist_matrix is square" =
      nrow(corr_function_params$dist_matrix) == ncol(corr_function_params$dist_matrix)
  )

  # presets
  dist_matrix <- corr_function_params$dist_matrix
  phi <- corr_function_params$phi
  l <- corr_function_params$l
  corr_matrix <- matrix(data = 0, nrow = nrow(dist_matrix), ncol = ncol(dist_matrix))

  for (i in 1:nrow(dist_matrix)) {
    for (j in 1:ncol(dist_matrix)) {
      d_ij <- dist_matrix[i, j]
      corr_matrix[i, j] <- exp(-(d_ij / phi)^l)
     }
   }
  
  return(corr_matrix)
}