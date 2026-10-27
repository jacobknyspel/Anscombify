
### This file includes the functions for coordinate transformation

# Function to return list of Anscombe Quartet target statistics
anscombe_target <- function() {
  list(
    mean = c(x = 9, y = 7.5),
    cov = matrix(
      c(11, 5.5, 5.5, 4.125), 2,
      dimnames = list(c("x", "y"), c("x", "y"))
    ),
    label = "Canonical targets"
  )
}

# Function to apply Affine transformation to coordinates, mapping them to target statistics
map_to_target <- function(df, target_mean, target_cov) {
  
  if (!all(c("x", "y") %in% names(df))) {
    stop("Coordinates must have x and y columns.")
  }
  
  # Convert coordinates to matrix
  pts <- as.matrix(df[, c("x", "y")])
  
  if (!is.numeric(pts) || nrow(pts) < 3L || any(!is.finite(pts))) {
    stop("At least three coordinate pairs are required.")
  }
  
  if (length(target_mean) != 2L || any(!is.finite(target_mean)) || !is.matrix(target_cov) || !identical(dim(target_cov), c(2L, 2L)) || any(!is.finite(target_cov)) || !isSymmetric(target_cov)) {
    stop("Invalid target mean or covariance matrix.")
  }
  
  if (min(eigen(target_cov, symmetric = TRUE, only.values = TRUE)$values) <= 0) {
    stop("The target covariance must be positive definite.")
  }
  
  # Calculate original coordinate means
  src_mean <- colMeans(pts)
  
  # Center coordinates so means become zero
  centered <- sweep(pts, 2, src_mean, "-")
  
  # Calculate original coordinate covariance matrix
  src_cov <- stats::cov(centered)
  
  # Calculate eigenvalues of covariance matrix
  eigenvalues <- eigen(src_cov, symmetric = TRUE, only.values = TRUE)$values
  
  if (max(eigenvalues) <= 0 || min(eigenvalues) <= 1e-10 * max(eigenvalues)) {
    stop("Eigendecompositon of the original coordinates failed. This can occur is all coordinates are exactly on (or very close to) a straight line. Try an image with greater variation in the x and y axes.")
  }

  # Apply transformation
  B <- backsolve(chol(src_cov), chol(target_cov))
  transformed <- sweep(centered %*% B, 2, target_mean, "+")
  
  # Compile output
  colnames(transformed) <- c("x", "y")
  output <- df
  output[, c("x", "y")] <- transformed
  
  # Calculate errors between transformed and target means/covariances
  mean_error <- max(abs(colMeans(transformed) - target_mean))
  covariance_error <- max(abs(stats::cov(transformed) - target_cov))
  
  # Return output
  list(
    data = output,
    matrix = B,
    offset = drop(target_mean - src_mean %*% B),
    source_mean = src_mean,
    source_cov = src_cov,
    mean_error = mean_error,
    covariance_error = covariance_error
  )
}

# Function to compile coordinate descriptive statistics
coordinate_stats <- function(df) {
  S <- stats::cov(df[, c("x", "y")])
  slope <- S[1,2] / S[1,1]
  c(
    mean_x = mean(df$x),
    mean_y = mean(df$y),
    var_x = S[1,1],
    var_y = S[2,2],
    covariance = S[1, 2],
    correlation = stats::cor(df$x, df$y),
    slope = slope,
    intercept = mean(df$y)-slope*mean(df$x)
  )
}

# Function to compile target descriptive statistics
target_stats <- function(target) {
  S <- target$cov
  slope <- S[1, 2] / S[1, 1]
  unname(c(
    mean_x = target$mean[1],
    mean_y = target$mean[2],
    var_x = S[1,1],
    var_y = S[2,2],
    covariance = S[1,2],
    correlation = S[1,2] / sqrt(S[1,1] * S[2,2]),
    slope = slope,
    intercept = target$mean[2] - slope * target$mean[1]
  ))
}

# Function to return overall results table
stats_table <- function(raw, transformed, target) {
  desired <- target_stats(target)
  observed <- unname(coordinate_stats(transformed))
  data.frame(
    Statistic = c("Mean x", "Mean y", "Variance x", "Variance y", "Covariance", "Correlation", "Regression slope", "Regression intercept"),
    `Before` = unname(coordinate_stats(raw)),
    `After` = observed,
    `Anscombe's Quartet` = desired,
    check.names = FALSE
  )
}
