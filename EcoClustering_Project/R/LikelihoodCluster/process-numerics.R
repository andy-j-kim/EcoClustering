#' @param quantile_probs_numeric Quantiles used to check if numerics have
#'   sufficient variation.
#' @param miss.cut eliminates explanatory (X) variables with proportion
#' @param impute Type of missing value imputation to conduct. One of: "zero",
#'   "median", "knn" (default). Note: knn results in the covariate data being centered/scaled.

process_numerics =
  function(data.num,
           quantile_probs_numeric = c(0.1, 0.9),
           miss.cut=0.5,
           impute = "median",
           verbose = FALSE) {

    
  n = nrow(data.num)

  if (ncol(data.num) > 0) {
    num_cols = ncol(data.num)
    if (verbose) cat("Processing numerics. Start count:", num_cols, "\n")

    # Remove columns where the 0.1 and 0.9 quantiles have the same value, i.e. insufficent variation.
    # TODO: set this is a configurable setting?
    if (!is.null(quantile_probs_numeric)) {
      data.num = restrict_by_quantiles(data.num, quantile_probs = quantile_probs_numeric)
    }

    if (verbose) {
      num_dropped = num_cols - ncol(data.num)
      if (num_dropped > 0) {
        cat("Dropped", num_dropped, "numerics due to lack of variation.\n")
      } else {
        cat("No numerics dropped due to lack of variation.\n")
      }
    }

    # Save how many numeric variables we have in this dataframe.
    num_numeric = ncol(data.num)
  } else {
    num_numeric = 0L
  }

    ###############
    # Missing Basis for numeric variables, post-binning.

    n.cont = nrow(data.num)

    sum_nas = apply(data.num, 2, sum_na)
    nmesX = colnames(data.num)
    miss.cont = NULL
    nmesm = NULL

    # Create imputed version of the numeric dataframe.
    # This is used as the adjustment set, but not used when generating the treatment assignment vector.
    data.numW = data.num

    # Loop over each binned numeric variable.
    # TODO: do this as part of the binning process.
    for (k in 1:num_numeric) {
      # Check if that variable has any missing values.
      if (sum_nas[k] > 0) {
        # The effect is that the basis is set to 1 if it exists and 0 if it's missing.
        ix = as.numeric(!is.na(data.numW[, k]))
        miss.cont = cbind(miss.cont, ix)
        # TODO: convert to paste0
        nmesm = c(nmesm, paste("Imiss_", nmesX[k], sep = ""))
      }
    }
    # if(is.null(miss.cont)){miss.cont= rep(1,n.cont)}
    colnames(miss.cont) = nmesm

    # Impute missing data in numeric columns.
    if (impute == "zero") {
      data.numW[is.na(data.num)] = 0
      impute_info = 0
    } else if (impute == "median") {
      impute_info = caret::preProcess(data.num, method = "medianImpute")
      data.numW = predict(impute_info, data.num)
    } else if (impute == "mean") {
      stop("Mean imputation not implemented yet. Please use another imputation method.")
    } else if (impute == "knn") {
      # NOTE: this also results in caret centering and scaling the data.
      impute_info = caret::preProcess(data.num, method = "knnImpute")
      data.numW = predict(impute_info, data.num)
    }

    # Confirm that there are no missing values remaining in data.numW

  (results =
      list(
        num_numeric = num_numeric,
        miss.cont = miss.cont,
        data.num = data.num,
        data.numW = data.numW,
        impute_info = impute_info
  ))
}
