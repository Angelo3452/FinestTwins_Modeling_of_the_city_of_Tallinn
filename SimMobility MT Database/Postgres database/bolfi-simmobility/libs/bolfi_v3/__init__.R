suppressMessages(library(tidyverse))
suppressMessages(library(reshape2))
suppressMessages(library(purrr))
suppressMessages(library(GPfit))
suppressMessages(library(optparse))
suppressMessages(library(randomForest))
suppressMessages(library(mixtools))
suppressMessages(library(mclust))
suppressMessages(library(NbClust))
suppressMessages(library(mlegp))
suppressMessages(library(ranger))
suppressMessages(library(assertthat))
suppressMessages(library(DEoptim))
suppressMessages(library(parallel))
suppressMessages(library(MLmetrics))
## --------------------------- Functions ----------------------------------
fn_gmm_mixtools <- function(vals){
  corrected_vals <- vals #+ runif(length(vals), 0, 0.0001)
  
  return( tryCatch({
    gmm_fit <- normalmixEM(corrected_vals, maxrestarts = 3, verb = F, maxit = 300)
    
    list(
      mu = matrix(gmm_fit$mu, ncol=1),
      sigma = matrix(gmm_fit$sigma, ncol=1),
      lambda = matrix(gmm_fit$lambda, ncol=1)
    )
  },
  error = function(ex){
    return( NULL )
  }) )
}
fn_gmm_mixtools_v2 <- function(vals){
  corrected_vals <- vals + runif(length(vals), 0, 0.0001)
  return( tryCatch({
    d <- density(corrected_vals)
    df <- data.frame(d[c("x", "y")])[c(F, diff(diff(d$y)>=0)<0),]
    y_max <- max(df[,"y"])
    y_max_limit <- y_max * 0.2 # Everything higher than 20% of the max will be considered as a peak
    x_mean <- df[which(df[,"y"] > y_max_limit),"x"]
    
    gmm_fit <- normalmixEM(corrected_vals, mu = x_mean, maxrestarts = 3, verb = F, maxit = 300 )
    
    list(
      mu = matrix(gmm_fit$mu, ncol=1),
      sigma = matrix(gmm_fit$sigma, ncol=1),
      lambda = matrix(gmm_fit$lambda, ncol=1)
    )
  },
  error = function(ex){
    print(ex)
    return( NULL )
  }) )
}
fn_gmm_mclust <- function(vals){
  corrected_vals <- vals + runif(length(vals), 0, 0.0001)
  gmm_fit <- Mclust(corrected_vals, modelNames = c("V")) #densityMclust(corrected_vals, modelNames = c("V"))


  if( "pro" %in% names(gmm_fit$parameters)){
    lambda_est <- gmm_fit$parameters$pro
  } else {
    lambda_est <- rep( (1.0/gmm_fit$G), gmm_fit$G )
  }

  output <- tryCatch({
    list(
      mu = gmm_fit$parameters$mean,
      sigma = sqrt(gmm_fit$parameters$variance$sigmasq),
      lambda = lambda_est
    )
  }, error = function(erx){
    cat(erx)
    return( NULL )
  })

  return( output )
}

fn_gmm_kclust <- function(vals){
  corrected_vals <- vals + runif(length(vals), 0, 0.0001)
  num_clusters <- tryCatch({ NbClust(as.matrix(corrected_vals), method = "centroid", index = "ratkowsky", max.nc = 5) }, error = function(err){ return(NULL) })
  
  if(is.null(num_clusters)) {
    num_clusters <- list(
      Best.nc = 1,
      Best.partition = rep(1, length(corrected_vals))
    )
  } else {
    if(is.infinite(num_clusters$Best.nc[1])){
      num_clusters <- list(
        Best.nc = 1,
        Best.partition = rep(1, length(corrected_vals))
      )
    }  
  }
  
  # tryCatch({ 1:num_clusters$Best.nc[1]  }, error = function(ex){ 
  #   print(ex)
  #   print(num_clusters)
  #   list(vals = vals,corrected_vals = corrected_vals) %>% saveRDS(paste0("objects/failed_iteration_",round(as.numeric(Sys.time())*1000, digits=0),".Rds"))
  # })
  
  clustered_stats <- lapply(1:num_clusters$Best.nc[1], function(c_id, c_vals, c_partitions){
    clustered_vals <- c_vals[which(c_partitions == c_id)]
    clustered_summary <- list(lambda = length(clustered_vals)/length(c_vals), mu = mean(clustered_vals, na.rm = T), sigma = sd(clustered_vals, na.rm = T))
    clustered_summary$sigma[is.na(clustered_summary$sigma)] <- 0
    
    clustered_summary
  }, c_vals = corrected_vals, c_partitions = num_clusters$Best.partition)
  
  list(
    mu = unlist(lapply(clustered_stats, '[[', 'mu')),
    sigma = unlist(lapply(clustered_stats, '[[', 'sigma')),
    lambda = unlist(lapply(clustered_stats, '[[', 'lambda'))
  )
}

fn_utility <- function(utility_func, ...){
  return( utility_func(...) )
}
fn_utility_ei <- function(traits, optimal_response, complement_u = F, estimated_sigma = NULL){ # Expected improvement
  m <- traits$mu
  s <- traits$sigma
  
  ## If sigma is estamated with other other than empirical estimation
  if(!is.null(estimated_sigma)){
    s <- estimated_sigma
  }
  
  #if(any(s == 0)) return(0)
  s[which(s[,1] == 0),] <- NA
  
  gamma <- (optimal_response - m)/s
  u_val_mat <- s * (gamma * pnorm(gamma) + dnorm(gamma) )
  #u_val_mat <- matrix( c(u_val), nrow = length(m)  )
  colnames(u_val_mat) <- c("ei")
  
  if(complement_u){
    list_gMix <- fn_utility_global_gmix(traits, optimal_response)  
    
    complement_utilities_fn <- list(
      list(f = fn_utility_lcb, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_gmix_agg_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_agg_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) )
    )
    
    c_utilities_val <- do.call(cbind, lapply(complement_utilities_fn, function(u_f){ do.call(u_f$f, u_f$params) }))
    u_val_mat <- cbind(u_val_mat,c_utilities_val)
  }
  
  return( u_val_mat )
}
fn_utility_ei_ij <- function(traits, optimal_response, complement_u = F){
  sigma.IJ <- rInfJack(traits$values, Reduce(cbind,traits$custom$inbag), calibrate = TRUE)
  fn_utility_ei(traits, optimal_response, complement_u, estimated_sigma = as.numeric(sigma.IJ[,"var.hat"]))
}

fn_utility_lcb <- function(traits, optimal_response, kappa = 2.0, complement_u = F, estimated_sigma = NULL){ # Lower confidence bound
  m <- traits$mu
  s <- traits$sigma
  
  ## If sigma is estamated with other other than empirical estimation
  if(!is.null(estimated_sigma)){
    s <- estimated_sigma
  }
  
  u_val <- -1 * (m - kappa * s)
  u_val_mat <- matrix( c(u_val), nrow = length(m)  )
  colnames(u_val_mat) <- c("lcb")
  
  if(complement_u){
    list_gMix <- fn_utility_global_gmix(traits, optimal_response)  
    
    complement_utilities_fn <- list(
      list(f = fn_utility_ei, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_gmix_agg_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_agg_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) )
    )
    
    c_utilities_val <- do.call(cbind, lapply(complement_utilities_fn, function(u_f){ do.call(u_f$f, u_f$params) }))
    #u_val_mat <- cbind(u_val_mat,u_val_mat)
    u_val_mat <- cbind(u_val_mat,c_utilities_val)
  }
  
  return( u_val_mat )
}
fn_utility_lcb_ij <- function(traits, optimal_response, complement_u = F){
  sigma.IJ <- rInfJack(traits$values, Reduce(cbind,traits$custom$inbag), calibrate = TRUE)
  fn_utility_lcb(traits, optimal_response, complement_u, estimated_sigma = as.numeric(sigma.IJ[,"var.hat"]))
}

fn_utility_global_gmix <- function(traits, optimal_response, byrow = T){ # Expected improvement over mixture of gaussian
  apply_margin <- 1
  if(!byrow) apply_margin <- 2
  
  return( apply(traits$values, apply_margin, function(vals){
    #fit_gMix <- quietly(fn_gmm_mixtools_v2)(vals)$result
    fit_gMix <- quietly(fn_gmm_mixtools)(vals)$result
    #fit_gMix <- quietly(fn_gmm_mclust)(vals)$result
    #fit_gMix <- fn_gmm_kclust(vals) #quietly(fn_gmm_kclust)(vals)$result
    
    if(is.null(fit_gMix)){
      fit_gMix <- list(
        mu = matrix(mean(vals), ncol = 1),
        sigma = matrix(sd(vals), ncol = 1),
        lambda = matrix(1, ncol = 1)
      )
    }
    
    #Separate out 0's deviations (in case there are more than one component)
    null_sd_idx <- which(fit_gMix$sigma[,1] == 0)
    if(length(null_sd_idx) > 0 && nrow(fit_gMix$sigma) > 1){
      correct_sd_idx <- which(fit_gMix$sigma[,1] != 0)
      fit_gMix$mu <- fit_gMix$mu[correct_sd_idx,]
      fit_gMix$sigma <- fit_gMix$sigma[correct_sd_idx,]
      fit_gMix$lambda <- fit_gMix$lambda[correct_sd_idx,]
    }
    
    return(fit_gMix)
  }) )
}

fn_utility_gmix_agg_ei <- function(traits, optimal_response, byrow = T, complement_u = F, list_MM = NA){ # Expected improvement over mixture of gaussian
  list_gMix <- list_MM
  if(is.na(list_gMix)){
    list_gMix <- fn_utility_global_gmix(traits, optimal_response, byrow)  
  }
  
  ## Integral part of the utility calculation
  utilities_gMix <- do.call(rbind,unname(lapply(list_gMix, function(single_gMix, base_utility, bu){
    util_gMix <- base_utility(single_gMix,bu)
    t(single_gMix$lambda[,1])%*%util_gMix[,1]
  },
  base_utility = fn_utility_ei,
  bu = optimal_response)))
  colnames(utilities_gMix) <- c("gmix_agg_ei")
  
  if(complement_u){
    complement_utilities_fn <- list(
      list(f = fn_utility_ei, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_lcb, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_gmix_wmax_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_agg_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) )
    )
    
    c_utilities_val <- do.call(cbind, lapply(complement_utilities_fn, function(u_f){ do.call(u_f$f, u_f$params) }))
    #utilities_gMix <- cbind(utilities_gMix,utilities_gMix)
    utilities_gMix <- cbind(utilities_gMix,c_utilities_val)
  }
  
  return( utilities_gMix )
}
fn_utility_gmix_wmax_ei <- function(traits, optimal_response, byrow = T, complement_u = F, list_MM = NA){ # Expected improvement over mixture of gaussian
  list_gMix <- list_MM
  if(is.na(list_gMix)){
    list_gMix <- fn_utility_global_gmix(traits, optimal_response, byrow)  
  }
  
  ## Integral part of the utility calculation
  utilities_gMix <- do.call(rbind,unname(lapply(list_gMix, function(single_gMix, base_utility, bu){
    util_gMix <- base_utility(single_gMix,bu)
    util_gMix_max_idx <- which(util_gMix[,1] == max(util_gMix[,1]) )
    single_gMix$lambda[util_gMix_max_idx] * util_gMix[util_gMix_max_idx,]
  },
  base_utility = fn_utility_ei,
  bu = optimal_response)))
  colnames(utilities_gMix) <- c("gmix_wmax_ei")
  
  if(complement_u){
    complement_utilities_fn <- list(
      list(f = fn_utility_ei, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_lcb, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_gmix_agg_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_agg_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) )
    )
    
    c_utilities_val <- do.call(cbind, lapply(complement_utilities_fn, function(u_f){ do.call(u_f$f, u_f$params) }))
    #utilities_gMix <- cbind(utilities_gMix,utilities_gMix)
    utilities_gMix <- cbind(utilities_gMix,c_utilities_val)
  }
  return( utilities_gMix )
}
fn_utility_gmix_max_ei <- function(traits, optimal_response, byrow = T, complement_u = F, list_MM = NA){ # Expected improvement over mixture of gaussian
  list_gMix <- list_MM
  if(is.na(list_gMix)){
    list_gMix <- fn_utility_global_gmix(traits, optimal_response, byrow)  
  }
  
  ## Integral part of the utility calculation
  utilities_gMix <- do.call(rbind,unname(lapply(list_gMix, function(single_gMix, base_utility, bu){
    util_gMix <- base_utility(single_gMix,bu)
    util_gMix_max_idx <- which(util_gMix[,1] == max(util_gMix[,1]) )
    util_gMix[util_gMix_max_idx,]
  },
  base_utility = fn_utility_ei,
  bu = optimal_response)))
  colnames(utilities_gMix) <- c("gmix_max_ei")
  
  if(complement_u){
    complement_utilities_fn <- list(
      list(f = fn_utility_ei, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_lcb, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_gmix_agg_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_agg_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) )
    )
    
    c_utilities_val <- do.call(cbind, lapply(complement_utilities_fn, function(u_f){ do.call(u_f$f, u_f$params) }))
    #utilities_gMix <- cbind(utilities_gMix,utilities_gMix)
    utilities_gMix <- cbind(utilities_gMix,c_utilities_val)
  }
  return( utilities_gMix )
}
fn_utility_gmix_agg_lcb <- function(traits, optimal_response, byrow = T, complement_u = F, list_MM = NA){ # Lower confidence bound over mixture of gaussian
  list_gMix <- list_MM
  if(is.na(list_gMix)){
    list_gMix <- fn_utility_global_gmix(traits, optimal_response, byrow)  
  }
  
  ## Integral part of the utility calculation
  utilities_gMix <- do.call(rbind,unname(lapply(list_gMix, function(single_gMix, base_utility, bu){
    util_gMix <- base_utility(single_gMix,bu)
    t(single_gMix$lambda)%*%util_gMix
  },
  base_utility = fn_utility_lcb,
  bu = optimal_response)))
  colnames(utilities_gMix) <- c("gmix_agg_lcb")
  
  if(complement_u){
    complement_utilities_fn <- list(
      list(f = fn_utility_ei, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_lcb, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_gmix_agg_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) )
    )
    
    c_utilities_val <- do.call(cbind, lapply(complement_utilities_fn, function(u_f){ do.call(u_f$f, u_f$params) }))
    #utilities_gMix <- cbind(utilities_gMix,utilities_gMix)
    utilities_gMix <- cbind(utilities_gMix,c_utilities_val)
  }
  return( utilities_gMix )
}
fn_utility_gmix_wmax_lcb <- function(traits, optimal_response, byrow = T, complement_u = F, list_MM = NA){ # Expected improvement over mixture of gaussian
  list_gMix <- list_MM
  if(is.na(list_gMix)){
    list_gMix <- fn_utility_global_gmix(traits, optimal_response, byrow)  
  }
  
  ## Integral part of the utility calculation
  utilities_gMix <- do.call(rbind,unname(lapply(list_gMix, function(single_gMix, base_utility, bu){
    util_gMix <- base_utility(single_gMix,bu)
    util_gMix_max_idx <- which(util_gMix[,1] == max(util_gMix[,1]) )
    single_gMix$lambda[util_gMix_max_idx] * util_gMix[util_gMix_max_idx,]
  },
  base_utility = fn_utility_lcb,
  bu = optimal_response)))
  colnames(utilities_gMix) <- c("gmix_wmax_lcb")
  
  if(complement_u){
    complement_utilities_fn <- list(
      list(f = fn_utility_ei, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_lcb, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_gmix_agg_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_agg_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) )
    )
    
    c_utilities_val <- do.call(cbind, lapply(complement_utilities_fn, function(u_f){ do.call(u_f$f, u_f$params) }))
    #utilities_gMix <- cbind(utilities_gMix,utilities_gMix)
    utilities_gMix <- cbind(utilities_gMix,c_utilities_val)
  }
  return( utilities_gMix )
}
fn_utility_gmix_max_lcb <- function(traits, optimal_response, byrow = T, complement_u = F, list_MM = NA){ # Expected improvement over mixture of gaussian
  list_gMix <- list_MM
  if(is.na(list_gMix)){
    list_gMix <- fn_utility_global_gmix(traits, optimal_response, byrow)  
  }
  
  ## Integral part of the utility calculation
  utilities_gMix <- do.call(rbind,unname(lapply(list_gMix, function(single_gMix, base_utility, bu){
    util_gMix <- base_utility(single_gMix,bu)
    util_gMix_max_idx <- which(util_gMix[,1] == max(util_gMix[,1]) )
    util_gMix[util_gMix_max_idx,]
  },
  base_utility = fn_utility_lcb,
  bu = optimal_response)))
  colnames(utilities_gMix) <- c("gmix_max_lcb")
  
  if(complement_u){
    complement_utilities_fn <- list(
      list(f = fn_utility_ei, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_lcb, params = list(traits = traits, optimal_response = optimal_response) ),
      list(f = fn_utility_gmix_agg_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_max_ei, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_agg_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) ),
      list(f = fn_utility_gmix_wmax_lcb, params = list(traits = traits, optimal_response = optimal_response, list_MM = list_gMix) )
    )
    
    c_utilities_val <- do.call(cbind, lapply(complement_utilities_fn, function(u_f){ do.call(u_f$f, u_f$params) }))
    #utilities_gMix <- cbind(utilities_gMix,utilities_gMix)
    utilities_gMix <- cbind(utilities_gMix,c_utilities_val)
  }
  return( utilities_gMix )
}

fn_eval_utility <- function(ds,surrogate_predict_fn,surrogate_model,surrogate_utility_fn, optimal_value, col_names, direction = 1, is_normalization_required, param_def, eval_complement = F){
  if(is.null( dim(ds))){
    ds <- matrix(ds, nrow = 1)
  }
  colnames(ds) <- col_names
  
  ## TODO: Check if the following code has been important for GP-like surrogates. It does the same normalization in the fn_predict function, as well.
  # if(is_normalization_required){
  #   ds <- unlist(fn_normalize_min_max(ds,param_def)[1,])
  # }
  
  #log4r::info(logger$LOGGER, paste0("[",settings$FINGERPRINT,"]: -- OPT Predicting with surrogate..." ))
  #surrogate_prediction <- surrogate_predict_fn(surrogate_model,as_tibble_row(ds))
  surrogate_predictions <- fn_predict(surrogate_predict_fn, surrogate_model, ds, param_def = param_def)
  #log4r::info(logger$LOGGER, paste0("[",settings$FINGERPRINT,"]: -- OPT Assembling prediction traits..." ))
  surrogate_traits <- fn_assemble_prediction_traits(surrogate_predictions)
  #log4r::info(logger$LOGGER, paste0("[",settings$FINGERPRINT,"]: -- OPT Estimating utility..." ))
  s_utility <- surrogate_utility_fn(traits = surrogate_traits, optimal_response = optimal_value, complement_u = eval_complement)
  #s_utility_mat <- matrix( (as.double(s_utility)*direction), ncol = 1  )
  #log4r::info(logger$LOGGER, paste0("[",settings$FINGERPRINT,"]: -- OPT Utility estimated!" ))
  # print(paste0("Utility: ", s_utility))
  s_utility
}

## Function that generates new points in space (parameter vector)
# Params:
# - type: function name to be called
# - params: tible with parameters
# - omit: vector of params to be omitted and use current_value
# - variation.rate: percentage of attributes to be changed
fn_acquisition <- function(type,param_space,...){
  ## If space has no records, it is initial parameter generation, hence need completely random sampling
  # if(nrow(param_space$space) == 0) {
  #   return( fn_acquisition_random_share(param_space, ...) )
  # }
  
  return( type(param_space,...) )
}
fn_sampling_lhs <- function(vals=NULL, param_space, sample_size, ...){
  dim_names <- param_space$definition$parameter
  sample_dim <- nrow(param_space$definition) #[which(param_space$definition[,"include"] == T),]
  
  if(hasName(shared.env$settings, "OPTIMISATION_CORES") && shared.env$settings$OPTIMISATION_CORES > 1){
    chunk_size <- ceiling(sample_size/shared.env$settings$OPTIMISATION_CORES)
    last_chunk_extra <- chunk_size*shared.env$settings$OPTIMISATION_CORES - sample_size
    
    prior_sample <- mclapply(1:shared.env$settings$OPTIMISATION_CORES, function(ch_id,ch_size,ch_extra){
      if(ch_id == shared.env$settings$OPTIMISATION_CORES) ch_size <- ch_size - ch_extra
      p_sample <- lhs::improvedLHS(ch_size, sample_dim)
      colnames(p_sample) <- dim_names
      fn_scaleup_standard(p_sample,param_space$definition)
    }, 
    ch_size = chunk_size,
    ch_extra = last_chunk_extra,
    mc.cores = shared.env$settings$OPTIMISATION_CORES )
    
    prior_sample <- Reduce("rbind",prior_sample)
  } else {
    prior_sample <- lhs::improvedLHS(sample_size, sample_dim)
    colnames(prior_sample) <- dim_names
    prior_sample <- fn_scaleup_standard(prior_sample, param_space$definition)
  }
  return(prior_sample)
}

fn_sampling_normal_optimal <- function(vals, param_space, sample_size, ...){
  dim_names <- param_space$definition$parameter
  sample_dim <- nrow(param_space$definition) #[which(param_space$definition[,"include"] == T),]
  sampling_intensity = ifelse(hasName(shared.env$settings, "MUTATION_RATE"), shared.env$settings$MUTATION_RATE, 0.5)
  sampling_deviation = ifelse(hasName(shared.env$settings, "MUTATION_SD"), shared.env$settings$MUTATION_SD, 1)
  
  if(hasName(shared.env$settings, "OPTIMISATION_CORES") && shared.env$settings$OPTIMISATION_CORES > 1){
    genereted_sample <- mclapply(matrix(1:sample_dim, nrow = 1), function(c_i, p_defs, s_size, s_intensity, s_sd, o_values){
      p_def <- p_defs[c_i,]
      p_lower_bound <- p_def[["lower_limit"]]
      p_upper_bound <- p_def[["upper_limit"]]
      p_replaced_size <- ceiling(s_size*s_intensity)
      p_current <- o_values[sample(1:nrow(o_values),1),c_i]  ## In case currently most optimal sets contains more than one sample set, then sample randomly one of them.
      p_sample <- rep(p_current, s_size)
      p_sample_replace <- sample(1:s_size, p_replaced_size, replace = F)
      
      #p_sample[p_sample_replace] <- runif(p_replaced_size,min = p_lower_bound,max = p_upper_bound) ## UNIFORMLY - SAMPLING
      p_sample[p_sample_replace] <- rnorm(p_replaced_size, mean = p_current, sd = s_sd) ## NORMAL AROUND THE OPTIMAL - SAMPLING
      
      return(p_sample)
    }, p_defs = param_space$definition, s_size = sample_size, s_intensity = sampling_intensity, s_sd = sampling_deviation, o_values = as.matrix(vals), mc.cores = shared.env$settings$OPTIMISATION_CORES)
    genereted_sample <- Reduce('cbind',genereted_sample)
  } else {
    
    genereted_sample <- apply(matrix(1:sample_dim, nrow = 1), 2, function(c_i, p_defs, s_size, s_intensity, s_sd, o_values){
      p_def <- p_defs[c_i,]
      p_lower_bound <- p_def[["lower_limit"]]
      p_upper_bound <- p_def[["upper_limit"]]
      p_replaced_size <- ceiling(s_size*s_intensity)
      p_current <- o_values[sample(1:nrow(o_values),1),c_i]  ## In case currently most optimal sets contains more than one sample set, then sample randomly one of them.
      p_sample <- rep(p_current, s_size)
      p_sample_replace <- sample(1:s_size, p_replaced_size, replace = F)
      
      #p_sample[p_sample_replace] <- runif(p_replaced_size,min = p_lower_bound,max = p_upper_bound) ## UNIFORMLY - SAMPLING
      p_sample[p_sample_replace] <- rnorm(p_replaced_size, mean = p_current, sd = s_sd) ## NORMAL AROUND THE OPTIMAL - SAMPLING
      
      return(p_sample)
    }, p_defs = param_space$definition, s_size = sample_size, s_intensity = sampling_intensity, s_sd = sampling_deviation, o_values = as.matrix(vals))
  }
  colnames(genereted_sample) <- dim_names
  
  return(genereted_sample)
}

fn_optimisation_uniform_prior <- function(vals, sampling_fn = fn_sampling_lhs, surrogate_predict_fn, surrogate_model, surrogate_utility_fn, iteration_space, param_range, param_space, omit, target, sample_size = 1, variation.rate = 0.3, opt_parallel = T, opt_cores = detectCores()-2, opt_store_intermediate = T, is_normalization_required = F, eval_complement = F, ...){
  prior_sample <- sampling_fn(vals, param_space, sample_size, ...)
  prior_names <- colnames(prior_sample)
  
  if(opt_parallel){
    sample_chunks <- lapply(split(1:nrow(prior_sample), ceiling(seq_along(1:nrow(prior_sample))/(nrow(prior_sample)/opt_cores) )), function(chunk_idx, p_sample){ return(p_sample[chunk_idx,]) }, p_sample = prior_sample)
    
    eval_targets <- mclapply(sample_chunks, function(s_chunk){
      eval_t <- fn_eval_utility(
        s_chunk,
        surrogate_predict_fn = surrogate_predict_fn,
        surrogate_model = surrogate_model,
        surrogate_utility_fn = surrogate_utility_fn,
        optimal_value = iteration_space$optimal$value,
        col_names = names(vals),
        is_normalization_required = is_normalization_required,
        param_def = param_space$definition,
        eval_complement = eval_complement
      )
      
      return(eval_t)
    }, mc.cores = opt_cores)
    
    eval_target <- Reduce(rbind,eval_targets)
  } else {
    eval_target <- fn_eval_utility(
      prior_sample,
      surrogate_predict_fn = surrogate_predict_fn,
      surrogate_model = surrogate_model,
      surrogate_utility_fn = surrogate_utility_fn,
      optimal_value = iteration_space$optimal$value,
      col_names = names(vals),
      is_normalization_required = is_normalization_required,
      param_def = param_space$definition,
      eval_complement = eval_complement
    )
  }
  
  prior_sample <- cbind(prior_sample, eval_target[,1])
  colnames(prior_sample)[ncol(prior_sample)] <- target
  
  ## If complementary acquisition function is set to be evaluated as well
  if(eval_complement && ncol(eval_target) > 1) prior_sample <- cbind(prior_sample, eval_target)
  
  best_val_idx <- which(prior_sample[,target] == max(prior_sample[,target]))[1]
  optimal_set <- prior_sample[best_val_idx, setdiff(prior_names,c(target)) ]
  
  output_obj <- list()
  output_obj$space <- matrix(optimal_set , nrow = 1)
  colnames(output_obj$space) <- names(vals)
  #output_obj$space <- as.data.frame(output_obj$space)
  output_obj$utility <- prior_sample[best_val_idx,target]
  
  if(opt_store_intermediate || eval_complement){
    output_obj$pop <- prior_sample
    output_obj$pop <- as.data.frame(output_obj$pop)
    output_obj$pop_utility <- prior_sample[,target]
    
    if(eval_complement) output_obj$complementary_utility_fn <- setdiff( colnames(prior_sample), c(prior_names,c(target)) )
  }
  
  return( output_obj )
}


fn_optimisation_quasi_newthon <- function(vals, sampling_fn = fn_sampling_lhs, surrogate_predict_fn, surrogate_model, surrogate_utility_fn, iteration_space, param_range, param_space, sample_size = 1, opt_parallel = T, opt_cores = detectCores()-2, opt_best_init = F, opt_init_pop_shufle_rate = 0.1, opt_store_intermediate = F, is_normalization_required = F,...){
  ## In case some parameters are enabled, then the param space need to be downgraded to the enabled dimensions
  enabled_idx <- 1:nrow(param_space$definition)
  if(c("enabled") %in% names(param_space$definition)) enabled_idx <- which(param_space$definition[,"enabled"] == T)
  
  reduced_vals <- vals[enabled_idx]
  reduced_param_space <- list()
  reduced_param_space$space <- param_space$space[,c(enabled_idx,length(param_space$space))] # <- maybe the target variable can be taken differently from the current approach - the last column
  reduced_param_space$definition <- param_space$definition[enabled_idx,]
  
  lower_bound <- reduced_param_space$definition[["lower_limit"]] # param_range$lower_limit
  upper_bound <- reduced_param_space$definition[["upper_limit"]]  # param_range$upper_limit
  
  singleton_optimisation <- function(idx, init.grid){
    optimal_set <- optim(
      par = init.grid[idx,],
      fn = fn_eval_utility, 
      surrogate_predict_fn = surrogate_predict_fn,
      surrogate_model = surrogate_model,
      surrogate_utility_fn = surrogate_utility_fn,
      optimal_value = iteration_space$optimal$value,
      col_names = names(reduced_vals),
      is_normalization_required = is_normalization_required,
      param_def = reduced_param_space$definition,
      method = "L-BFGS-B",
      lower = lower_bound, upper = upper_bound, 
      control = list(fnscale = -1, maxit = 1000, ndeps = rep(0.5,length(reduced_vals)) )) # , reltol = .Machine$double.eps, factr = 0.5
    
    return(optimal_set)
  }
  
  initGrid <- sampling_fn(reduced_vals, reduced_param_space, sample_size,...)
  
  if(opt_parallel){
    optimal_sets <- mclapply(1:sample_size, singleton_optimisation, init.grid = initGrid, mc.cores = opt_cores)
  } else {
    optimal_sets <- lapply(1:sample_size, singleton_optimisation, init.grid = initGrid)
  }
  
  optimal_sets <- optimal_sets[!sapply(optimal_sets,function(opt_set){ return( is.null(opt_set) || !hasName(opt_set, "convergence") )})]
  best_vals <- unlist(lapply(optimal_sets,'[[',c("value") ))
  best_val_idx <- which(best_vals == max(best_vals))
  
  ## This may need to be re-considered as there are cases when all restarts converge to 0 utility, which means that 
  if(length(best_val_idx) > 1) best_val_idx <- best_val_idx[1]
  
  optimal_set <- optimal_sets[[best_val_idx]]
  
  output_obj <- list()
  output_obj$space <- unlist(param_space$definition[,"initial"], use.names = F) # it can be tried with "vals" variable
  output_obj$space[enabled_idx] <- optimal_set$par
  output_obj$space <- matrix(output_obj$space, nrow = 1, byrow = T)
  colnames(output_obj$space) <- names(vals)
  
  output_obj$utility <- optimal_set$value
  
  if(opt_store_intermediate){
    output_obj$pop <- do.call("rbind", lapply( optimal_sets,'[[',c("par") ))
    colnames(output_obj$pop) <- names(reduced_vals)
    output_obj$pop_utility <- best_vals
  }
  
  return( output_obj )
}



fn_optimisation_differential_evolution <- function(vals, surrogate_predict_fn, surrogate_model, surrogate_utility_fn, iteration_space, param_range, param_space, sample_size = 1, opt_parallel = T, opt_cores = detectCores()-2, opt_best_init = F, opt_init_pop_shufle_rate = 0.1, opt_store_intermediate = F, is_normalization_required = F, ... ){
  ## In case some parameters are enabled, then the param space need to be downgraded to the enabled dimensions
  enabled_idx <- 1:nrow(param_space$definition)
  if(c("enabled") %in% names(param_space$definition)) enabled_idx <- which(param_space$definition[,"enabled"] == T)
  
  reduced_vals <- vals[enabled_idx]
  reduced_param_space <- list()
  reduced_param_space$space <- param_space$space[,c(enabled_idx,length(param_space$space))] # <- maybe the target variable can be taken differently from the current approach - the last column
  reduced_param_space$definition <- param_space$definition[enabled_idx,]
  
  lower_bound <- reduced_param_space$definition[["lower_limit"]] # param_range$lower_limit
  upper_bound <- reduced_param_space$definition[["upper_limit"]]  # param_range$upper_limit
  
  arg.params <- list(
    strategy = 6, p = 0.01, F = 0.8, c = 0.8, itermax = 1000, NP = 10*length(enabled_idx), trace = F)
  
  # if(opt_best_init){
  #   init_pop <- matrix(rep(as.numeric(vals),each=arg.params$NP),nrow=arg.params$NP)
  #   init_pop_shuffle <- matrix(runif(prod(dim(init_pop)), min = param_range$lower_limit*opt_init_pop_shufle_rate, max = param_range$upper_limit*opt_init_pop_shufle_rate), nrow = dim(init_pop)[1])
  #   arg.params$initialpop <- (init_pop + init_pop_shuffle)
  # }
  
  if(opt_store_intermediate){
    arg.params$storepopfrom <- 1
    arg.params$storepopfreq <- 10
  }
  
  optimal_sets <- lapply(1:sample_size, function(x){
    if(opt_parallel){
      cl <<- makeCluster(opt_cores)
      arg.params$parallelType <- 1
      arg.params$cluster <- cl
      clusterExport(arg.params$cluster,c("fn_assemble_prediction_traits","fn_gmm_mixtools","fn_gmm_mixtools_v2","fn_gmm_mclust","fn_utility_ei","settings","logger","fn_predict","fn_predict_rf","fn_predict_gp","fn_predict_rf_ranger","fn_utility_global_gmix"))
      clusterEvalQ(arg.params$cluster, { library("tidyverse"); library("reshape2"); library("purrr"); library("GPfit"); library("randomForest"); library("mixtools"); library("mclust"); library("NbClust"); library("mlegp"); library("ranger"); library("log4r") })
    }
    
    #log4r::info(logger$LOGGER, paste0("[",settings$FINGERPRINT,"]: OPT Optimisation starting - Restart = ", x ))
    opt_output <- DEoptim(fn_eval_utility, lower_bound, upper_bound,
          control = arg.params,
          surrogate_predict_fn = surrogate_predict_fn,
          surrogate_model = surrogate_model,
          surrogate_utility_fn = surrogate_utility_fn,
          optimal_value = iteration_space$optimal$value,
          col_names = names(reduced_vals),
          is_normalization_required = is_normalization_required,
          param_def = reduced_param_space$definition,
          direction = -1)
    #log4r::info(logger$LOGGER, paste0("[",settings$FINGERPRINT,"]: OPT Optimisation done! - Restart = ", x ))
    return( opt_output )
  })
  
  best_vals <- unlist(lapply(optimal_sets,'[[',c("optim","bestval") ))
  best_val_idx <- which(best_vals == max(best_vals))
  
  ## This may need to be re-considered as there are cases when all restarts converge to 0 utility, which means that 
  if(length(best_val_idx) > 1) best_val_idx <- best_val_idx[1]
  optimal_set <- optimal_sets[[best_val_idx]]
  
  output_obj <- list()
  output_obj$space <- unlist(param_space$definition[,"initial"], use.names = F) # it can be tried with "vals" variable
  output_obj$space[enabled_idx] <- optimal_set$optim$bestmem
  output_obj$space <- matrix(output_obj$space, nrow = 1, byrow = T)
  colnames(output_obj$space) <- names(vals)
  
  if(opt_store_intermediate){
    output_obj$pop <- optimal_set$member$storepop
  }
  
  return( output_obj )
}

fn_optimal_acquisition <- function(param_space, iter_space = NULL, sample_size = 1, omit = c(), inner_optimisation_fn = fn_optimisation_uniform_prior, sampling_fn = fn_sampling_lhs, surrogate_predict_fn = NA, surrogate_utility_fn = NA, ...){
  assert_that(is.function(inner_optimisation_fn), msg = "Provided optimisation function is not a valid function!")
  assert_that(is.function(sampling_fn), msg = "Provided sampling function is not a valid function!")
  
  sps <- param_space$space
  # Bounds (min and max) are taken by default from the first parameter
  if(nrow(param_space$definition) > 0){
    lower_limit <- as.numeric(param_space$definition[["lower_limit"]])
    upper_limit <- as.numeric(param_space$definition[["upper_limit"]])  
  } else {
    lower_limit <- -10
    upper_limit <- 10
  }
  
  # Get reference sample point for future mutation
  ref_param_instance <- fn_reference_sample(sps, ref_type = 0, ...)
  
  # Omit any not necessary parameters
  if(!is.null(omit) || length(omit) > 0) {
    ref_param_instance <- ref_param_instance %>% select(-any_of(omit))
  }
  
  optimal_ps <- inner_optimisation_fn(ref_param_instance, 
                                      sampling_fn = sampling_fn,
                                      surrogate_predict_fn = surrogate_predict_fn, 
                                      surrogate_model = iter_space$surrogate$model, 
                                      surrogate_utility_fn = surrogate_utility_fn, 
                                      iteration_space = iter_space, 
                                      param_range = list(lower_limit = lower_limit, upper_limit = upper_limit),
                                      param_space = param_space,
                                      sample_size = sample_size,
                                      omit = omit,
                                      ... 
                                    )
  optimal_ps
}

fn_reference_sample <- function(sps, ref_type = 0, target, ...){
  assert_that(ref_type %in% c(0,1), msg = "Chosen type of a reference sample point is not correct. Allowed values are euther 0 ('optimal') or 1 ('last').")
  
  if(nrow(sps) > 0){
    if(ref_type == 0){
      return( sps[ tail(which(sps[,target] == min(sps[,target]) ), n = 1),] )
    } 
    
    if(ref_type == 1) {
      return( sps %>% slice_tail(n=1) )
    }
  }
  
  # if(is.null(last_param_instance)){
  #   last_param_instance <- matrix(runif( ncol(sps), min = lower_limit, max = upper_limit  ), ncol = ncol(sps), byrow = T )
  #   colnames(last_param_instance) <- names(sps)
  # }
  return(NULL)
}

# fn_acquisition_v2_random_share <- function(param_space, iter_space = NULL, sample_size = 1, omit = c()){
#   # Random generation of parameter vector (for now random, later can be changed)
#   sps <- param_space$space
#   n <- ncol(sps)
#   sps_headers <- names(sps)
#   
#   # Get reference sample point for future mutation
#   ref_param_instance <- fn_reference_sample(sps, ref_type = 0, ...)
#   
#   # Omit any not necessary parameters
#   if(!is.null(omit) || length(omit) > 0) {
#     ref_param_instance <- ref_param_instance %>% select(-any_of(omit))
#   }
#   
#   
#   
#   
#   
#   
#   
#   # Bounds (min and max) are taken by default from the first parameter
#   if(nrow(param_space$definition) > 0){
#     lower_limit <- as.numeric(unlist(param_space$definition[,"lower_limit"]))
#     upper_limit <- as.numeric(unlist(param_space$definition[,"upper_limit"]))  
#   } else {
#     lower_limit <- rep(-10, n)
#     upper_limit <- rep(10, n)
#   }
#   
#   s_sampled <- setNames(lapply(1:n, function(i){
#     runif( sample_size, min = lower_limit[i], max = upper_limit[i]  )
#   }), sps_headers)
#   s_sampled <- bind_cols(s_sampled)
#   
#   
#   return( s_sampled )
# }


fn_enforce_bounds <- function(value, param, param_def){
  pps <- param_def[which(param_def[,"parameter"] == param),]
  
  if(nrow(pps) == 0) return( value );
  if(pps[1,"lower_limit"] > value ) return( as.numeric(pps[1,"lower_limit"]) )
  if(pps[1,"upper_limit"] < value ) return( as.numeric(pps[1,"upper_limit"]) )
  
  return( value );
}

fn_normalize_min_max <- function(ds, param_def){
  if(is.null(dim(ds))){
    ds <- data.frame(t(ds))
  }
  
  param_mat <- as.matrix(param_def %>% select(parameter,lower_limit,upper_limit))
  rownames(param_mat) <- param_mat[,1]
  params_used <- names(ds)[which(names(ds) %in% param_mat[,1]) ]
  params_omitted <- names(ds)[ ! names(ds) %in% params_used ]
  
  input_space_cols <- lapply(params_used, function(col_name, param_limits, l_ds){ 
    (l_ds[,col_name]-as.numeric(param_limits[col_name,"lower_limit"]))/( as.numeric(param_limits[col_name,"upper_limit"]) - as.numeric(param_limits[col_name,"lower_limit"]) ) 
  }, 
  param_limits = param_mat, 
  l_ds = ds )
  names(input_space_cols) <- params_used
  input_space <- bind_cols(input_space_cols)
  
  bind_cols(input_space,ds[,params_omitted])
}
fn_scaleup_standard <- function(ds, param_def){
  if(is.null(dim(ds))){
    m_col_names <- names(ds)
    ds <- matrix(ds, nrow = 1)
    colnames(ds) <- m_col_names
  }
  
  param_mat <- as.matrix(param_def %>% select(parameter,lower_limit,upper_limit))
  rownames(param_mat) <- param_mat[,1]
  params_used <- colnames(ds)[which(colnames(ds) %in% param_mat[,1]) ]
  params_omitted <- colnames(ds)[ ! colnames(ds) %in% params_used ]
  
  input_space_cols <- lapply(params_used, function(col_name, param_limits, l_ds){ 
    l_lower <- as.numeric(param_limits[col_name,"lower_limit"])
    l_upper <- as.numeric(param_limits[col_name,"upper_limit"])
    l_range <- l_upper - l_lower
    
    ((l_ds[,col_name]*l_range) + l_lower)
  }, 
  param_limits = param_mat, 
  l_ds = ds )
  i_space <- Reduce(cbind,input_space_cols)
  colnames(i_space) <- params_used
  
  if(length(params_omitted) > 0){
    i_space <- cbind(i_space,ds[,params_omitted])
    colnames(i_space) <- c(params_used,params_omitted)
  }
  
  return( i_space )
}

## Function to fit GP model
# Params:
# - ds: dataset (tibble)
# - target: target variable name
fn_fit_gp <- function(ds,target, method_params = list(KERNEL="exponential", POWER = 1.95), test_ds = NA){
  design_mat <- as.matrix( ds[,! names(ds) %in% c(target)] )
  
  output <- list(
    model = GP_fit(
      X = design_mat,
      Y = as.vector(ds %>% pull(!! target)),
      corr = list(type=method_params$KERNEL, power = method_params$POWER)
    )
  )
  
  if(!is.na(test_ds)){
    test_ds_output <- unlist(test_ds[,target])
    output$performance <- RMSE(y_pred = fn_predict_gp(output$model, test_ds[, (! names(test_ds) %in% c(target))  ])$mu, y_true = test_ds_output)
  }
  
  return( output )
}

## Function to fit GP model (with MLEGP library)
# Params:
# - ds: dataset (tibble)
# - target: target variable name
fn_fit_mlegp <- function(ds,target, method_params = list(), test_ds = NA){
  output <- list(
    model = mlegp(
      X = as.matrix( ds[,! names(ds) %in% c(target)] ),
      Z = as.vector(ds[, target]),
      verbose = 0
    )
  )
  
  if(!is.na(test_ds)){
    test_ds_output <- unlist(test_ds[,target])
    output$performance <- RMSE(y_pred = fn_predict_mlegp(output$model,test_ds[, (! names(test_ds) %in% c(target))  ])$mu, y_true = test_ds_output)
  }
  
  return( output )
}

## Function to fit Random Forest model
# Params:
# - ds: dataset (tibble)
# - target: target variable name
fn_fit_rf <- function(ds,target, method_params = list(RF_NSIZE = 1, RF_NTREE = 500), test_ds = NA, keep_inbag = T  ){
  if(is.na(method_params$RF_MTRY) || is.null(method_params$RF_MTRY) ) method_params$RF_MTRY <- floor(sqrt(ncol(ds)))
  
  output <- list(
    model = randomForest( as.formula(paste0(target," ~ .")), data = ds, nodesize = method_params$RF_NSIZE, ntree = method_params$RF_NTREE, mtry = method_params$RF_MTRY, keep.inbag = keep_inbag)
  )
  
  if(!is.na(test_ds)){
    test_ds_output <- unlist(test_ds[,target])
    output$performance <- RMSE(y_pred = fn_predict_rf(output$model,test_ds[, (! names(test_ds) %in% c(target))  ])$mu, y_true = test_ds_output)
  }
  return( output )
}

## Function to fit Random Forest model (ranger package)
# Params:
# - ds: dataset (tibble)
# - target: target variable name
fn_fit_rf_ranger <- function(ds, target, method_params = list(RF_NSIZE = 1, RF_NTREE = 500), test_ds = NA, keep_inbag = T ){
  if(is.na(method_params$RF_MTRY) || is.null(method_params$RF_MTRY) ) method_params$RF_MTRY <- floor(sqrt(ncol(ds)))
  
  output <- list(
    model = ranger( as.formula(paste0(target," ~ .")), data = ds, min.node.size = method_params$RF_NSIZE, num.trees = method_params$RF_NTREE, mtry = method_params$RF_MTRY, keep.inbag = keep_inbag)
  )
  
  if(!is.na(test_ds)){
    test_ds_output <- unlist(test_ds[,target])
    output$performance <- RMSE(y_pred = fn_predict_rf_ranger(output$model,test_ds[, (! names(test_ds) %in% c(target))  ])$mu, y_true = test_ds_output)
  }
  return( output )
}

## Function for creating folds for cross validation
## Params:
# - ds: dataset (tibble)
# - folds: number of folds
# - loo: is it leave-one-out strategy?
fn_do_folding <- function(ds, folds = 10, loo = F){
  if(nrow(ds) <= folds){
    folds <- nrow(ds)
    loo <- T
  }
  
  test_ds_idx <- list()
  
  if(loo){
    test_ds_idx <- as.list(sample(1:nrow(ds), folds, replace = F))
  } else {
    test_ds_idx <- lapply(1:folds, function(x, ds_idcs, fold_size){ sample(ds_idcs, fold_size, replace = T) }, ds_idcs = 1:nrow(ds), fold_size =  floor(nrow(ds)/folds) ) 
  }
  
  
  lapply(test_ds_idx, function(idcs, ds_mat){ 
      list(
        train = ds_mat[-idcs,],
        test = ds_mat[idcs,]
      )  
    },
    ds_mat = ds
  )
}

## Function to fit a model
# Params:
# - ds: dataset (tibble)
# - target: target variable name
fn_fit <- function(type,ds,target,param_def,is_normalization_required = F, method_params = NA, cv_parallel = T, cv_cores = (detectCores()-2),...){
  col_idx_target <- which(names(ds) == target)
  
  ## Apply filtering in accordance to the enabled feature
  if(c("enabled") %in% names(param_def)){
    enabled_idx <- c(which(param_def[,"enabled"] == T), col_idx_target)
    ds <- ds[,enabled_idx]
    col_idx_target <- length(enabled_idx)
  }
  
  ## Some methods requires normalization
  if(is_normalization_required){
      ds <- fn_normalize_min_max(ds,param_def)
  }
  
  if(!all(is.na(method_params))){
    method_params_expanded <- expand.grid(method_params)
    
    if(nrow(method_params_expanded) > 1){
      ## Create cross-validation folds
      cv_datasets <- fn_do_folding(ds)
      
      method_params_performance <- lapply(1:nrow(method_params_expanded), function(conf_idx, method_confs, folds_ds, cv_par, cv_par_cores, t_attr, fit_fun){
        method_conf <- as.list(unlist(method_confs[conf_idx,]))
        names(method_conf) <- colnames(method_confs)
        
        if(cv_par){
          folds_model_performance <- mclapply(folds_ds, function(f_ds, m_conf, t, l_fit_fun){
            l_fit_fun(f_ds$train, target = t, method_params = m_conf, test_ds = f_ds$test)$performance
          },
          mc.cores = cv_par_cores,
          m_conf = method_conf,
          t = t_attr,
          l_fit_fun = fit_fun)
        } else {
          folds_model_performance <- lapply(folds_ds, function(f_ds, m_conf, t, l_fit_fun){
            fitted_mdl <- l_fit_fun(f_ds$train, target = t, method_params = m_conf, test_ds = f_ds$test)
            #print(fitted_mdl)
            fitted_mdl$performance
          },
          m_conf = method_conf,
          t = t_attr,
          l_fit_fun = fit_fun)
        }
        
        list(mean_performance = mean(unlist(folds_model_performance), na.rm = T), partial_performance = unlist(folds_model_performance) )
      },
      method_confs = method_params_expanded,
      folds_ds = cv_datasets,
      cv_par = cv_parallel,
      cv_par_cores = cv_cores,
      t_attr = target,
      fit_fun = type )
      
      ## Retrieved performance of models build on different folds, find the best and retrain to whole dataset
      mean_performance <- unlist(lapply(method_params_performance,"[[",1))
      best_performer_idx <- which(mean_performance == min(mean_performance))[1]
      method_conf <- as.list(unlist(method_params_expanded[best_performer_idx,]))
      names(method_conf) <- colnames(method_params_expanded)
      
      list_models <- lapply(fn_split_space(ds), function(sub_ds_idx){ list(type(ds[,c(sub_ds_idx,col_idx_target)], target, method_params = method_conf,...)$model,sub_ds_idx)  } )
      return( list_models )
    }
    
    ## Not enough params combinations for tuning
    list_models <- lapply(fn_split_space(ds), function(sub_ds_idx){ list(type(ds[,c(sub_ds_idx,col_idx_target)], target, method_params = method_params,...)$model,sub_ds_idx) } )
    return( list_models )
  }
  
  list_models <- lapply(fn_split_space(ds), function(sub_ds_idx){ list(type(ds[,c(sub_ds_idx,col_idx_target)], target,...)$model,sub_ds_idx) } )
  return( list_models )
}

## Function for splitting the space into set of subspaces for learning/fitting many models
fn_split_space <- function(ds){
  no_subspaces <- 1
  if(hasName(shared.env$settings,"SUBSPACES_NUMBER")){
    no_subspaces <- shared.env$settings$SUBSPACES_NUMBER
  }
  
  size_subspace <- ceiling((ncol(ds)-1)/no_subspaces)
  if(hasName(shared.env$settings,"SUBSPACES_SIZE")){
    size_subspace <- ceiling((ncol(ds)-1)*shared.env$settings$SUBSPACES_SIZE)
  }
  
  overlap_subspaces <- F
  if(hasName(shared.env$settings,"SUBSPACES_OVERLAP")){
    overlap_subspaces <- shared.env$settings$SUBSPACES_OVERLAP
  }
  if((ncol(ds)-1) < (no_subspaces * size_subspace)) {
    overlap_subspaces <- T
  }
  
  output <- list()
  if(overlap_subspaces){
    output <- lapply(1:no_subspaces, function(idx, l_size, l_replace, l_ds){
      col_idx <- sort(sample( (ncol(l_ds)-1), l_size, replace = F )  )
      return(col_idx)
    },
    l_size = size_subspace,
    l_replace = overlap_subspaces,
    l_ds = ds)
  } else {
    col_set <- 1:(ncol(ds)-1)
    for(i in 1:no_subspaces){
      gen_vector <- sort(sample( col_set, size_subspace, replace = F ))
      output <- c(output, list(gen_vector))
      col_set <- base::setdiff(col_set,gen_vector)
    }
  }
  
  return( output )
}


## Function to predict using GP model
# Params:
# - surrogate_model: model fitted with fn_fit function
# - ds: dataset (tibble)
fn_predict_gp <- function(surrogate_model,ds){
  pred <- predict.GP(surrogate_model, xnew = ds)
  
  return(list(
    #object = pred,
    pred = pred,
    mu = pred$Y_hat,
    sigma = sqrt(pred$MSE)
  ))
}
## Function to predict using GP model (MLEGP Library)
# Params:
# - surrogate_model: model fitted with fn_fit function
# - ds: dataset (tibble)
fn_predict_mlegp <- function(surrogate_model,ds){
  pred <- predict.gp(surrogate_model, newData = ds, se.fit = T)
  
  return(list(
    #object = pred,
    pred = pred,
    mu = as.vector(pred$fit),
    sigma = as.vector(pred$se.fit)
  ))
}
## Function to predict using Random Forest model
# Params:
# - surrogate_model: model fitted with fn_fit function
# - ds: dataset (tibble)
fn_predict_rf <- function(surrogate_model,ds){
  pred <- predict(surrogate_model, newdata = ds, predict.all = T)
  
  return(list(
    #object = pred,
    pred = pred$individual,
    mu = pred$aggregate, #matrix(pred$aggregate, ncol = 1),
    sigma = apply(pred$individual, 1, sd), #matrix(apply(pred$individual, 1, sd), ncol = 1) ,
    inbag = surrogate_model$inbag
  ))
}

## Function to predict using Random Forest model (ranger package)
# Params:
# - surrogate_model: model fitted with fn_fit function
# - ds: dataset (tibble)
fn_predict_rf_ranger <- function(surrogate_model,ds){
  pred <- predict(surrogate_model, data = ds, predict.all = T)

  inbag.counts <- simplify2array(surrogate_model$inbag.counts)
  if (is.vector(inbag.counts)) {
    inbag.counts <- t(as.matrix(inbag.counts))
  }
  inbag.counts <- inbag.counts[rowSums(inbag.counts == 0) > 0, , drop = FALSE]
  
  return(list(
    #object = pred,
    pred = pred$predictions,
    mu = apply(pred$predictions, 1, mean),
    sigma = apply(pred$predictions, 1, sd),
    inbag = inbag.counts
    #sigma.IJ = rInfJack(pred$predictions, inbag.counts, calibrate = TRUE)
  ))
}

## Function to predict using a model
# Params:
# - type: mode type - predict function
# - surrogate_model: model fitted with fn_fit function (list of surrogates)
# - ds: dataset (tibble)
fn_predict <- function(type, surrogate_model, ds, param_def = NULL, is_normalization_required = F, ...){
  # ds_colnames <- colnames(ds)
  ## Apply filtering in accordance to the enabled feature
  if(!is.null(param_def) && c("enabled") %in% names(param_def)){
    enabled_idx <- which(param_def[,"enabled"] == T)
    ds <- ds[,enabled_idx, drop = F]
    
    # if(is.null(dim(ds))){
    #   ds <- matrix(ds, nrow = 1)
    #   colnames(ds) <- ds_colnames[enabled_idx]
    # }
  }
  
  if(is_normalization_required && !is.null(param_def)){
    ds <- fn_normalize_min_max(ds,param_def)
  }
  
  ## Surrogate model object contains list of lists [surrogate, indices of subspace columns]
  list_predictions <- lapply(surrogate_model, function(l_model_subspace){ 
    type(l_model_subspace[[1]], ds[,l_model_subspace[[2]], drop = F], ...)
  } )
  output_headers <- names(list_predictions[[1]])
  output <- list()
  for(h in output_headers){
    output[[h]] <- lapply(list_predictions, '[[', h)
  }
  
  return( output )
}


## Function that perform the bayesian optimisation
# Params:
# - termination: function that return logical value whether the process need to be terminated (TRUE) or not (FALSE), based on input args "iteration_number" and "param_space" 
fn_bayes_optimisiation <- function(
  param_init, ## initial paramater set in case it is loaded from file
  param_def, #= param_space$definition <- for simulation_fn
  param_addons = list(),
  simulation_fn,
  termination_fn = NULL, 
  verbose_output_fn = NULL,
  save_object_fn = NULL,
  target = "inadequacy", 
  surrogate_fit_fn = "fn_fit_rf",
  surrogate_predict_fn = "fn_predict_rf",
  surrogate_utility_fn = "fn_utility_ei",
  init_sampling_fn = "fn_sampling_lhs",
  sampling_fn = "fn_sampling_lhs",
  acquisition_fn = "fn_optimal_acquisition",
  selection_fn = "fn_pull_optimal_selection",
  inner_optimisation_fn = "fn_optimisation_quasi_newthon",
  init_sample_size = 10,
  sample_size = 100,
  potential_size = 1,
  initialize_only = F,
  store_iteratively = F,
  #objective_fn = min <- selecting best performer (!must be a function and not name)
  #randomized_selection = F <- selecting best performer
  #selection_size = 0 (all) <- selecting best performer
  ...){
  
  ## Get global settings object
  if(!hasName(shared.env$settings,"RETRAIN_AFTER")) {
    shared.env$settings$RETRAIN_AFTER = 1
  }
  settings_obj <- shared.env$settings
  
  ## Compile all functions
  tryCatch({
    fn_compulation <- c("surrogate_fit_fn","surrogate_predict_fn","surrogate_utility_fn","init_sampling_fn","sampling_fn","acquisition_fn","selection_fn","termination_fn","inner_optimisation_fn")
    
    for(i in 1:length(fn_compulation)){
      assign(fn_compulation[i], match.fun( get(eval(fn_compulation[i])) ))
    }
    
    if(!is.function(simulation_fn)){
      simulation_fn <- match.fun( get(eval(simulation_fn)) )
    }
  }, error = function(err){
    stop(err)
  })
  
  ## Check if terminal conditions are set up
  if(is.null(termination_fn)) stop("Termination function is required! It must return logical value T (terminate)/F (continiue). At input get the iteration number, params and inadequacy through iterations")
  
  iteration_no <- 0
  
  ## Assemble the param space
  param_space <- list()
  param_space$space <- param_init
  param_space$definition <- param_def
  param_space <- modifyList(param_space, param_addons)
  
  if(nrow(param_space$space) == 0){
    ## Generate initial set of params to be simulated! (preferable 10 instances)
    param_space$space <- rbind(param_space$space, do.call(fn_acquisition, c(list(type = init_sampling_fn, param_space = param_space), fn_resolve_fn_params(init_sampling_fn, .omit = c("type","param_space"), sample_size = init_sample_size, omit = c(target), ... )) )) 
    #param_space$space <- bind_rows(param_space$space, do.call(fn_acquisition, c(list(type = fn_acquisition_random_share, param_space = param_space), fn_resolve_fn_params(fn_acquisition_random_share, .omit = c("type","param_space"), sample_size = init_sample_size, omit = c(target), ... )) )) 
    if(!is.null(verbose_output_fn)) verbose_output_fn("Initial sample set acquired.", count = init_sample_size)
    
    ## Simulate initial set od params
    simulation_outcome <- do.call(apply, c(list(X = param_space$space, MARGIN = 1, FUN = simulation_fn), fn_resolve_fn_params(simulation_fn, .omit = 1, param_def = param_space$definition, ... )) )
    param_space$space <- cbind(param_space$space,simulation_outcome)
    colnames(param_space$space)[ncol(param_space$space)] <- target
    
    if(!is.null(verbose_output_fn)) verbose_output_fn("Initial sample set simulated.")
    if(!is.null(save_object_fn)) save_object_fn(param_space, "param_space", stage = "initial")
  }
  
  if(initialize_only) {
    return(NULL)
  }
  
  termination_args <- fn_resolve_fn_params(termination_fn, .omit = c("iteration","space"), ...)
  ## Perform optimisation
  while( ! do.call(termination_fn, c(list(iteration = iteration_no, space = param_space), termination_args)) ){
    # Increase iteration number
    iteration_no <- iteration_no + 1
    iteration_space <- list()
    
    tryCatch({
      # 1. get the best performer and save its parameters
      iteration_space$optimal <- do.call(fn_pull_optimal_samples, c(list(space = param_space$space, selection_fn = selection_fn, target = target), fn_resolve_fn_params(selection_fn,.omit = c("space","selection_fn","target"), ... ) ) )
      if(!is.null(verbose_output_fn)) verbose_output_fn("Retrieved current most optimal sample.", count = nrow(iteration_space$optimal$space))
      
      # 2. fit surrogate model
      if( (iteration_no == 1) || (iteration_no %% settings_obj$RETRAIN_AFTER == 0) ){
        iteration_space$surrogate <- list()
        iteration_space$surrogate$model <- do.call(fn_fit, c(list(type = surrogate_fit_fn, ds = param_space$space, target = target, param_def = param_space$definition), 
                                                             fn_resolve_fn_params(fn_fit, .omit = c("type","ds","target","param_def","..."), ...),
                                                             fn_resolve_fn_params(surrogate_fit_fn, .omit = c("type","ds","target","param_def","method_params"), ...) ) )
        param_space$surrogate <- iteration_space$surrogate
        if(!is.null(verbose_output_fn)) verbose_output_fn("Fitted surrogate model(s).", training_set_size = nrow(param_space$space))
      } else {
        iteration_space$surrogate <- param_space$surrogate
        if(!is.null(verbose_output_fn)) verbose_output_fn("Loaded previously trained surrogate model(s).", training_set_size = nrow(param_space$space))
      }
      
      # 3. optimise acquisition utility
      iteration_space$potential <- do.call(fn_acquisition, 
                                                 c(list(type = acquisition_fn, param_space = param_space), 
                                                   fn_resolve_fn_params(acquisition_fn, .omit = c("type","param_space"), iter_space = iteration_space, sample_size = sample_size, omit = c(target), sampling_fn = sampling_fn, surrogate_predict_fn = surrogate_predict_fn, surrogate_utility_fn = surrogate_utility_fn, inner_optimisation_fn = inner_optimisation_fn, ... ),
                                                   fn_resolve_fn_params(inner_optimisation_fn, .omit = c("sampling_fn","surrogate_predict_fn", "surrogate_model", "surrogate_utility_fn", "iteration_space", "param_range","param_space","sample_size","omit"), target = target, ...) ) ) 
      if(!is.null(verbose_output_fn)) verbose_output_fn("New optimal sample set acquired.")
      
      # 4. simulate results of the picked-up instances
      simulation_outcome <- do.call(apply, c(list(X = iteration_space$potential$space, MARGIN = 1, FUN = simulation_fn), fn_resolve_fn_params(simulation_fn, .omit = 1, param_def = param_space$definition, ... )) )
      iteration_space$potential$space <- cbind(iteration_space$potential$space,simulation_outcome)
      colnames(iteration_space$potential$space)[ncol(iteration_space$potential$space)] <- target
      #iteration_space$potential$space[,target] <- simulation_outcome #<- iteration_space$potential$space %>% mutate( !! rlang::sym(target) := simulation_outcome)
      if(!is.null(verbose_output_fn)) verbose_output_fn("Simulation of the most promissing samples is completed.")
      
      if(hasName(iteration_space$potential, name = "complementary_utility_fn") && length(iteration_space$potential$complementary_utility_fn) > 0 ){
        # 4b. optionally simulate results of the picked-up instances by complementary acquisition functions
        
        compl_space <- bind_rows(lapply(iteration_space$potential$complementary_utility_fn, function(fn_header){
          best_val_idx <- which(iteration_space$potential$pop[,fn_header] == max(iteration_space$potential$pop[,fn_header]))[1]
          eval_set <- as.data.frame(iteration_space$potential$pop[best_val_idx, colnames(iteration_space$potential$space) ])
          simulation_outcome <- do.call(apply, c(list(X = eval_set, MARGIN = 1, FUN = simulation_fn), fn_resolve_fn_params(simulation_fn, .omit = 1, param_def = param_space$definition, ... )) )
          
          eval_set["select_idx"] <- best_val_idx
          eval_set["acq"] <- fn_header
          eval_set["utility"] <- as.numeric(iteration_space$potential$pop[best_val_idx, fn_header ])
          eval_set["simulation"] <- simulation_outcome
          eval_set["iteration"] <- iteration_no
          rownames(eval_set) <- NULL
          eval_set
        }))
        
        if( hasName(param_space, "complementary_space")){
          param_space$complementary_space <- bind_rows(param_space$complementary_space, compl_space)
        } else {
          param_space$complementary_space <- compl_space
        }
        
        if(!is.null(verbose_output_fn)) verbose_output_fn("Simulation of the most promissing samples from complementary acquisition functions is completed.")
      }
      
      # 5. add simulated results to the base set
      param_space$space <- bind_rows( param_space$space, as_tibble(iteration_space$potential$space))
      if(!is.null(verbose_output_fn)) verbose_output_fn("Simulated results are added to the global set.")
      
      ## Store required objects
      if(!is.null(save_object_fn) && store_iteratively) save_object_fn(iteration_space, "param_space", stage = "optimisation", iteration = iteration_no)
      if(!is.null(save_object_fn)) save_object_fn(param_space, "output_object", stage = "partial")
      if(exists("shared.env") && exists("param_space", envir = shared.env) ) shared.env$param_space = param_space
    },
    error = function(err){

      if(!is.null(save_object_fn)) save_object_fn(iteration_space, "param_space", stage = "failed_optimisation_iter", iteration = iteration_no)
      if(!is.null(verbose_output_fn)) verbose_output_fn("ERROR: Iteration failed! ", iteration = iteration_no, ex = err)
      throw(err)
    })
  }
  
  return( param_space )
}

## Function: assemble prediction traits that helps further applying acquisition and utility functions
## Params:
## - prediction_obj: object containning predictions return from surrogate_model_fn
fn_assemble_prediction_traits <- function(prediction_obj){
  prediction_traits <- list()
  prediction_traits$mu <- matrix(apply(Reduce(cbind, lapply(prediction_obj$mu, function(x) { matrix(x, ncol = 1) })), 1, mean, na.rm = T), ncol = 1) #mean(unlist(prediction_obj$mu), na.rm = T)
  prediction_traits$sigma <- matrix(apply(Reduce(cbind, lapply(prediction_obj$sigma, function(x) { matrix(x, ncol = 1) })), 1, mean, na.rm = T), ncol = 1) #mean(unlist(prediction_obj$sigma), na.rm = T)
  prediction_traits$values <- Reduce(cbind, prediction_obj$pred)
  prediction_traits$custom <- prediction_obj[ ! names(prediction_obj) %in% c("mu","sigma","pred")]
  
  return( prediction_traits )
}
fn_assemble_prediction_traits_single <- function(prediction_obj){
  prediction_traits <- list()
  prediction_traits$mu <- prediction_obj$mu
  prediction_traits$sigma <- prediction_obj$sigma
  prediction_traits$values <- prediction_obj$pred
  
  prediction_traits$custom <- prediction_obj[ ! names(prediction_obj) %in% c("mu","sigma","pred")]
  
  return( prediction_traits )
}

## Function: perform selection of optimal samples
## Params:
## - space: param space to be selected from
## - target: variable name to be arranged with
## - objective_fn: function that will take a vector and return subset of that vector that optimises certain objective. Default it minimum
## - selection_fn: function that will perform selection after optimal have been selected. This primarily refers to selector that will selected either all or single sample, with or without randomization
fn_pull_optimal_samples <- function(space, target, objective_fn = min, selection_fn = NULL, ...){
  best_performer <- list()
  best_performer$idx <- which(space[,target] == objective_fn(space[,target]))
  best_performer$space <- space[best_performer$idx,]
  
  if(!is.null(selection_fn)) {
    selected_space <- selection_fn(best_performer$space, ...)
    best_performer$space <- selected_space$space
    best_performer$idx <- best_performer$idx[selected_space$idx]
  }
  best_performer$value <- objective_fn(best_performer$space[,target])
  
  return( best_performer )
}

## Function: perform selection of one or many samples form optimally pulled
## Params:
## - space: optimal space selected
## - random: flag indicating randomisation before selection
## - selection_size: number of element to return from the beginning
fn_pull_optimal_selection <- function(space, randomized_selection = F, selection_size = 0){
  space_nrow <- nrow(space)
  idx <- space_nrow
  if(randomized_selection) {
    idx <- sample(1:space_nrow,space_nrow)
    space <- space[idx,]
  }
  
  if(selection_size <= 0 || selection_size >= space_nrow) {
    return( list(space = space, idx = idx ) )
  }
  
  return( list( space = space[1:selection_size,], idx = idx[1:selection_size] )  )
}

## Function: helper function that resolve parameters of a function into a list of values, based on current environment
fn_resolve_fn_params <- function(fn, .omit = c(), ...){
  ## Get arguments list of the provided function
  args_list <- rlang::fn_fmls(fn = fn)
  
  ## Get all parameters provided in this function call
  params_list <- list(...)#params_list[3:length(params_list)] 
  params_names <- names(params_list)
  
  ## Omit args that are requested not to be set
  if(is.numeric(.omit)) { .omit <- names(args_list)[.omit] }
  args_list <- args_list[ setdiff(names(args_list),.omit) ]
  args_names <- names(args_list)
  
  ## If no other args needed, return empty
  if(length(args_list) == 0){
    return( args_list )
  }
  
  ## Resolve parameter values
  for(i in 1:length(args_list)){
    
    if(args_names[i] %in% params_names ) {
      args_list[i] = params_list[args_names[i]]
    }
  }
  return( args_list )
  
  #resolved_values <- lapply(as.list(names(args_list)), function(arg){ ifelse( (arg %in% names(params_list) ),params_list[[arg]], args_list[[arg]] )  }) 
  #names(resolved_values) <- names(args_list)
  #return( resolved_values )
}

fn_termination_max_iterations <- function(iteration, space, max_iterations){
  return( iteration > max_iterations )
}


## --------------------
# Compute variance of estimate from a ranger model
# 
# Computes variances for a prediction from a ranger model, using the infinitesimal jackknife procedure
# 
# This function is a ranger-adaptation of the package \pkg{randomForestCI} of Wager et al. (2014). Their original can be found on github: \url{ https://github.com/swager/randomForestCI/}. 
#
# @param pred A nrow(newdata) by no. of trees matrix which contains numeric predictions
#        from a random forest trained with trees grown on bootstrap samples of the training data
# @param inbag A number of obs. in the training data by no. of trees matrix giving the
#        number of times the ith observation in the training data appeared in the bootstrap sample for the jth tree.
# @param calibrate whether to apply calibration to mitigate Monte Carlo noise
#        warning: if calibrate = FALSE, some variance estimates may be negative
#                 due to Monte Carlo effects if the number of trees in rf is too small
# @param used.trees set of trees to use for variance estimation; uses all tress if NULL
#
# @return A two-column matrix is returned, with predictions in the first column and estimates of prediction variance in the second. 
# @author Stefan Wager
rInfJack = function(pred, inbag, calibrate = TRUE, used.trees = NULL) {
  # original: https://github.com/swager/randomForestCI/blob/master/R/infinitesimalJackknife.R
  
  if (is.null(used.trees)) {
    used.trees = 1:ncol(inbag)
  }
  pred = pred[, used.trees, drop=FALSE]
  
  # Check if sampling without replacement
  no.replacement = (max(inbag) == 1)
  
  # Extract tree-wise predictions and variable counts from random forest
  B = length(used.trees)
  n = nrow(inbag)
  s = sum(inbag) / ncol(inbag)
  
  y.hat = rowMeans(pred)
  pred.centered = pred - rowMeans(pred)
  
  N = Matrix::Matrix(inbag[, used.trees], sparse = TRUE)
  N.avg = Matrix::rowMeans(N)
  
  # Compute raw infinitesimal jackknife
  if (as.numeric(B)^2 > as.numeric(n) * as.numeric(nrow(pred))) {
    
    C = Matrix::tcrossprod(N, pred.centered) -
      Matrix::Matrix(N.avg, nrow(N), 1) %*%
      Matrix::Matrix(rowSums(pred.centered), 1, nrow(pred.centered))
    raw.IJ = Matrix::colSums(C^2) / B^2
    
  } else {
    # Faster implementation when n is large. Uses the fact that
    # colSums((A - B)^2) = T1 - 2 * T2 + T3,
    # where T1 = diag(A'A), T2 = diag(B'A), and T3 = diag(B'B)
    
    NTN = Matrix::crossprod(N, N)
    NTNPT_T = Matrix::tcrossprod(pred.centered, NTN)
    T1 = Matrix::rowSums(pred.centered * NTNPT_T)
    
    RS = rowSums(pred.centered)
    NbarTN = Matrix::crossprod(N.avg, N)
    T2 = RS * Matrix::tcrossprod(NbarTN, pred.centered)
    
    T3 = sum(N.avg^2) * RS^2
    raw.IJ = as.numeric(T1 - 2 * T2 + T3) / B^2
    
  }
  
  # Apply Monte Carlo bias correction
  N.var = mean(Matrix::rowMeans(N^2) - Matrix::rowMeans(N)^2)
  boot.var = rowSums(pred.centered^2) / B
  bias.correction = n * N.var * boot.var / B
  vars = raw.IJ - bias.correction
  
  # Finite sample correction
  if (no.replacement) {
    variance.inflation = 1 / (1 - mean(inbag))^2
    vars = variance.inflation * vars
  }
  
  results = data.frame(y.hat=y.hat, var.hat=vars)
  
  if (isTRUE(calibrate) && nrow(results) <= 20) {
    calibrate = FALSE
    warning("Sample size <=20, no calibration performed.")
  }
  
  # If appropriate, calibrate variance estimates; this step in particular
  # ensures that all variance estimates wil be positive.
  
  if (calibrate) {
    # Compute variance estimates using half the trees
    calibration.ratio = 2
    n.sample = ceiling(B / calibration.ratio)
    results.ss = rInfJack(pred, inbag, calibrate = FALSE, used.trees = sample(used.trees, n.sample))
    
    # Use this second set of variance estimates to estimate scale of Monte Carlo noise
    sigma2.ss = mean((results.ss$var.hat - results$var.hat)^2)
    delta = n.sample / B
    sigma2 = (delta^2 + (1 - delta)^2) / (2 * (1 - delta)^2) * sigma2.ss
    
    # Use Monte Carlo noise scale estimate for empirical Bayes calibration
    results = tryCatch(
      expr = {
        vars.calibrated = calibrateEB(vars, sigma2)
        results$var.hat = vars.calibrated
        results
      }, 
      error = function(e) {
        warning(sprintf("Calibration failed with error:\n%sFalling back to non-calibrated variance estimates.", e))
        results = rInfJack(pred, inbag, calibrate = FALSE, used.trees = used.trees)
        return(results)
      }
    )
  }
  
  return(results)
}

# Fit an empirical Bayes prior in the hierarchical model
#     mu ~ G, X ~ N(mu, sigma^2)
#
# @param X a vector of observations
# @param sigma noise estimate
# @param p tuning parameter -- number of parameters used to fit G
# @param nbin tuning parameter -- number of bins used for discrete approximation
# @param unif.fraction tuning parameter -- fraction of G modeled as "slab"
#
# @return posterior density estimate g
#
# @section References:
# For more details about "g-estimation", see: B Efron. Two modeling strategies for
# empirical Bayes estimation. Stat. Sci., 29: 285-301, 2014.
# @author Stefan Wager
gfit = function(X, sigma, p = 2, nbin = 1000, unif.fraction = 0.1) {
  
  xvals = seq(min(min(X) - 2 * sd(X), 0), max(max(X) + 2 * sd(X), sd(X)), length.out = nbin)
  binw = xvals[2] - xvals[1]
  
  zero.idx = max(which(xvals <= 0))
  noise.kernel = dnorm(xvals / sigma) * binw / sigma
  
  if (zero.idx > 1) {
    noise.rotate = noise.kernel[c(zero.idx:length(xvals), 1:(zero.idx - 1))]
  } else {
    noise.rotate = noise.kernel
  }
  
  XX = sapply(1:p, function(j) xvals^j * as.numeric(xvals >= 0))
  neg.loglik = function(eta) {
    g.eta.raw = exp(XX %*% eta) * as.numeric(xvals >= 0)
    if ((sum(g.eta.raw) == Inf) | (sum(g.eta.raw) <= 100 * .Machine$double.eps)) {
      return (1000 * (length(X) + sum(eta^2)))
    }
    g.eta.main = g.eta.raw / sum(g.eta.raw)
    g.eta = (1 - unif.fraction) * g.eta.main +
      unif.fraction * as.numeric(xvals >= 0) / sum(xvals >= 0)
    f.eta = convolve(g.eta, noise.rotate)
    sum(approx(xvals, -log(pmax(f.eta, 0.0000001)), X)$y)
  }
  
  eta.hat = nlm(neg.loglik, rep(-1, p))$estimate
  g.eta.raw = exp(XX %*% eta.hat) * as.numeric(xvals >= 0)
  g.eta.main = g.eta.raw / sum(g.eta.raw)
  g.eta = (1 - unif.fraction) * g.eta.main +
    unif.fraction * as.numeric(xvals >= 0) / sum(xvals >= 0)
  
  return(data.frame(x=xvals, g=g.eta))
}

# Bayes posterior estimation with Gaussian noise
#
# @param x0 an obsevation
# @param g.est a prior density, as returned by gfit
# @param sigma noise estimate
#
# @return posterior estimate E[mu | x0]
# @author Stefan Wager
gbayes = function(x0, g.est, sigma) {
  Kx = dnorm((g.est$x - x0) / sigma)
  post = Kx * g.est$g
  post = post / sum(post)
  sum(post * g.est$x)
}

# Empirical Bayes calibration of noisy variance estimates
#
# @param vars list of variance estimates
# @param sigma2 estimate of the Monte Carlo noise in vars
#
# @return calibrated variance estimates
# @author Stefan Wager
calibrateEB = function(vars, sigma2) {
  
  if(sigma2 <= 0 | min(vars) == max(vars)) {
    return(pmax(vars, 0))
  }
  
  sigma = sqrt(sigma2)
  eb.prior = gfit(vars, sigma)
  
  if (length(vars >= 200)) {
    # If there are many test points, use interpolation to speed up computations
    calib.x = unique(quantile(vars, q = seq(0, 1, by = 0.02)))
    calib.y = sapply(calib.x, function(xx) gbayes(xx, eb.prior, sigma))
    calib.all = approx(x=calib.x, y=calib.y, xout=vars)$y
  } else {
    calib.all = sapply(vars, function(xx) gbayes(xx, eb.prior, sigma))
  }
  return(calib.all)
}





