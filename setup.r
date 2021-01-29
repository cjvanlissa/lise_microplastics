library(renv)
library(psych)
library(ggplot2)
library(tidyLPA)
library(MplusAutomation)

tab_class <- function(x){
  tab <- x$fits[, c("Model", "Classes", "LogLik", "BIC", "Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_p")]
  tab$Model <- c("Free M, S2", "Free M, S2, r")[match(tab$Model, c(4, 6))]
  names(tab) <- c("Model", "Classes", "LL", "BIC", "Entropy", "$p_{min}$", "$p_{max}$", "$n_{min}$", "$n_{max}$", "$p_{BLRT}$")
  tab
}

plot_bic <- function(x){
  df_plot <- x$fits
  df_plot$Model <- factor(df_plot$Model, levels = c(2, 4), labels = c("Fixed covariance", "Free covariance"))
  ggplot(df_plot, aes(x = Classes, y = BIC, colour = Model, group = Model)) + geom_point() + geom_path()
}

std_est <- function(x){
  #x <- res_line$model_2_class_3
  res <- get_estimates(x)
  std <- x$model$parameters$stdyx.standardized
  res$Estimate <- std$est
  res$se <- std$se
  res$se[res$se == 0] <- NA
  res$p <- std$pval
  res$p[res$p == 999] <- NA
  res
}

est_cor <- function(x){
  res <- get_estimates(x)
  std <- x$model$parameters$stdyx.standardized
  which_cov <- res$Category == "Covariances"
  res$Estimate[which_cov] <- std$est[which_cov]
  res$se[which_cov] <- std$se[which_cov]
  res$p[which_cov] <- std$pval[which_cov]
  res$Category[which_cov] <- "Correlation"
  res
}

report <- function(x, digits = 2, equals = TRUE){
  equal_sign <- "= "
  if(x%%1==0){
    outstring <- formatC(x, digits = 0, format = "f")
  } else {
    if(abs(x) <  10^-digits){
      equal_sign <- "< "
      outstring <- 10^-digits
    } else {
      outstring <- formatC(x, digits = digits, format = "f")
    }
  }
  ifelse(equals, paste0(equal_sign, outstring), outstring)
}

confint <- function(x, digits = 2, se = NULL, lb = NULL, ub = NULL, ci = 95){
  if(!is.null(se) & !is.null(lb) & !is.null(ub)) {
    message("Both se and lb/ub provided. Used lb/ub to construct confidence interval.", call. = FALSE)
    se <- NULL
  }
  if(!is.null(lb) & !is.null(ub)){
    paste0("[", formatC(lb, digits = digits, format = "f"), ", ", formatC(ub, digits = digits, format = "f"), "]")
  } else {
    if(!(ci>0 & ci < 100)) stop("Argument 'ci' must have a value between 0-100.", call. = FALSE)
    bound <- qnorm((1-(ci/100))/2)
    paste0("[", formatC(x+(bound*se), digits = digits, format = "f"), ", ", formatC(x-(bound*se), digits = digits, format = "f"), "]")
  }
}
