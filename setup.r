library(renv)
library(psych)
library(ggplot2)
library(tidyLPA)
library(MplusAutomation)

tab_class <- function(x){
  tab <- x$fits[, c("Classes", "LogLik", "BIC", "Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_p")]
  names(tab) <- c("Classes", "LL", "BIC", "Entropy", "$p_{min}$", "$p_{max}$", "$n_{min}$", "$n_{max}$", "$p_{BLRT}$")
  papaja::apa_table(tab, escape = FALSE)
}

plot_bic <- function(x){
  df_plot <- x$fits
  df_plot$Model <- factor(df_plot$Model, levels = c(2, 4), labels = c("Fixed covariance", "Free covariance"))
  ggplot(df_plot, aes(x = Classes, y = BIC, colour = Model, group = Model)) + geom_point() + geom_path()
}
