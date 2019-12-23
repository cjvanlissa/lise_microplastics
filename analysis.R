analyzedat <- df[, c("length", "width", "Film", "Line")]

# First, analyze separately for film, line, and others, to determine number
# of components





plot_profiles(res_other[[1]])

class(res_other[[1]]$dff)

tidyLPA:::estimate_profiles_mplus2()

# Analyze lines ------------------------------------------------------------

analyze_line <- analyzedat[analyzedat$Line == 1, c("length")]

if(!file.exists("res_line.RData")){
  res_line <- estimate_profiles(analyze_line, 1:5, variances = c("varying"), covariances = c("zero"), package = "MplusAutomation")
  saveRDS(res_line, "res_line.RData")
} else {
  res_line <- readRDS("res_line.RData")
}
plot_density(res_line)

# Two-class solution looks best

plot_bivariate(res_line$model_4_class_1)
res_line[[2]]$model$errors
model <- c("[length];",
           "[width];",
           "length;",
           "width;",
           "length WITH width;"
           )


results <- lapply(1:2, function(class){
  ob <- mplusObject(TITLE = paste0(class, " classses"),
              VARIABLE = paste0("CATEGORICAL ARE Film Line;\nCLASSES = c1(", class, ");\n"),
              ANALYSIS = "TYPE = mixture;",
              MODEL = do.call(c, lapply(1:class, function(i){
                c(paste0("%c1#", i, "%"), model)})),
              OUTPUT = "TECH14 standardized;",
              SAVEDATA = paste0("FILE IS class", class, ".dat;\nSAVE = cprobabilities;"),
              rdata = analyzedat)
  mplusModeler(ob, modelout = paste0(class, "class.inp"), run = 1L)
})

res <- mplusModeler(Args[[1]])


# Make a plot -------------------------------------------------------------
tidyprofile <- estimate_profiles(analyzedat[, c(1, 2)], 2, variances = "varying", covariances = "varying") # Just as a template
# Replace data
tidyprofile$model_6_class_2$dff[, c("CPROB1", "CPROB2", "Class")] <- results[[2]]$results$savedata[, c("CPROB1", "CPROB2", "C1")]
# Plot
plot_bivariate(tidyprofile$model_6_class_2)