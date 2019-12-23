install.packages("psych")
install.packages("doParallel")
install.packages("tidyLPA")
library(psych)
library(doParallel)
library(parallel)

n = 200
K = 3*45

create_clusters <- function(no_cores = 45){
  cl <- makeCluster(no_cores)
  clusterEvalQ(cl, library(psych))
  clusterEvalQ(cl, library(tidyLPA))
  registerDoParallel(cl)
  cl
}
cl <- create_clusters()

seed <- 6883

sim_results <- foreach(it = 1:K, .combine = rbind)  %dopar% {
  true_class <- factor(sample(c("fragment", "flake"), n, replace = TRUE))
  
  h <- rnorm(n)
  w <- vector("numeric", n)
  w[true_class == "fragment"] <- rnorm(sum(true_class == "fragment"), mean = h[true_class == "fragment"], sd = .3)
  w[true_class == "flake"] <- rnorm(sum(true_class == "flake"), mean = (h[true_class == "flake"]/8), sd = .1)
  
  res <- estimate_profiles(cbind(h, w), 1:4, variances = "varying", covariances = "varying")
  tmp <- compare_solutions(res)
  tab <- table(res$model_6_class_2$dff$Class, true_class)
  if(!is.na(tmp$fits$LogLik)){
    out <- c(tmp$fits$LogLik, tmp$fits$BIC, tmp$fits$Entropy, sum(apply(tab, 1, max)/n))
  } else {
    out <- rep(NA, 13)
  }
  
  pcdat <- principal(cbind(h, w), nfactors = 2)$scores
  
  res <- estimate_profiles(pcdat, 1:4, variances = "varying", covariances = "varying")
  tmp <- compare_solutions(res)
  tab <- table(res$model_6_class_2$dff$Class, true_class)
  if(!is.na(tmp$fits$LogLik)){
    c(out, tmp$fits$LogLik, tmp$fits$BIC, tmp$fits$Entropy, sum(apply(tab, 1, max)/n))
  } else {
    out <- c(out, rep(NA, 13))
  }
  
}

#saveRDS(sim_results, "sim_results.RData")
sim_results <- readRDS("sim_results.RData")
sim_results <- data.frame(sim_results)
names(sim_results) <- c(paste0("LL", 1:4), paste0("BIC", 1:4), paste0("Ent", 1:4), "correct", 
                        paste0("LLpc", 1:4), paste0("BICpc", 1:4), paste0("Entpc", 1:4), "correctpc")

# BIC found best
sum(apply(sim_results[, grep("^BIC\\d", names(sim_results))], 1, which.min) == 2)/K
sum(apply(sim_results[, grep("^BICpc\\d", names(sim_results))], 1, which.min) == 2)/K

# Entropy higher for PC
sum(sim_results$Entpc2 > sim_results$Ent2)
sum(sim_results$Ent2 > sim_results$Entpc2)

# More correct
sum(sim_results$correctpc > sim_results$correct)-sum(sim_results$correctpc < sim_results$correct)




library(ggplot2)

ggplot(data.frame(x = w, y = h, col = true_class), aes(x=x, y=y, colour = col)) + geom_point()

