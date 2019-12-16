library(psych)
seed <- 57432
n = 200
K = 10

sim_results <- replicate(K, {
  true_class <- factor(sample(c("fragment", "flake"), n, replace = TRUE))
  
  h <- rnorm(n)
  w <- vector("numeric", n)
  w[true_class == "fragment"] <- rnorm(sum(true_class == "fragment"), mean = h[true_class == "fragment"], sd = .3)
  w[true_class == "flake"] <- rnorm(sum(true_class == "flake"), mean = (h[true_class == "flake"]/8), sd = .1)
  
  res <- estimate_profiles(cbind(h, w), 1:4, variances = "varying", covariances = "varying")
  tmp <- compare_solutions(res)
  tab <- table(res$model_6_class_2$dff$Class, true_class)
  out <- c(tmp$fits$LogLik, tmp$fits$BIC, tmp$fits$Entropy, sum(apply(tab, 1, max)/n))
  
  pcdat <- principal(cbind(h, w), nfactors = 2)$scores
  
  res <- estimate_profiles(pcdat, 1:4, variances = "varying", covariances = "varying")
  tmp <- compare_solutions(res)
  tab <- table(res$model_6_class_2$dff$Class, true_class)
  
  c(out, tmp$fits$LogLik, tmp$fits$BIC, tmp$fits$Entropy, sum(apply(tab, 1, max)/n))
})
sim_results <- data.frame(t(sim_results))
names(sim_results) <- c(paste0("LL", 1:4), paste0("BIC", 1:4), paste0("Ent", 1:4), "correct", 
                        paste0("LLpc", 1:4), paste0("BICpc", 1:4), paste0("Entpc", 1:4), "correctpc")

# BIC found best
sum(apply(sim_results[, grep("^BIC\\d", names(sim_results))], 1, which.min) == 2)/K
sum(apply(sim_results[, grep("^BICpc\\d", names(sim_results))], 1, which.min) == 2)/K

# Entropy higher for PC
sum(sim_results$Entpc2 > sim_results$Ent2)

# More correct
sum(sim_results$correctpc > sim_results$correct)-sum(sim_results$correctpc < sim_results$correct)




library(ggplot2)

ggplot(data.frame(x = w, y = h, col = true_class), aes(x=x, y=y, colour = col)) + geom_point()

