df_pc <- df[df$category == "Line", c("length", "width")]
pc <- principal(df_pc, nfactors = 2)
plot(pc$scores[,1], pc$scores[,2])

df_pc <- df[df$category == "Line", c("length")]
pc <- principal(df_pc, nfactors = 1)
hist(pc$scores[,1], breaks = 100)


df_pc <- df[df$category == "Film", c("length", "width")]
pc <- principal(df_pc, nfactors = 2)
plot(pc$scores[,1], pc$scores[,2])

df_pc <- df[df$category %in% c("Fragment", "Film"), c("length", "width")]
pc <- principal(df_pc, nfactors = 2)
plot(pc$scores[,1], pc$scores[,2])
plot(density(pc$scores[,1]))
hist(pc$scores[,1], breaks = 100)
min(pc$scores[,1])

plot(density(pc$scores[,2]))


