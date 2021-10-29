#use df1 (datafile with categories including film & foam (rest is the same))
df1 <- read.csv("df1.csv")

#frequencies category (table S3)
table(df1$poly_type,df1$category, useNA = "always")

install.packages('epiDisplay')
library(epiDisplay)
tab1(df1$category, sort.group = "decreasing", cum.percent = TRUE)


nrow(df1[df1$length <= 5, ])
nrow(df1[df1$length >5, ])
nrow(df1)

#distributions

#length distribution
p <- ggplot(df, aes(x=log(length))) + theme_bw() + geom_density(aes(y = ..density..), alpha = 10, col = "red", size = 1) + geom_density(aes(color = category), alpha = 0.1) + labs(x = "log (L)") + ggtitle("Length distribution for Film, Fragment, Line")+ theme_bw() +
  theme(plot.title=element_text(size = rel(1), face = "bold"),
        legend.position = "bottom",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "gray90"),
        axis.title = element_text(face = "bold", size = 13)) 

#length distribution for microplastics only (to compare with K&K)
df_plot5cat <- df[df$length <5, ]
ggplot(df_plot5cat, aes(x=log(length))) + theme_bw() + geom_density(aes(y = ..density..), alpha = 10, col = "red", size = 1) + geom_density(aes(color = category), alpha = 0.1) + labs(x = "log (L)") + ggtitle("Length distribution for Film, Fragment, Line < 5mm")+ theme_bw() +
  theme(plot.title=element_text(size = rel(1), face = "bold"),
        legend.position = "bottom",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "gray90"),
        axis.title = element_text(face = "bold", size = 13))

#width distribution 
ggplot(df, aes(x=log(width))) + theme_bw() + geom_density(aes(y = ..scaled..), alpha = 10, col = "red", size = 1) + geom_density(aes(y = ..scaled.., color = category), alpha = 0.1) + labs(x = "width") + ggtitle("Shape (W:L) distribution for Film, Fragment, Line (log scaled)")+ theme_bw() +
  theme(plot.title=element_text(size = rel(1), face = "bold"),
        legend.position = "bottom",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "gray90"),
        axis.title = element_text(face = "bold", size = 13)) 

#width distribution for microplastics only (to compare with K&K)
ggplot(df_plot5cat, aes(x=log(width))) + theme_bw() + geom_density(aes(y = ..density..), alpha = 10, col = "red", size = 1) + geom_density(aes(color = category), alpha = 0.1) + labs(x = "log (L)") + ggtitle("Width distribution for Film, Fragment, Line < 5mm")+ theme_bw() +
  theme(plot.title=element_text(size = rel(1), face = "bold"),
        legend.position = "bottom",
        legend.background = element_rect(colour = "gray"),
        legend.key = element_rect(fill = "gray90"),
        axis.title = element_text(face = "bold", size = 13))



