stats_Wline <- read.csv("Results line W hand meas.csv", stringsAsFactors = FALSE, colClasses = "character")

stats_Wline$Length <- as.numeric(stats_Wline$Length)
summary(stats_Wline$Length)

install.packages("pastecs")
library("pastecs")
read.csv("df1.csv", stringsAsFactors = FALSE)
stat.desc(stats_Wline$Length)

stats_Wfilm <- read.csv("Results H film measurement.csv", stringsAsFactors = FALSE, colClasses = "character")
stats_Wfilm$Length <- as.numeric(stats_Wfilm$Length)
stat.desc(stats_Wfilm$Length).

#stats voor length & width
gsummary(df1$length)
stat.desc(df1$width)

#total poly type
df1 <- read.csv("df1.csv")
install.packages("dplyr")
library(dplyr)
df1 %>%
  select(poly_type, length) %>%
  group_by(poly_type) %>%
  summarise(N = n())

table(df1$poly_type,df1$category, useNA = "always")

install.packages('epiDisplay')
library(epiDisplay)
tab1(df1$category, sort.group = "decreasing", cum.percent = TRUE)

nrow(df1[df1$length <= 5, ])
nrow(df1[df1$length >5, ])
nrow(df1)

#frequencies & relative frequency crosstable
install.packages("gmodels")
library(gmodels)
crostab <- CrossTable(df1$poly_type, df1$category, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
print(crostab)


