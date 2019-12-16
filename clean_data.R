df <- read.csv("data file Cas.csv", stringsAsFactors = FALSE, colClasses = "character")
df <- df[, !grepl("^CSF", names(df))]
missingness <- rowSums(is.na(df))
table(missingness)
df <- df[missingness < 14, ]

missingness <- colSums(is.na(df))
table(missingness)

df <- df[, !missingness > 5790]

illegal_values <- function(x){
  suppressWarnings(
    table(x[!is.na(x) & is.na(as.numeric(x))])  
  )
}

df[c("S", "N", "L..mm.", "W..mm.")] <- lapply(df[c("S", "N", "L..mm.", "W..mm.")], as.numeric)

# Reorder L and W
all(df$L..mm. >= df$W..mm.)
df[,c("L..mm.", "W..mm.")] <- t(apply(df[,c("L..mm.", "W..mm.")], 1, function(x){c(max(x), min(x))}))
all(df$L..mm. >= df$W..mm.)

illegal_values(df$A..mm.)
df$A..mm.[df$A..mm. %in% c("#VALUE!", "na")] <- NA
df$A..mm. <- as.numeric(df$A..mm.)

illegal_values(df$H..mm.)
df$H..mm.[df$H..mm. %in% c("na", "foutje")] <- NA
df$H..mm. <- as.numeric(df$H..mm.)

illegal_values(df$D)
df$D[df$D %in% c("na")] <- NA
df$D <- as.numeric(df$D)

# Remove imputed H..5mm. values
has_h <- !is.na(df$H..5mm.) & !df$H..5mm. == ""
h <- df$H..5mm.[has_h]
num_decimals <- nchar(gsub("^.+\\.", "", h))
df$H..5mm.[has_h][num_decimals > 1] <- NA

df$H..5mm. <- as.numeric(df$H..5mm.)

re_categorize <- function(x, categories){
  x <- as.character(x)
  out <- x
  for(i in names(categories)){
    out[x %in% categories[[i]]] <- i
  }
  out[!out %in% c(NA, names(categories), unlist(categories))] <- "other"
  out
}

df$PT <- factor(df$PT)
PT_cat <- list(
  PP = grep("PP", levels(df$PT), value = TRUE),
  PE = grep("^*?PE\\s{0,}$", levels(df$PT), value = TRUE)
)
df$poly_type <- re_categorize(df$PT, PT_cat)
table(df$PT, df$poly_type)

df$cat <- factor(df$category)
Cat_cat <- list(
  Film = grep("ilm", levels(df$cat), value = TRUE),
  Foam = grep("foam", levels(df$cat), value = TRUE),
  Fragment = grep("fragment", levels(df$cat), value = TRUE),
  Line = grep("line", levels(df$cat), value = TRUE),
  Pellet = grep("pellet", levels(df$cat), value = TRUE)
)
df$category <- re_categorize(df$cat, Cat_cat)
table(df$cat, df$category)

names(df)[match(c("S", "L..mm.", "W..mm.", "H..mm.", "H..5mm."), names(df))] <- c("sample", "length", "width", "height_est", "height_obs")

df <- df[, c("current", "sample", "length", "width", "height_est", "height_obs", "category", "poly_type")]

# Imputed height correlates poorly with actual height, even for the fragments
sapply(unique(df$category), function(x){
  #x <- "Fragment"
  tmp <- df[df$category == x, c("height_est", "height_obs")]
  c(x, sum(complete.cases(tmp)), cor(tmp$height_est, tmp$height_obs, use = "pairwise.complete.obs"))
})

tmp <- df[!is.na(df$height_obs) & df$category == "Fragment", ]
cor(tmp[, c("length", "width", "height_obs")])

pc <- principal(cor(tmp[, c("length", "width", "height_obs")]))
pc$values
# And for Fragment, LxWxH carry very similar information

table(df$height_est[df$category %in% c("Film", "Line")], df$category[df$category %in% c("Film", "Line")])

df$Two_dim <- as.numeric(df$category %in% c("Film", "Line"))
df$Film <- as.numeric(df$category == "Film")
df$Line <- as.numeric(df$category == "Line")

# Maybe do something with outliers:
check_vars <- c("length", "width")
mah <- mahalanobis(df[, check_vars], colMeans(df[, check_vars], na.rm = TRUE), cov(df[, check_vars]))
plot(density(mah))
sum(mah > 14)
df[order(mah, decreasing = TRUE)[1:10], ]
df[order(mah, decreasing = TRUE)[11:20], ]

rm(Cat_cat, pc, PT_cat, tmp, missingness, re_categorize, mah, check_vars, h, has_h, num_decimals, illegal_values)